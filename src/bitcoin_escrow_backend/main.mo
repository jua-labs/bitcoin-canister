import Text "mo:base/Text";
import Map "mo:base/HashMap";
import Principal "mo:base/Principal";
import Error "mo:base/Error";
import Debug "mo:base/Debug";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Array "mo:base/Array";
import Iter "mo:base/Iter";

import BitcoinApi "bitcoinapi";
import BitcoinWallet "bitcoinwallet";
import Types "types";
import Utils "utils";

actor class EscrowBitcoin(_network : Types.Network) {
  type GetUtxosResponse = Types.GetUtxosResponse;
  type MillisatoshiPerVByte = Types.MillisatoshiPerVByte;
  type SendRequest = Types.SendRequest;
  type Network = Types.Network;
  type BitcoinAddress = Types.BitcoinAddress;
  type Satoshi = Types.Satoshi;
  type OutPoint = Types.OutPoint;

  stable let NETWORK : Network = _network;
  let DERIVATION_PATH : [[Nat8]] = [];
  let KEY_NAME : Text = switch NETWORK {
    case (#regtest) "dfx_test_key";
    case _ "test_key_1";
  };

  let GET_BALANCE_COST_CYCLES : Nat = 100_000_000;

  // Escrow state
  private let escrows = Map.HashMap<Text, EscrowData>(0, Text.equal, Text.hash);

  type ManagementCanisterActor = actor {
    bitcoin_get_balance : {
      address : BitcoinAddress;
      network : Network;
      min_confirmations : ?Nat32;
    } -> async Satoshi;
    bitcoin_get_utxos : {
      address : BitcoinAddress;
      network : Network;
      filter : ?{ min_confirmations : ?Nat32 };
    } -> async GetUtxosResponse;
    bitcoin_get_current_fee_percentiles : { network : Network } -> async [MillisatoshiPerVByte];
    bitcoin_send_transaction : { transaction : [Nat8]; network : Network } -> async ();
  };

  private var managementCanisterActor : ?ManagementCanisterActor = null;

  type EscrowStatus = {
    #created;
    #pendingPayment;
    #paid;
    #completed;
    #cancelled;
  };

  type EscrowData = {
    seller : Principal;
    buyer : Principal;
    amount : Satoshi;
    status : EscrowStatus;
    sellerAddress : BitcoinAddress;
    buyerAddress : BitcoinAddress;
    transactionId : ?Text;
    sellerPrivateKey : Text;
  };

  type EscrowSummary = {
    id : Text;
    seller : Principal;
    buyer : Principal;
    amount : Satoshi;
    status : EscrowStatus;
  };

  public func checkBitcoinConnection() : async Bool {
    Debug.print("Checking Bitcoin network connection...");
    try {
      let fees = await get_current_fee_percentiles();
      Debug.print("Successfully connected to Bitcoin network. Current fee percentiles: " # debug_show (fees));
      true;
    } catch (error) {
      Debug.print("Failed to connect to Bitcoin network: " # Error.message(error));
      false;
    };
  };

  // Create a new escrow after checking seller's balance
  public shared (msg) func createEscrow(seller : Principal, buyer : Principal, amount : Satoshi, sellerAddress : BitcoinAddress, buyerAddress : BitcoinAddress, sellerPrivateKey : Text) : async Result.Result<Text, Text> {
    Debug.print("Attempting to create escrow...");

    let isConnected = await checkBitcoinConnection();
    if (not isConnected) {
      Debug.print("Cannot create escrow: Not connected to Bitcoin network");
      return #err("Not connected to Bitcoin network");
    };
    
    // Check seller's balance
    let sellerBalance = await get_balance(NETWORK, sellerAddress);
    if (sellerBalance < amount) {
      Debug.print("Insufficient funds in seller's address");
      return #err("Insufficient funds in seller's address");
    };

    // Check if seller's balance is already allocated to existing escrows
    let allocatedBalance = getAllocatedBalance(seller);
    if (sellerBalance - allocatedBalance < amount) {
      Debug.print("Seller's available balance is insufficient due to existing escrows");
      return #err("Insufficient available balance due to existing escrows");
    };

    let escrowId = Utils.generateUniqueId();

    escrows.put(
      escrowId,
      {
        seller = seller;
        buyer = buyer;
        amount = amount;
        status = #created;
        sellerAddress = sellerAddress;
        buyerAddress = buyerAddress;
        transactionId = null;
        sellerPrivateKey = sellerPrivateKey;
      },
    );

    #ok(escrowId);
  };

  // Helper function to get total allocated balance for a seller
  private func getAllocatedBalance(seller : Principal) : Satoshi {
    var total : Satoshi = 0;
    for ((_, escrow) in escrows.entries()) {
      if (escrow.seller == seller and escrow.status != #completed and escrow.status != #cancelled) {
        total += escrow.amount;
      };
    };
    total;
  };

  // Initiate payment and mark escrow as pending payment
  public shared (msg) func initiatePayment(escrowId : Text) : async Result.Result<Text, Text> {
    switch (escrows.get(escrowId)) {
      case (null) {
        #err("Escrow not found");
      };
      case (?escrow) {
        switch (escrow.status) {
          case (#created) {
            // Initiate the Bitcoin transaction
            try {
              let txId = await sendFromSellerToBuyer(escrow.sellerAddress, escrow.buyerAddress, escrow.amount, escrow.sellerPrivateKey);
              
              if (txId != "") {
                // Update escrow status and store the transaction ID
                escrows.put(
                  escrowId,
                  {
                    seller = escrow.seller;
                    buyer = escrow.buyer;
                    amount = escrow.amount;
                    status = #pendingPayment;
                    sellerAddress = escrow.sellerAddress;
                    buyerAddress = escrow.buyerAddress;
                    transactionId = ?txId;
                    sellerPrivateKey = escrow.sellerPrivateKey;
                  },
                );
                #ok(txId);
              } else {
                #err("Failed to initiate the transaction: Empty transaction ID returned");
              };
            } catch (error) {
              #err("Failed to initiate the transaction: " # Error.message(error));
            };
          };
          case (#pendingPayment) {
            #err("Payment already initiated for this escrow");
          };
          case (#paid) {
            #err("Escrow is already paid");
          };
          case (#completed) {
            #err("Escrow is already completed");
          };
          case (#cancelled) {
            #err("Escrow is cancelled");
          };
        };
      };
    };
  };

  // Check transaction status and update escrow
  public shared (msg) func checkPaymentStatus(escrowId : Text) : async Result.Result<EscrowStatus, Text> {
    switch (escrows.get(escrowId)) {
      case (null) {
        #err("Escrow not found");
      };
      case (?escrow) {
        switch (escrow.status) {
          case (#created) {
            #ok(#created);
          };
          case (#pendingPayment) {
            switch (escrow.transactionId) {
              case (?txId) {
                let confirmations = await getTransactionConfirmations(txId);
                if (confirmations >= 1) {  // You can adjust this threshold as needed
                  // Update escrow status to paid
                  let updatedEscrow = {
                    seller = escrow.seller;
                    buyer = escrow.buyer;
                    amount = escrow.amount;
                    status = #paid;
                    sellerAddress = escrow.sellerAddress;
                    buyerAddress = escrow.buyerAddress;
                    transactionId = escrow.transactionId;
                    sellerPrivateKey = escrow.sellerPrivateKey;
                  };
                  escrows.put(escrowId, updatedEscrow);
                  #ok(#paid);
                } else {
                  #ok(#pendingPayment);
                };
              };
              case (null) {
                #err("Transaction ID is missing for pending payment");
              };
            };
          };
          case (#paid) {
            #ok(#paid);
          };
          case (#completed) {
            #ok(#completed);
          };
          case (#cancelled) {
            #ok(#cancelled);
          };
        };
      };
    };
  };

  // Helper function to get transaction confirmations
  private func getTransactionConfirmations(txId : Text) : async Nat {
    // This is a placeholder. You would need to implement this function
    // to check the number of confirmations for a Bitcoin transaction.
    // This might involve calling a Bitcoin node or using a third-party API.
    5  // Placeholder return value
  };

  // Private function to send from seller to buyer
  private func sendFromSellerToBuyer(sellerAddress : BitcoinAddress, buyerAddress : BitcoinAddress, amount : Satoshi, sellerPrivateKey : Text) : async Text {
    let request : SendRequest = {
      destination_address = buyerAddress;
      amount_in_satoshi = amount;
    };
    Utils.bytesToText(await BitcoinWallet.send(NETWORK, DERIVATION_PATH, sellerPrivateKey, buyerAddress, amount));
  };

  // Mark escrow as completed
  public shared (msg) func completeEscrow(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) {
        #err("Escrow not found");
      };
      case (?escrow) {
        if (escrow.status != #paid) {
          #err("Cannot complete escrow: payment not confirmed");
        } else {
          escrows.put(
            escrowId,
            {
              seller = escrow.seller;
              buyer = escrow.buyer;
              amount = escrow.amount;
              status = #completed;
              sellerAddress = escrow.sellerAddress;
              buyerAddress = escrow.buyerAddress;
              transactionId = escrow.transactionId;
              sellerPrivateKey = escrow.sellerPrivateKey;
            },
          );
          #ok();
        };
      };
    };
  };

  // Cancel escrow (only if it's not paid yet)
  public shared (msg) func cancelEscrow(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) {
        #err("Escrow not found");
      };
      case (?escrow) {
        if (escrow.status != #created) {
          #err("Cannot cancel escrow: payment already initiated");
        } else {
          escrows.put(
            escrowId,
            {
              seller = escrow.seller;
              buyer = escrow.buyer;
              amount = escrow.amount;
              status = #cancelled;
              sellerAddress = escrow.sellerAddress;
              buyerAddress = escrow.buyerAddress;
              transactionId = escrow.transactionId;
              sellerPrivateKey = escrow.sellerPrivateKey;
            },
          );
          #ok();
        };
      };
    };
  };

  // Get escrow details
  public query func getEscrowDetails(escrowId : Text) : async ?EscrowData {
    escrows.get(escrowId);
  };

  // Get all escrows
  public func getAllEscrows() : async [EscrowSummary] {
    let escrowEntries = escrows.entries();
    let escrowSummaries = Array.map<(Text, EscrowData), EscrowSummary>(
      Iter.toArray(escrowEntries),
      func ((id, escrow) : (Text, EscrowData)) : EscrowSummary {
        {
          id = id;
          seller = escrow.seller;
          buyer = escrow.buyer;
          amount = escrow.amount;
          status = escrow.status;
        }
      }
    );
    escrowSummaries
  };

  // Get escrows by user (either as seller or buyer)
  public func getEscrowsByUser(user : Principal) : async [EscrowSummary] {
    let allEscrows = await getAllEscrows();
    Array.filter<EscrowSummary>(
      allEscrows,
      func (escrow : EscrowSummary) : Bool {
        escrow.seller == user or escrow.buyer == user
      }
    )
  };

  // Helper functions for Bitcoin operations
  public func get_balance(network : Network, address : BitcoinAddress) : async Satoshi {
    let isConnected = await checkBitcoinConnection();
    if (not isConnected) {
      Debug.print("Cannot get balance: Not connected to Bitcoin network");
    };

    ExperimentalCycles.add(GET_BALANCE_COST_CYCLES);

    let management_canister_actor = getManagementCanisterActor();

    try {
      let result = await management_canister_actor.bitcoin_get_balance({
        address;
        network;
        min_confirmations = null;
      });
      result;
    } catch (error) {
      Debug.print("BitcoinApi: Error calling management canister: " # Error.message(error));
      throw error;
    };
  };

  private func getManagementCanisterActor() : ManagementCanisterActor {
    switch (managementCanisterActor) {
      case (null) {
        let newActor : ManagementCanisterActor = actor ("aaaaa-aa");
        managementCanisterActor := ?newActor;
        newActor;
      };
      case (?existingActor) { existingActor };
    };
  };

  public func get_utxos(address : BitcoinAddress) : async GetUtxosResponse {
    await BitcoinApi.get_utxos(NETWORK, address);
  };

  public func get_current_fee_percentiles() : async [MillisatoshiPerVByte] {
    await BitcoinApi.get_current_fee_percentiles(NETWORK);
  };

  public func get_p2pkh_address() : async BitcoinAddress {
    await BitcoinWallet.get_p2pkh_address(NETWORK, KEY_NAME, DERIVATION_PATH);
  };
};