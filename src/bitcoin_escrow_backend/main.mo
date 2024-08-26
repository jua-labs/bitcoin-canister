import Text "mo:base/Text";
import Map "mo:base/HashMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import Int "mo:base/Int";
import Time "mo:base/Time";
import Float "mo:base/Float";
import Nat64 "mo:base/Nat64";
import Debug "mo:base/Debug";
import Error "mo:base/Error";
import ExperimentalCycles "mo:base/ExperimentalCycles";

import Types "types";
import BitcoinApi "bitcoinapi";
import BitcoinWallet "bitcoinwallet";

actor class EscrowBitcoin(initialAdminPrincipal : Principal) {
  type Network = Types.Network;
  type BitcoinAddress = Types.BitcoinAddress;
  type Satoshi = Types.Satoshi;
  type MillisatoshiPerVByte = Types.MillisatoshiPerVByte;
  type GetUtxosResponse = Types.GetUtxosResponse;

  var NETWORK : Network = #testnet; // Default to testnet
  let GET_BALANCE_COST_CYCLES : Nat = 100_000_000;
  var KEY_NAME : Text = "test_key_1";
  let DERIVATION_PATH : [[Nat8]] = [];

  type ManagementCanisterActor = actor {
    bitcoin_get_balance : {
      address : BitcoinAddress;
      network : Network;
      min_confirmations : ?Nat32;
    } -> async Satoshi;
    bitcoin_get_current_fee_percentiles : { network : Network } -> async [MillisatoshiPerVByte];
  };

  private var managementCanisterActor : ?ManagementCanisterActor = null;

  private var adminPrincipal : Principal = initialAdminPrincipal;

  // Escrow state
  private let escrows = Map.HashMap<Text, EscrowData>(0, Text.equal, Text.hash);

  type EscrowStatus = {
    #created;
    #fundsPending;
    #fundsReceived;
    #completed;
    #cancelled;
    #refunded;
  };

  type EscrowData = {
    id : Text;
    seller : Principal;
    buyer : Principal;
    amount : Satoshi;
    status : EscrowStatus;
    sellerAddress : BitcoinAddress;
    buyerAddress : BitcoinAddress;
    escrowAddress : BitcoinAddress;
    transactionId : ?Text;
    creationTime : Int;
    lastUpdateTime : Int;
    completionTime : ?Int;
    cancellationTime : ?Int;
    refundTime : ?Int;
    description : Text;
    network : Network;
  };

  // Function to change the network
  public shared (msg) func changeNetwork(newNetwork : Network) : async Result.Result<(), Text> {
    Debug.print("Caller principal: " # Principal.toText(msg.caller));
    Debug.print("Admin principal: " # Principal.toText(adminPrincipal));

    // if (Principal.equal(msg.caller, adminPrincipal) == false) {
    //   return #err("Only the admin can change the network. Caller: " # Principal.toText(msg.caller) # ", Admin: " # Principal.toText(adminPrincipal));
    // };

    NETWORK := newNetwork;

    // Update KEY_NAME based on the new network
    KEY_NAME := switch (NETWORK) {
      case (#regtest) "dfx_test_key";
      case _ "test_key_1";
    };

    #ok();
  };

  // Function to update the admin principal
  public shared (msg) func updateAdminPrincipal(newAdminPrincipal : Principal) : async Result.Result<(), Text> {
    Debug.print("Current admin principal: " # Principal.toText(adminPrincipal));
    Debug.print("Caller principal: " # Principal.toText(msg.caller));
    Debug.print("Proposed new admin principal: " # Principal.toText(newAdminPrincipal));

    if (Principal.equal(msg.caller, adminPrincipal) == false) {
      return #err("Only the current admin can update the admin principal. Caller: " # Principal.toText(msg.caller) # ", Admin: " # Principal.toText(adminPrincipal));
    };

    adminPrincipal := newAdminPrincipal;
    #ok();
  };

  // Function to get the current admin principal
  public query func getAdminPrincipal() : async Principal {
    adminPrincipal;
  };

  // Function to get the current network
  public query func getNetwork() : async Network {
    NETWORK;
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

  // Utility function to convert Bitcoin to Satoshis
  private func bitcoinToSatoshis(btc : Float) : Satoshi {
    let satoshisFloat : Float = btc * 1e8;
    let satoshisInt : Int = Float.toInt(satoshisFloat);

    if (satoshisInt < 0) {
      Debug.print("Warning: Negative Bitcoin amount converted to 0 Satoshis");
      return 0;
    };

    let satoshisNat : Nat = Int.abs(satoshisInt);
    Nat64.fromNat(satoshisNat);
  };

  // Utility function to convert Satoshis to Bitcoin
  private func satoshisToBitcoin(satoshis : Satoshi) : Float {
    Float.fromInt(Nat64.toNat(satoshis)) / 1e8;
  };

  public func get_balance(address : BitcoinAddress) : async {
    total : Float;
    available : Float;
  } {
    let isConnected = await checkBitcoinConnection();
    if (not isConnected) {
      Debug.print("Cannot get balance: Not connected to Bitcoin network");
      throw Error.reject("Not connected to Bitcoin network");
    };

    ExperimentalCycles.add(GET_BALANCE_COST_CYCLES);

    let management_canister_actor = getManagementCanisterActor();

    try {
      let resultInSatoshis = await management_canister_actor.bitcoin_get_balance({
        address;
        network = NETWORK;
        min_confirmations = null;
      });

      // Convert Satoshis to Bitcoin
      let totalBalanceInBitcoin : Float = satoshisToBitcoin(resultInSatoshis);

      // Calculate the amount in escrow for this address
      let amountInEscrow = getAmountInEscrow(address);

      // Calculate available balance
      let availableBalanceInBitcoin : Float = totalBalanceInBitcoin - amountInEscrow;

      {
        total = totalBalanceInBitcoin;
        available = Float.max(0, availableBalanceInBitcoin);
      };
    } catch (error) {
      Debug.print("BitcoinApi: Error calling management canister: " # Error.message(error));
      throw error;
    };
  };

  // Helper function to calculate the amount in escrow for a given address
  private func getAmountInEscrow(address : BitcoinAddress) : Float {
    var totalInEscrow : Float = 0;
    for ((_, escrow) in escrows.entries()) {
      if (escrow.sellerAddress == address and escrow.network == NETWORK and (escrow.status == #created or escrow.status == #fundsPending)) {
        totalInEscrow += satoshisToBitcoin(escrow.amount);
      };
    };
    totalInEscrow;
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

  // Create a new escrow
  public shared (msg) func createEscrow(
    buyer : Principal,
    amount : Satoshi,
    sellerAddress : BitcoinAddress,
    buyerAddress : BitcoinAddress,
    escrowAddress : BitcoinAddress,
    description : Text,
  ) : async Result.Result<Text, Text> {
    let escrowId = generateUniqueId();
    let currentTime = Time.now();

    let newEscrow : EscrowData = {
      id = escrowId;
      seller = msg.caller;
      buyer = buyer;
      amount = amount;
      status = #created;
      sellerAddress = sellerAddress;
      buyerAddress = buyerAddress;
      escrowAddress = escrowAddress;
      transactionId = null;
      creationTime = currentTime;
      lastUpdateTime = currentTime;
      completionTime = null;
      cancellationTime = null;
      refundTime = null;
      description = description;
      network = NETWORK;
    };

    escrows.put(escrowId, newEscrow);
    #ok(escrowId);
  };

  // Update escrow status when funds are sent (called by backend)
  public func updateEscrowFundsSent(escrowId : Text, transactionId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) {
        if (escrow.status != #created) {
          #err("Invalid escrow status for updating to funds sent");
        } else if (escrow.network != NETWORK) {
          #err("Escrow network does not match current network");
        } else {
          let updatedEscrow = {
            escrow with
            status = #fundsPending;
            transactionId = ?transactionId;
            lastUpdateTime = Time.now();
          };
          escrows.put(escrowId, updatedEscrow);
          #ok();
        };
      };
    };
  };

  // Update escrow status when funds are received (called by backend)
  public func updateEscrowFundsReceived(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) {
        if (escrow.status != #fundsPending) {
          #err("Invalid escrow status for updating to funds received");
        } else if (escrow.network != NETWORK) {
          #err("Escrow network does not match current network");
        } else {
          let updatedEscrow = {
            escrow with
            status = #fundsReceived;
            lastUpdateTime = Time.now();
          };
          escrows.put(escrowId, updatedEscrow);
          #ok();
        };
      };
    };
  };

  // Complete escrow (release funds to buyer)
  public shared (msg) func completeEscrow(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) {
        if (escrow.status != #fundsReceived) {
          #err("Invalid escrow status for completion");
        } else if (msg.caller != escrow.seller) {
          #err("Only the seller can complete the escrow");
        } else if (escrow.network != NETWORK) {
          #err("Escrow network does not match current network");
        } else {
          let updatedEscrow = {
            escrow with
            status = #completed;
            lastUpdateTime = Time.now();
            completionTime = ?Time.now();
          };
          escrows.put(escrowId, updatedEscrow);
          #ok();
        };
      };
    };
  };

  // Cancel escrow (only if funds haven't been sent)
  public shared (msg) func cancelEscrow(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) {
        if (escrow.status != #created) {
          #err("Cannot cancel escrow: funds already sent");
        } else if (msg.caller != escrow.seller and msg.caller != escrow.buyer) {
          #err("Only the seller or buyer can cancel the escrow");
        } else if (escrow.network != NETWORK) {
          #err("Escrow network does not match current network");
        } else {
          let updatedEscrow = {
            escrow with
            status = #cancelled;
            lastUpdateTime = Time.now();
            cancellationTime = ?Time.now();
          };
          escrows.put(escrowId, updatedEscrow);
          #ok();
        };
      };
    };
  };

  // Refund escrow (only if funds have been received but not completed)
  public shared (msg) func refundEscrow(escrowId : Text) : async Result.Result<(), Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) {
        if (escrow.status != #fundsReceived) {
          #err("Invalid escrow status for refund");
        } else if (msg.caller != escrow.seller) {
          #err("Only the seller can refund the escrow");
        } else if (escrow.network != NETWORK) {
          #err("Escrow network does not match current network");
        } else {
          let updatedEscrow = {
            escrow with
            status = #refunded;
            lastUpdateTime = Time.now();
            refundTime = ?Time.now();
          };
          escrows.put(escrowId, updatedEscrow);
          #ok();
        };
      };
    };
  };

  // Get escrow details
  public query func getEscrowDetails(escrowId : Text) : async Result.Result<EscrowData, Text> {
    switch (escrows.get(escrowId)) {
      case (null) { #err("Escrow not found") };
      case (?escrow) { #ok(escrow) };
    };
  };

  // Get all escrows for the current network
  public query func getAllEscrows() : async [EscrowData] {
    Iter.toArray(Iter.filter(escrows.vals(), func(escrow : EscrowData) : Bool { escrow.network == NETWORK }));
  };

  // Get escrows by user (either as seller or buyer) for the current network
  public query func getEscrowsByUser(user : Principal) : async [EscrowData] {
    Iter.toArray(
      Iter.filter(
        escrows.vals(),
        func(escrow : EscrowData) : Bool {
          (escrow.seller == user or escrow.buyer == user) and escrow.network == NETWORK;
        },
      )
    );
  };

  // Get total value of all escrows for a user on the current network
  public query func getTotalEscrowValue(user : Principal) : async Satoshi {
    var totalValue : Satoshi = 0;
    for (escrow in escrows.vals()) {
      if ((escrow.seller == user or escrow.buyer == user) and escrow.network == NETWORK) {
        totalValue += escrow.amount;
      };
    };
    totalValue;
  };

  // ... [previous code remains unchanged]

  // Get escrow statistics for the current network
  public query func getEscrowStatistics() : async {
    totalEscrows : Nat;
    totalValueLocked : Satoshi;
    completedEscrows : Nat;
    cancelledEscrows : Nat;
    refundedEscrows : Nat;
  } {
    var totalEscrows = 0;
    var totalValueLocked : Satoshi = 0;
    var completedEscrows = 0;
    var cancelledEscrows = 0;
    var refundedEscrows = 0;

    for (escrow in escrows.vals()) {
      if (escrow.network == NETWORK) {
        totalEscrows += 1;
        switch (escrow.status) {
          case (#created or #fundsPending or #fundsReceived) {
            totalValueLocked += escrow.amount;
          };
          case (#completed) { completedEscrows += 1 };
          case (#cancelled) { cancelledEscrows += 1 };
          case (#refunded) { refundedEscrows += 1 };
        };
      };
    };

    {
      totalEscrows = totalEscrows;
      totalValueLocked = totalValueLocked;
      completedEscrows = completedEscrows;
      cancelledEscrows = cancelledEscrows;
      refundedEscrows = refundedEscrows;
    };
  };

  // Helper function to generate a unique ID
  private func generateUniqueId() : Text {
    let timestamp = Int.abs(Time.now());
    let randomPart = Int.abs(Time.now()) % 10000;
    Int.toText(timestamp) # "-" # Int.toText(randomPart);
  };

  // Add a function to estimate transaction fee (placeholder implementation)
  public func estimateTransactionFee(amount : Satoshi) : async Result.Result<Satoshi, Text> {
    try {
      let feePercentiles = await get_current_fee_percentiles();
      if (feePercentiles.size() == 0) {
        return #err("Unable to fetch fee estimates");
      };

      // Use the median fee (adjust as needed)
      let medianFeeRate = feePercentiles[feePercentiles.size() / 2];

      // Estimate transaction size (this is a simplified estimate)
      let estimatedTxSize : Nat64 = 250; // bytes

      let estimatedFeeInMillisatoshis : Nat64 = medianFeeRate * estimatedTxSize / 1000;
      let estimatedFeeInSatoshis : Satoshi = estimatedFeeInMillisatoshis / 1000;

      #ok(estimatedFeeInSatoshis);
    } catch (error) {
      #err("Error estimating fee: " # Error.message(error));
    };
  };

  // Add a function to get transaction details (placeholder implementation)
  public func getTransactionDetails(txId : Text) : async Result.Result<{ confirmations : Nat; amount : Satoshi }, Text> {
    // This is a placeholder. In a real implementation, you would fetch the transaction details from the Bitcoin network.
    // For demonstration purposes, we're returning mock data.
    #ok({
      confirmations = 3;
      amount = 50_000_000; // 0.5 BTC in Satoshis
    });
  };

  // Add a system health check function
  public func healthCheck() : async {
    bitcoin_network_connected : Bool;
    escrow_count : Nat;
    canister_balance : Nat;
    current_network : Network;
  } {
    let bitcoinConnected = await checkBitcoinConnection();
    let escrowCount = Iter.size(Iter.filter(escrows.vals(), func(escrow : EscrowData) : Bool { escrow.network == NETWORK }));
    let canisterBalance = ExperimentalCycles.balance();

    {
      bitcoin_network_connected = bitcoinConnected;
      escrow_count = escrowCount;
      canister_balance = canisterBalance;
      current_network = NETWORK;
    };
  };

  // Function to get escrows by status for the current network
  public query func getEscrowsByStatus(status : EscrowStatus) : async [EscrowData] {
    Iter.toArray(
      Iter.filter(
        escrows.vals(),
        func(escrow : EscrowData) : Bool {
          escrow.status == status and escrow.network == NETWORK;
        },
      )
    );
  };

  // Function to get the total number of escrows for the current network
  public query func getTotalEscrowCount() : async Nat {
    Iter.size(Iter.filter(escrows.vals(), func(escrow : EscrowData) : Bool { escrow.network == NETWORK }));
  };

  // Function to clear all completed, cancelled, and refunded escrows older than a certain time
  public func clearOldEscrows(olderThanDays : Nat) : async Nat {
    let currentTime = Time.now();
    let threshold = currentTime - (olderThanDays * 24 * 60 * 60 * 1000000000);
    var removedCount = 0;

    let escrowsToRemove = Iter.toArray(
      Iter.filter(
        escrows.entries(),
        func((_, escrow) : (Text, EscrowData)) : Bool {
          switch (escrow.status) {
            case (#completed or #cancelled or #refunded) {
              escrow.lastUpdateTime < threshold;
            };
            case (_) { false };
          };
        },
      )
    );

    for ((id, _) in escrowsToRemove.vals()) {
      switch (escrows.remove(id)) {
        case null { /* Entry was already removed */ };
        case (?_) { removedCount += 1 };
      };
    };

    removedCount;
  };
};
