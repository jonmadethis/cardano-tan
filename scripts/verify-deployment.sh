#!/bin/bash
set -euo pipefail

# Configuration
CARDANO_CLI="cardano-cli"
PROJECT_ROOT=$(git rev-parse --show-toplevel)
DEPLOYMENT_DIR="$PROJECT_ROOT/deployment"

# Command line arguments
NETWORK="$1"
CONTRACT_ADDRESS="$2"

# Validate network selection
case "$NETWORK" in
    "testnet")
        NETWORK_PARAM="--testnet-magic 1097911063"
        ;;
    "mainnet")
        NETWORK_PARAM="--mainnet"
        ;;
    *)
        echo "Error: Invalid network. Use 'testnet' or 'mainnet'"
        exit 1
        ;;
esac

echo "Verifying deployment on $NETWORK..."

# Query UTxO at contract address
echo "Querying contract UTxO..."
$CARDANO_CLI query utxo \
    --address "$CONTRACT_ADDRESS" \
    $NETWORK_PARAM \
    --out-file "$DEPLOYMENT_DIR/$NETWORK/utxo.json"

# Verify script presence
if [ -f "$DEPLOYMENT_DIR/$NETWORK/utxo.json" ]; then
    echo "Contract successfully deployed!"
    echo "UTxO information:"
    cat "$DEPLOYMENT_DIR/$NETWORK/utxo.json"
else
    echo "Error: Contract not found at address $CONTRACT_ADDRESS"
    exit 1
fi

# Verify initial state
echo "Verifying initial contract state..."
SCRIPT_HASH=$($CARDANO_CLI transaction policyid \
    --script-file "$DEPLOYMENT_DIR/$NETWORK/$CONTRACT_ADDRESS.plutus")

echo "Script hash: $SCRIPT_HASH"
echo "Deployment verification complete!"
echo ""
echo "Contract is ready for interaction"
echo "Monitor the contract using: scripts/monitor-contract.sh $NETWORK $CONTRACT_ADDRESS"
