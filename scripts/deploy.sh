#!/bin/bash
set -euo pipefail

# Configuration
CARDANO_CLI="cardano-cli"
CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-node.socket}"
PROJECT_ROOT=$(git rev-parse --show-toplevel)
DEPLOYMENT_DIR="$PROJECT_ROOT/deployment"

# Command line arguments
NETWORK="$1"
DEALER_PKH="$2"
MIN_BET="$3"
TIMEOUT="$4"
OUTPUT_FILE="$5"

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

# Setup deployment directory
mkdir -p "$DEPLOYMENT_DIR/$NETWORK"
cd "$DEPLOYMENT_DIR/$NETWORK"

echo "Preparing for deployment on $NETWORK..."

# Generate verification and signing keys if they don't exist
if [ ! -f "payment.vkey" ]; then
    echo "Generating payment key pair..."
    $CARDANO_CLI address key-gen \
        --verification-key-file payment.vkey \
        --signing-key-file payment.skey
fi

# Generate deployment address
if [ ! -f "payment.addr" ]; then
    echo "Generating payment address..."
    $CARDANO_CLI address build \
        --payment-verification-key-file payment.vkey \
        $NETWORK_PARAM \
        --out-file payment.addr
fi

# Query protocol parameters
echo "Querying protocol parameters..."
$CARDANO_CLI query protocol-parameters \
    $NETWORK_PARAM \
    --out-file protocol.json

# Build contract
echo "Building contract..."
cd "$PROJECT_ROOT"
cabal build fan-tan-contract

# Generate Plutus script
echo "Generating Plutus script..."
cabal run fan-tan-deploy -- \
    "$NETWORK" \
    "$DEALER_PKH" \
    "$MIN_BET" \
    "$TIMEOUT" \
    "$DEPLOYMENT_DIR/$NETWORK/$OUTPUT_FILE"

# Calculate minimum ADA requirement
echo "Calculating minimum ADA requirement..."
$CARDANO_CLI transaction calculate-min-required-utxo \
    --protocol-params-file "$DEPLOYMENT_DIR/$NETWORK/protocol.json" \
    --tx-out-datum-embed-file "$DEPLOYMENT_DIR/$NETWORK/game-datum.json" \
    --tx-out="$OUTPUT_FILE+0" \
    $NETWORK_PARAM

echo "Deployment preparation complete!"
echo "Contract address: $(cat "$DEPLOYMENT_DIR/$NETWORK/$OUTPUT_FILE.addr")"
echo "Minimum ADA requirement calculated and saved"
echo ""
echo "Next steps:"
echo "1. Fund the deployment address with required ADA"
echo "2. Run deploy-contract.sh to submit the contract to the blockchain"
echo "3. Verify deployment using verify-deployment.sh"
