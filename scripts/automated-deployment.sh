#!/bin/bash
set -euo pipefail

# Configuration and environment setup
CARDANO_CLI="cardano-cli"
CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-node.socket}"
PROJECT_ROOT=$(git rev-parse --show-toplevel)
DEPLOYMENT_DIR="$PROJECT_ROOT/deployment"
SCRIPTS_DIR="$PROJECT_ROOT/scripts"
LOG_DIR="$PROJECT_ROOT/logs"

# Load environment variables
if [[ -f "$PROJECT_ROOT/.env" ]]; then
    source "$PROJECT_ROOT/.env"
fi

# Verify required environment variables
required_vars=("DEALER_PKH" "MIN_BET" "TIMEOUT" "NETWORK")
for var in "${required_vars[@]}"; do
    if [[ -z "${!var:-}" ]]; then
        echo "Error: Required environment variable $var is not set"
        exit 1
    fi
done

# Setup logging
mkdir -p "$LOG_DIR"
DEPLOYMENT_LOG="$LOG_DIR/deployment_$(date +%Y%m%d_%H%M%S).log"
exec 1> >(tee -a "$DEPLOYMENT_LOG")
exec 2>&1

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Pre-deployment verification
verify_contract() {
    log "Starting contract verification"
    
    # Build and test contract
    log "Building contract..."
    cabal build fan-tan-contract || return 1
    
    log "Running test suite..."
    cabal test all || return 1
    
    # Verify script size
    log "Checking script size..."
    script_size=$(wc -c < "$DEPLOYMENT_DIR/$NETWORK/fan-tan.plutus")
    if [[ $script_size -gt 16384 ]]; then
        log "Warning: Script size ($script_size bytes) exceeds recommended limit"
        return 1
    fi
    
    log "Contract verification completed successfully"
    return 0
}

# Optimize contract parameters
optimize_contract() {
    log "Optimizing contract parameters"
    
    # Calculate optimal execution units
    "$CARDANO_CLI" transaction calculate-min-required-utxo \
        --protocol-params-file "$DEPLOYMENT_DIR/$NETWORK/protocol.json" \
        --tx-out-datum-embed-file "$DEPLOYMENT_DIR/$NETWORK/game-datum.json" \
        --tx-out="$(cat $DEPLOYMENT_DIR/$NETWORK/fan-tan.addr)+0" \
        --$NETWORK > "$DEPLOYMENT_DIR/$NETWORK/min-utxo.json"
    
    # Update protocol parameters based on optimization
    jq '.maxTxSize = 16384 | .maxValSize = 5000' \
        "$DEPLOYMENT_DIR/$NETWORK/protocol.json" > "$DEPLOYMENT_DIR/$NETWORK/protocol-optimized.json"
    
    log "Contract optimization completed"
}

# Deploy contract
deploy_contract() {
    log "Starting contract deployment"
    
    # Generate deployment keys if they don't exist
    if [[ ! -f "$DEPLOYMENT_DIR/$NETWORK/payment.skey" ]]; then
        "$CARDANO_CLI" address key-gen \
            --verification-key-file "$DEPLOYMENT_DIR/$NETWORK/payment.vkey" \
            --signing-key-file "$DEPLOYMENT_DIR/$NETWORK/payment.skey"
    fi
    
    # Build and submit deployment transaction
    log "Building deployment transaction..."
    "$SCRIPTS_DIR/deploy.sh" \
        "$NETWORK" \
        "$DEALER_PKH" \
        "$MIN_BET" \
        "$TIMEOUT" \
        "fan-tan" || return 1
    
    # Verify deployment
    log "Verifying deployment..."
    "$SCRIPTS_DIR/verify-deployment.sh" \
        "$NETWORK" \
        "$(cat $DEPLOYMENT_DIR/$NETWORK/fan-tan.addr)" || return 1
    
    log "Contract deployment completed successfully"
    return 0
}

# Monitor deployment
monitor_deployment() {
    log "Starting deployment monitoring"
    
    # Launch monitoring script in background
    "$SCRIPTS_DIR/monitor-contract.sh" \
        "$NETWORK" \
        "$(cat $DEPLOYMENT_DIR/$NETWORK/fan-tan.addr)" \
        60 &
    
    # Store monitoring process ID
    echo $! > "$DEPLOYMENT_DIR/$NETWORK/monitor.pid"
    
    log "Deployment monitoring started"
}

# Main deployment process
main() {
    log "Starting automated deployment process"
    
    # Create deployment directory structure
    mkdir -p "$DEPLOYMENT_DIR/$NETWORK"
    
    # Execute deployment steps
    if ! verify_contract; then
        log "Error: Contract verification failed"
        exit 1
    fi
    
    optimize_contract
    
    if ! deploy_contract; then
        log "Error: Contract deployment failed"
        exit 1
    fi
    
    monitor_deployment
    
    log "Deployment process completed successfully"
    log "Contract address: $(cat $DEPLOYMENT_DIR/$NETWORK/fan-tan.addr)"
    log "Deployment log: $DEPLOYMENT_LOG"
}

# Execute main function
main "$@"
