#!/bin/bash
set -euo pipefail

# Configuration
CARDANO_CLI="cardano-cli"
PROJECT_ROOT=$(git rev-parse --show-toplevel)
DEPLOYMENT_DIR="$PROJECT_ROOT/deployment"
LOG_DIR="$PROJECT_ROOT/logs"
METRICS_DIR="$PROJECT_ROOT/metrics"

# Command line arguments
NETWORK="$1"
CONTRACT_ADDRESS="$2"
INTERVAL="${3:-60}"  # Default monitoring interval: 60 seconds

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

# Setup directories
mkdir -p "$LOG_DIR"
mkdir -p "$METRICS_DIR"

# Initialize log files
MONITOR_LOG="$LOG_DIR/monitor_$(date +%Y%m%d_%H%M%S).log"
METRICS_FILE="$METRICS_DIR/metrics_$(date +%Y%m%d_%H%M%S).csv"

# Initialize metrics file with headers
echo "timestamp,utxo_count,total_value,active_games,total_bets" > "$METRICS_FILE"

# Function to query contract state
query_contract_state() {
    local timestamp=$(date +%s)
    
    # Query UTxO at contract address
    $CARDANO_CLI query utxo \
        --address "$CONTRACT_ADDRESS" \
        $NETWORK_PARAM \
        --out-file "$DEPLOYMENT_DIR/$NETWORK/current_utxo.json"
    
    # Extract metrics
    local utxo_count=$(jq 'length' "$DEPLOYMENT_DIR/$NETWORK/current_utxo.json")
    local total_value=$(jq '[.[].value.lovelace] | add // 0' "$DEPLOYMENT_DIR/$NETWORK/current_utxo.json")
    local active_games=$(jq '[.[].data] | length // 0' "$DEPLOYMENT_DIR/$NETWORK/current_utxo.json")
    local total_bets=$(jq '[.[].data.fields[2]] | add // 0' "$DEPLOYMENT_DIR/$NETWORK/current_utxo.json")
    
    # Log metrics
    echo "$timestamp,$utxo_count,$total_value,$active_games,$total_bets" >> "$METRICS_FILE"
    
    # Log state changes
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Contract state:" >> "$MONITOR_LOG"
    echo "  UTxO Count: $utxo_count" >> "$MONITOR_LOG"
    echo "  Total Value: $total_value lovelace" >> "$MONITOR_LOG"
    echo "  Active Games: $active_games" >> "$MONITOR_LOG"
    echo "  Total Bets: $total_bets" >> "$MONITOR_LOG"
    echo "----------------------------------------" >> "$MONITOR_LOG"
    
    # Alert on significant changes
    if [ "$total_value" -gt 1000000000000 ]; then
        echo "ALERT: High value detected in contract!" | tee -a "$MONITOR_LOG"
    fi
}

# Function to generate metrics report
generate_report() {
    local report_file="$METRICS_DIR/report_$(date +%Y%m%d_%H%M%S).txt"
    
    echo "Fan-Tan Contract Metrics Report" > "$report_file"
    echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')" >> "$report_file"
    echo "" >> "$report_file"
    
    # Calculate statistics
    awk -F',' '
        NR>1 {
            sum_utxo+=$2; sum_value+=$3; sum_games+=$4; sum_bets+=$5;
            count++
        }
        END {
            print "Average UTxO Count: " sum_utxo/count;
            print "Average Total Value: " sum_value/count;
            print "Average Active Games: " sum_games/count;
            print "Average Total Bets: " sum_bets/count;
        }
    ' "$METRICS_FILE" >> "$report_file"
}

# Main monitoring loop
echo "Starting contract monitoring..."
echo "Logging to: $MONITOR_LOG"
echo "Metrics stored in: $METRICS_FILE"

trap 'echo "Monitoring stopped. Generating final report..."; generate_report; exit 0' SIGINT SIGTERM

while true; do
    query_contract_state
    sleep "$INTERVAL"
done
