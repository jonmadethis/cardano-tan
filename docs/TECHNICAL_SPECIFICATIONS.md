# Fan-Tan Smart Contract Technical Specification

## Contract Architecture

The Fan-Tan smart contract implements a decentralized gambling game on the Cardano blockchain. This document outlines the technical implementation details, security considerations, and operational parameters.

### State Machine Design

The contract implements a state machine with three distinct phases:
1. Betting Phase: Players submit bets with amounts and chosen numbers (1-4)
2. Reveal Phase: The dealer reveals the bead count, determining the winning number
3. Payout Phase: Winners claim their rewards or players request refunds after timeout

### Data Structures

The contract uses the following primary data structures:

FanTanDatum:
```haskell
data FanTanDatum = FanTanDatum
    { ftGameId    :: Integer
    , ftNumBeads  :: Integer
    , ftBets      :: [(PubKeyHash, Integer, Integer)]
    , ftDeadline  :: POSIXTime
    }
```

This structure maintains the complete game state, tracking all bets and game parameters.

### Validation Rules

The contract enforces the following validation rules:

1. Bet Placement
   - Minimum bet amount must be met
   - Bet numbers must be between 1 and 4
   - Bets must be placed before the deadline
   - Multiple bets from the same address are allowed

2. Result Revelation
   - Only the authorized dealer can reveal results
   - Revelation must occur after betting deadline
   - Bead count must be positive
   - Winning number is calculated as (beadCount mod 4) + 1

3. Payout Distribution
   - Winners receive proportional payouts based on bet amounts
   - House fee is deducted before payout calculation
   - Minimum payout amounts must cover transaction fees

### Security Measures

1. Timing Controls
   - Strict phase transitions prevent early revelations
   - Timeout mechanism ensures funds aren't locked indefinitely
   - Grace period for technical issues

2. Access Controls
   - Dealer operations require signature verification
   - Player operations validate against stored public key hashes
   - Multi-signature capability for administrative functions

3. Fund Security
   - Automated refund mechanism
   - Maximum bet limits
   - Transaction validation prevents double-spending

### Performance Optimizations

1. Transaction Structure
   - Optimized datum size
   - Efficient UTXO management
   - Batched operations where possible

2. Memory Usage
   - Bounded list sizes
   - Efficient data encoding
   - Optimized validator script size

### Network Parameters

Mainnet Configuration:
```json
{
    "minBetAmount": 5000000,
    "maxBetAmount": 500000000,
    "timeout": 86400000,
    "houseFee": 0.02,
    "refundDelay": 86400000
}
```

Testnet Configuration:
```json
{
    "minBetAmount": 2000000,
    "maxBetAmount": 100000000,
    "timeout": 3600000,
    "houseFee": 0.01,
    "refundDelay": 3600000
}
```

### Error Handling

The contract implements comprehensive error handling:

1. Invalid Inputs
   - Insufficient funds
   - Invalid bet numbers
   - Unauthorized operations
   - Timing violations

2. Network Conditions
   - Transaction failures
   - Network congestion
   - Slippage protection

3. Recovery Mechanisms
   - Automated refunds
   - Manual intervention procedures
   - State recovery options

### Monitoring and Maintenance

The contract includes built-in monitoring capabilities:

1. Event Emission
   - Bet placement events
   - Result revelation events
   - Payout distribution events
   - Error events

2. State Queries
   - Current game state
   - Bet history
   - Player statistics
   - Performance metrics

### Future Enhancements

Planned improvements for future versions:

1. Technical Improvements
   - Optimized UTXO management
   - Enhanced parallelization
   - Reduced transaction sizes

2. Feature Additions
   - Multiple simultaneous games
   - Tournament support
   - Advanced betting options
   - Enhanced randomness sources
