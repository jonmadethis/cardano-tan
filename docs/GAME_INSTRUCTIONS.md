# Fan-Tan Game Instructions

Welcome to Fan-Tan, a traditional gambling game implemented as a smart contract on the Cardano blockchain. This document provides comprehensive instructions for playing the game and understanding its mechanics.

## Game Overview

Fan-Tan is a game where players bet on numbers 1-4, representing the remainder when dividing a set of beads by four. The dealer reveals the number of beads, and winners are those who correctly predicted the remainder.

## How to Play

### Step 1: Connect Your Wallet

1. Click the "Connect Wallet" button in the top right corner
2. Select your Cardano wallet (Nami, Eternl, etc.)
3. Approve the connection request

### Step 2: Place Your Bet

1. Choose a number between 1 and 4
2. Enter your bet amount (minimum 5 ADA, maximum 100 ADA)
3. Click "Place Bet" to submit your transaction
4. Wait for transaction confirmation

### Step 3: Wait for Results

1. The betting phase lasts for 5 minutes
2. Watch the countdown timer for phase completion
3. The dealer will reveal the number of beads when betting closes
4. Results are automatically calculated based on the remainder

### Step 4: Claim Winnings

1. If you win, the "Claim Winnings" button becomes active
2. Click to claim your winnings
3. Approve the transaction in your wallet
4. Wait for confirmation

## Game Phases

### Betting Phase
- Players can place bets on numbers 1-4
- Multiple bets are allowed
- Minimum bet: 5 ADA
- Maximum bet: 100 ADA
- Duration: 5 minutes

### Revealing Phase
- Dealer reveals the number of beads
- No new bets accepted
- Results are calculated automatically
- Duration: Up to 2 minutes

### Claiming Phase
- Winners can claim their prizes
- Proportional payouts based on bet amounts
- 2% house fee on winnings
- Duration: 24 hours

### Expired Phase
- Game timeout reached
- All players can claim refunds
- No house fee on refunds

## Winning & Payouts

### How Winners Are Determined
1. The dealer reveals N beads
2. Calculate: N ÷ 4 = Q remainder R
3. If R = 0, winning number is 4
4. Otherwise, winning number is R

### Payout Calculation
- Winners share the total pot proportionally
- House fee: 2% of winnings
- Example: If total pot is 100 ADA and you contributed 20% of winning bets:
  - Your share: 20 ADA
  - House fee: 0.4 ADA
  - Final payout: 19.6 ADA

## Security Features

### Time Locks
- Strict phase transitions
- Automatic timeouts
- Refund mechanism for expired games

### Transaction Verification
- All bets require confirmation
- Multi-signature validation
- Automated state verification

### Player Protection
- Maximum bet limits
- Guaranteed refunds
- Transparent random number generation

## Troubleshooting

### Transaction Failed
1. Check your wallet balance
2. Ensure minimum ADA for fees
3. Try again in a few minutes

### Claim Failed
1. Verify game phase
2. Check winning number
3. Ensure claiming within time limit

### Refund Needed
1. Wait for game expiration
2. Click "Request Refund"
3. Approve transaction

## Contact & Support

For technical support or questions:
- Discord: [Link to Discord]
- Email: support@fantan.example.com
- GitHub Issues: [Repository Link]

## Terms & Conditions

Please review the complete terms and conditions before playing. By participating, you agree to:
1. Accept game outcomes as final
2. Follow fair play guidelines
3. Use only legitimate funding sources
4. Comply with all applicable regulations
