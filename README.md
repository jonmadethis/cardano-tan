# Fan-Tan Smart Contract

A Cardano smart contract implementation of the traditional Fan-Tan gambling game.

## Overview

This smart contract enables playing Fan-Tan on the Cardano blockchain, with the following features:
- Secure random number generation through dealer input
- Automated bet handling and payout distribution
- Configurable parameters for minimum bets and timeouts
- Full on-chain validation of game rules

## Prerequisites

- GHC 8.10.7 or later
- Cabal 3.6.2.0 or later
- Cardano Node 1.35.4 or later
- Cardano CLI 1.35.4 or later
- Plutus Application Framework

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/fan-tan-contract.git
cd fan-tan-contract
```

2. Build the project:
```bash
cabal update
cabal build
```

## Development Setup

1. Install development dependencies:
```bash
cabal install --only-dependencies --enable-tests
```

2. Run the test suite:
```bash
cabal test
```

## Deployment

1. Configure your network parameters in `deployment/config.json`

2. Run the deployment script:
```bash
cd deployment
./deploy.sh testnet "<dealer-public-key-hash>" 5000000 86400000 fan-tan
```

3. Verify the deployment:
```bash
cardano-cli query utxo --address $(cat fan-tan.addr) --testnet-magic 1097911063
```

## Contract Parameters

- `fpDealerPKH`: Public key hash of the authorized dealer
- `fpMinBet`: Minimum bet amount in Lovelace (1 ADA = 1,000,000 Lovelace)
- `fpTimeout`: Time limit for betting phase in milliseconds

## Security Considerations

- The contract includes timeouts to prevent dealer manipulation
- All bets are locked in the contract until game completion
- Automated refund mechanism for expired games
- Full validation of all game actions on-chain

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

Distributed under the Apache License 2.0. See `LICENSE` for more information.

## Contact

Your Name - [@yourusername](https://twitter.com/yourusername)
Project Link: [https://github.com/yourusername/fan-tan-contract](https://github.com/yourusername/fan-tan-contract)
