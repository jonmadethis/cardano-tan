# Contributing to Fan-Tan Smart Contract

We appreciate your interest in contributing to the Fan-Tan smart contract project. This document provides guidelines and information about contributing.

## Development Setup

1. Install required dependencies:
   - GHC 8.10.7 or later
   - Cabal 3.6.2.0 or later
   - Cardano Node 1.35.4 or later
   - Plutus Application Framework

2. Clone and build the project:
```bash
git clone https://github.com/yourusername/fan-tan-contract.git
cd fan-tan-contract
make setup
make build
```

## Development Workflow

1. Create a new branch for your feature or bug fix:
```bash
git checkout -b feature/your-feature-name
```

2. Make your changes, ensuring you:
   - Follow the Haskell style guide
   - Add appropriate tests
   - Update documentation as needed
   - Keep commits focused and atomic

3. Run tests locally:
```bash
make test
```

4. Submit a pull request:
   - Provide a clear description of the changes
   - Link to any relevant issues
   - Ensure CI checks pass

## Code Style Guidelines

- Follow the standard Haskell style guide
- Use meaningful variable names
- Document functions with Haddock comments
- Keep functions focused and reasonable in length
- Use type signatures for all top-level definitions

## Testing Guidelines

- Write unit tests for all new functions
- Include property-based tests where appropriate
- Add integration tests for contract interactions
- Maintain test coverage above 80%

## Documentation

- Update README.md for significant changes
- Add Haddock documentation for new modules and functions
- Include usage examples for new features
- Update deployment documentation as needed

## Review Process

1. All changes require review before merging
2. Address review comments promptly
3. Keep discussions focused and professional
4. Ensure CI checks pass before requesting review

## Smart Contract Security

- Follow Plutus best practices
- Consider all possible edge cases
- Test thoroughly with different parameters
- Document security considerations

## Questions and Support

- Create an issue for bug reports or feature requests
- Join our developer community on Discord
- Check existing documentation and issues first

Thank you for contributing to the Fan-Tan smart contract project!
