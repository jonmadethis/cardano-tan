.PHONY: build test clean deploy

# Default target
all: build

# Build the project
build:
	cabal build all

# Run tests
test:
	cabal test all

# Clean build artifacts
clean:
	cabal clean
	rm -rf deployment/testnet/*
	rm -rf deployment/mainnet/*
	rm -f *.plutus
	rm -f *.flat
	rm -f *.addr
	rm -f *.signed

# Format source code
format:
	find src -name "*.hs" -exec ormolu --mode inplace {} +

# Generate documentation
docs:
	cabal haddock

# Install development dependencies
setup:
	cabal update
	cabal install --only-dependencies --enable-tests

# Deploy to testnet
deploy-testnet:
	@if [ -z "$(DEALER_PKH)" ]; then \
		echo "Error: DEALER_PKH is required"; \
		exit 1; \
	fi
	cd deployment && ./deploy.sh testnet "$(DEALER_PKH)" 5000000 86400000 fan-tan

# Deploy to mainnet (requires confirmation)
deploy-mainnet:
	@if [ -z "$(DEALER_PKH)" ]; then \
		echo "Error: DEALER_PKH is required"; \
		exit 1; \
	fi
	@echo "Are you sure you want to deploy to mainnet? [y/N]"
	@read -r confirmation; \
	if [ "$$confirmation" = "y" ]; then \
		cd deployment && ./deploy.sh mainnet "$(DEALER_PKH)" 5000000 86400000 fan-tan; \
	else \
		echo "Deployment cancelled"; \
	fi

# Development helpers
repl:
	cabal repl

ghcid:
	ghcid -c "cabal repl"

# CI helpers
ci-build:
	cabal update
	cabal build all --enable-tests

ci-test:
	cabal test all --test-show-details=direct
