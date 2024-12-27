export interface NetworkConfig {
    name: string;
    networkId: number;
    networkMagic: number;
    contract: ContractConfig;
    api: ApiConfig;
    explorer: ExplorerConfig;
    timing: TimingConfig;
    monitoring: MonitoringConfig;
}

interface ContractConfig {
    address: string;
    scriptHash: string;
    minBet: bigint;
    maxBet: bigint;
    timeout: number;
    houseFee: number;
    minimumUtxo: bigint;
    validatorVersion: string;
}

interface ApiConfig {
    url: string;
    blockfrostProjectId: string;
    timeout: number;
    retryAttempts: number;
    retryDelay: number;
    maxConcurrentRequests: number;
}

interface ExplorerConfig {
    url: string;
    txPath: string;
    addressPath: string;
    assetsPath: string;
}

interface TimingConfig {
    bettingPhaseDuration: number;
    revealingPhaseDuration: number;
    claimingPhaseDuration: number;
    minConfirmations: number;
    maxWaitTime: number;
}

interface MonitoringConfig {
    enabled: boolean;
    updateInterval: number;
    maxRetries: number;
    webhookUrl?: string;
    alertThresholds: {
        highValue: bigint;
        timeoutWarning: number;
        errorRate: number;
    };
}

const mainnetConfig: NetworkConfig = {
    name: 'mainnet',
    networkId: 1,
    networkMagic: 764824073,
    contract: {
        address: 'addr1...',
        scriptHash: 'script1...',
        minBet: 5_000_000n,
        maxBet: 100_000_000n,
        timeout: 3600,
        houseFee: 0.02,
        minimumUtxo: 2_000_000n,
        validatorVersion: '1.0.0'
    },
    api: {
        url: 'https://cardano-mainnet.blockfrost.io/api/v0',
        blockfrostProjectId: process.env.BLOCKFROST_PROJECT_ID_MAINNET || '',
        timeout: 30000,
        retryAttempts: 3,
        retryDelay: 2000,
        maxConcurrentRequests: 10
    },
    explorer: {
        url: 'https://cardanoscan.io',
        txPath: '/transaction',
        addressPath: '/address',
        assetsPath: '/token'
    },
    timing: {
        bettingPhaseDuration: 300000,    // 5 minutes
        revealingPhaseDuration: 120000,   // 2 minutes
        claimingPhaseDuration: 86400000,  // 24 hours
        minConfirmations: 20,
        maxWaitTime: 3600000              // 1 hour
    },
    monitoring: {
        enabled: true,
        updateInterval: 10000,
        maxRetries: 5,
        webhookUrl: process.env.MONITORING_WEBHOOK_MAINNET,
        alertThresholds: {
            highValue: 1000_000_000n,     // 1000 ADA
            timeoutWarning: 540,          // 9 minutes
            errorRate: 0.05               // 5%
        }
    }
};

const testnetConfig: NetworkConfig = {
    name: 'testnet',
    networkId: 0,
    networkMagic: 1097911063,
    contract: {
        address: 'addr_test1...',
        scriptHash: 'script1...',
        minBet: 2_000_000n,
        maxBet: 50_000_000n,
        timeout: 1800,
        houseFee: 0.01,
        minimumUtxo: 1_000_000n,
        validatorVersion: '1.0.0'
    },
    api: {
        url: 'https://cardano-testnet.blockfrost.io/api/v0',
        blockfrostProjectId: process.env.BLOCKFROST_PROJECT_ID_TESTNET || '',
        timeout: 15000,
        retryAttempts: 5,
        retryDelay: 1000,
        maxConcurrentRequests: 20
    },
    explorer: {
        url: 'https://testnet.cardanoscan.io',
        txPath: '/transaction',
        addressPath: '/address',
        assetsPath: '/token'
    },
    timing: {
        bettingPhaseDuration: 180000,     // 3 minutes
        revealingPhaseDuration: 60000,    // 1 minute
        claimingPhaseDuration: 3600000,   // 1 hour
        minConfirmations: 3,
        maxWaitTime: 1800000              // 30 minutes
    },
    monitoring: {
        enabled: true,
        updateInterval: 5000,
        maxRetries: 10,
        webhookUrl: process.env.MONITORING_WEBHOOK_TESTNET,
        alertThresholds: {
            highValue: 100_000_000n,      // 100 ADA
            timeoutWarning: 240,          // 4 minutes
            errorRate: 0.10               // 10%
        }
    }
};

const previewConfig: NetworkConfig = {
    ...testnetConfig,
    name: 'preview',
    networkMagic: 2,
    contract: {
        ...testnetConfig.contract,
        address: 'addr_test1...',
        minBet: 1_000_000n,
        maxBet: 10_000_000n
    }
};

export const networks: Record<string, NetworkConfig> = {
    mainnet: mainnetConfig,
    testnet: testnetConfig,
    preview: previewConfig
};

export const getNetworkConfig = (network: string): NetworkConfig => {
    const config = networks[network];
    if (!config) {
        throw new Error(`Network configuration not found for: ${network}`);
    }
    return config;
};

export const getCurrentNetwork = (): string => {
    return process.env.CARDANO_NETWORK || 'testnet';
};

export const validateNetworkConfig = (config: NetworkConfig): void => {
    if (!config.contract.address || !config.contract.scriptHash) {
        throw new Error('Invalid contract configuration');
    }
    if (!config.api.blockfrostProjectId) {
        throw new Error('Blockfrost project ID not configured');
    }
    if (config.contract.minBet >= config.contract.maxBet) {
        throw new Error('Invalid bet limits configuration');
    }
};
