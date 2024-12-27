import { Lucid } from 'lucid-cardano';
import { ContractService } from './ContractService';
import { AnalyticsService } from './AnalyticsService';

export interface ContractHealth {
    status: 'healthy' | 'warning' | 'critical';
    totalValue: bigint;
    activeGames: number;
    lastUpdate: Date;
    errorRate: number;
    avgResponseTime: number;
}

export interface GameParameters {
    minBet: bigint;
    maxBet: bigint;
    timeout: number;
    houseFee: number;
    minimumUtxo: bigint;
}

export interface EmergencyState {
    paused: boolean;
    recoveryMode: boolean;
    lastEmergencyAction: Date | null;
    activeAlerts: string[];
}

export class AdminService {
    private lucid: Lucid;
    private contractService: ContractService;
    private analyticsService: AnalyticsService;
    private adminAddress: string;
    private updateInterval: NodeJS.Timer | null = null;

    constructor(
        lucid: Lucid,
        contractService: ContractService,
        analyticsService: AnalyticsService,
        adminAddress: string
    ) {
        this.lucid = lucid;
        this.contractService = contractService;
        this.analyticsService = analyticsService;
        this.adminAddress = adminAddress;
    }

    async getContractHealth(): Promise<ContractHealth> {
        const metrics = await this.analyticsService.generatePerformanceReport();
        const utxos = await this.contractService.getContractUtxos();
        
        const totalValue = utxos.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n);
        const activeGames = await this.contractService.getActiveGameCount();
        
        return {
            status: this.determineHealthStatus(metrics),
            totalValue,
            activeGames,
            lastUpdate: new Date(),
            errorRate: metrics.errorRate,
            avgResponseTime: metrics.averageConfirmationTime
        };
    }

    async updateGameParameters(params: GameParameters): Promise<string> {
        if (!await this.verifyAdminAccess()) {
            throw new Error('Unauthorized access');
        }

        const tx = await this.contractService.updateParameters(params);
        return await this.submitAdminTransaction(tx);
    }

    async toggleEmergencyMode(mode: 'pause' | 'recovery'): Promise<EmergencyState> {
        if (!await this.verifyAdminAccess()) {
            throw new Error('Unauthorized access');
        }

        const currentState = await this.getEmergencyState();
        const newState: EmergencyState = {
            ...currentState,
            [mode === 'pause' ? 'paused' : 'recoveryMode']: 
                !currentState[mode === 'pause' ? 'paused' : 'recoveryMode'],
            lastEmergencyAction: new Date()
        };

        await this.updateEmergencyState(newState);
        return newState;
    }

    async addDealer(dealerPkh: string): Promise<string> {
        if (!await this.verifyAdminAccess()) {
            throw new Error('Unauthorized access');
        }

        const tx = await this.contractService.addAuthorizedDealer(dealerPkh);
        return await this.submitAdminTransaction(tx);
    }

    async removeDealer(dealerPkh: string): Promise<string> {
        if (!await this.verifyAdminAccess()) {
            throw new Error('Unauthorized access');
        }

        const tx = await this.contractService.removeAuthorizedDealer(dealerPkh);
        return await this.submitAdminTransaction(tx);
    }

    async startMonitoring(): Promise<void> {
        if (this.updateInterval) return;

        this.updateInterval = setInterval(async () => {
            try {
                const health = await this.getContractHealth();
                
                if (health.status === 'critical') {
                    await this.handleCriticalState(health);
                }

                // Update monitoring metrics
                await this.analyticsService.trackHealthMetrics(health);
            } catch (error) {
                console.error('Monitoring error:', error);
            }
        }, 30000); // Monitor every 30 seconds
    }

    async stopMonitoring(): Promise<void> {
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
            this.updateInterval = null;
        }
    }

    private async verifyAdminAccess(): Promise<boolean> {
        const address = await this.lucid.wallet.address();
        return address === this.adminAddress;
    }

    private async submitAdminTransaction(tx: any): Promise<string> {
        try {
            const signedTx = await tx.sign().complete();
            return await signedTx.submit();
        } catch (error) {
            console.error('Admin transaction failed:', error);
            throw new Error('Failed to submit admin transaction');
        }
    }

    private determineHealthStatus(metrics: any): ContractHealth['status'] {
        if (metrics.errorRate > 0.1 || metrics.averageConfirmationTime > 300) {
            return 'critical';
        }
        if (metrics.errorRate > 0.05 || metrics.averageConfirmationTime > 180) {
            return 'warning';
        }
        return 'healthy';
    }

    private async handleCriticalState(health: ContractHealth): Promise<void> {
        // Implement emergency response procedures
        const emergencyState = await this.getEm
