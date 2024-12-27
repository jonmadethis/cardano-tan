import { Lucid, TxHash, Address } from 'lucid-cardano';
import { ContractService } from './ContractService';
import { AnalyticsService } from './AnalyticsService';
import { AdminService } from './AdminService';

interface SecurityConfig {
    rateLimits: {
        maxRequestsPerMinute: number;
        maxBetsPerHour: number;
        maxTotalBetsPerDay: number;
        maxConcurrentRequests: number;
        cooldownPeriod: number;
    };
    thresholds: {
        suspiciousAmountThreshold: bigint;
        rapidTransactionThreshold: number;
        unusualPatternThreshold: number;
        reputationScoreThreshold: number;
    };
    blockDurations: {
        temporary: number;
        medium: number;
        extended: number;
        permanent: number;
    };
    ipRestrictions: {
        maxAddressesPerIP: number;
        allowedCountries: string[];
        blacklistedRanges: string[];
        vpnRestriction: boolean;
    };
}

interface ActivityLog {
    address: string;
    ipAddress: string;
    timestamp: number;
    actionType: string;
    amount?: bigint;
    status: 'allowed' | 'blocked';
    riskScore: number;
}

class SecurityService {
    private lucid: Lucid;
    private config: SecurityConfig;
    private contractService: ContractService;
    private analyticsService: AnalyticsService;
    private adminService: AdminService;

    private addressActivityMap: Map<string, ActivityLog[]> = new Map();
    private ipActivityMap: Map<string, ActivityLog[]> = new Map();
    private temporaryBlocks: Map<string, number> = new Map();
    private reputationScores: Map<string, number> = new Map();

    constructor(
        lucid: Lucid,
        config: SecurityConfig,
        contractService: ContractService,
        analyticsService: AnalyticsService,
        adminService: AdminService
    ) {
        this.lucid = lucid;
        this.config = config;
        this.contractService = contractService;
        this.analyticsService = analyticsService;
        this.adminService = adminService;
        this.initializeSecurityMonitoring();
    }

    async validateRequest(
        address: string,
        ipAddress: string,
        actionType: string,
        amount?: bigint
    ): Promise<boolean> {
        if (await this.isBlocked(address, ipAddress)) {
            return false;
        }

        const riskAssessment = await this.assessRisk(address, ipAddress, actionType, amount);
        
        if (riskAssessment.riskScore > this.config.thresholds.reputationScoreThreshold) {
            await this.handleSuspiciousActivity(address, ipAddress, riskAssessment);
            return false;
        }

        if (!this.checkRateLimits(address, ipAddress)) {
            await this.enforceRateLimit(address, ipAddress);
            return false;
        }

        await this.logActivity(address, ipAddress, actionType, amount, 'allowed', riskAssessment.riskScore);
        return true;
    }

    private async assessRisk(
        address: string,
        ipAddress: string,
        actionType: string,
        amount?: bigint
    ): Promise<{ riskScore: number; factors: string[] }> {
        const factors: string[] = [];
        let riskScore = 0;

        // Volume-based risk assessment
        const recentActivity = await this.getRecentActivity(address, ipAddress);
        if (recentActivity.length > this.config.rateLimits.maxRequestsPerMinute) {
            riskScore += 30;
            factors.push('high_request_volume');
        }

        // Amount-based risk assessment
        if (amount && amount > this.config.thresholds.suspiciousAmountThreshold) {
            riskScore += 25;
            factors.push('large_transaction');
        }

        // Pattern-based risk assessment
        if (this.detectUnusualPatterns(recentActivity)) {
            riskScore += 20;
            factors.push('unusual_pattern');
        }

        // Reputation-based assessment
        const reputationScore = this.reputationScores.get(address) ?? 100;
        if (reputationScore < 50) {
            riskScore += 25;
            factors.push('low_reputation');
        }

        // IP-based assessment
        if (await this.isIPSuspicious(ipAddress)) {
            riskScore += 15;
            factors.push('suspicious_ip');
        }

        return { riskScore, factors };
    }

    private async handleSuspiciousActivity(
        address: string,
        ipAddress: string,
        riskAssessment: { riskScore: number; factors: string[] }
    ): Promise<void> {
        const blockDuration = this.determineBlockDuration(riskAssessment.riskScore);
        await this.applyTemporaryBlock(address, ipAddress, blockDuration);

        if (riskAssessment.riskScore > 80) {
            await this.notifyAdministrators({
                address,
                ipAddress,
                riskScore: riskAssessment.riskScore,
                factors: riskAssessment.factors,
                timestamp: Date.now()
            });
        }

        this.updateReputationScore(address, riskAssessment.riskScore);
    }

    private async enforceRateLimit(address: string, ipAddress: string): Promise<void> {
        const cooldownEnd = Date.now() + this.config.rateLimits.cooldownPeriod;
        this.temporaryBlocks.set(address, cooldownEnd);
        this.temporaryBlocks.set(ipAddress, cooldownEnd);

        await this.logActivity(
            address,
            ipAddress,
            'rate_limit',
            undefined,
            'blocked',
            0
        );
    }

    private determineBlockDuration(riskScore: number): number {
        if (riskScore > 90) return this.config.blockDurations.permanent;
        if (riskScore > 70) return this.config.blockDurations.extended;
        if (riskScore > 50) return this.config.blockDurations.medium;
        return this.config.blockDurations.temporary;
    }

    private updateReputationScore(address: string, riskScore: number): void {
        const currentScore = this.reputationScores.get(address) ?? 100;
        const newScore = Math.max(0, Math.min(100, currentScore - (riskScore / 10)));
        this.reputationScores.set(address, newScore);
    }

    private async isIPSuspicious(ipAddress: string): Promise<boolean> {
        // Implementation for IP reputation checking
        return false;
    }

    private detectUnusualPatterns(activities: ActivityLog[]): boolean {
        // Implementation for pattern detection
        return false;
    }

    private async getRecentActivity(
        address: string,
        ipAddress: string
    ): Promise<ActivityLog[]> {
        const addressActivity = this.addressActivityMap.get(address) ?? [];
        const ipActivity = this.ipActivityMap.get(ipAddress) ?? [];
        return [...addressActivity, ...ipActivity].sort((a, b) => b.timestamp - a.timestamp);
    }

    private async logActivity(
        address: string,
        ipAddress: string,
        actionType: string,
        amount: bigint | undefined,
        status: 'allowed' | 'blocked',
        riskScore: number
    ): Promise<void> {
        const activity: ActivityLog = {
            address,
            ipAddress,
            timestamp: Date.now(),
            actionType,
            amount,
            status,
            riskScore
        };

        this.updateActivityLog(address, ipAddress, activity);
        await this.analyticsService.recordSecurityEvent(activity);
    }

    private updateActivityLog(
        address: string,
        ipAddress: string,
        activity: ActivityLog
    ): void {
        // Update address activity
        const addressActivity = this.addressActivityMap.get(address) ?? [];
        addressActivity.push(activity);
        this.addressActivityMap.set(address, addressActivity.slice(-100));

        // Update IP activity
        const ipActivity = this.ipActivityMap.get(ipAddress) ?? [];
        ipActivity.push(activity);
        this.ipActivityMap.set(ipAddress, ipActivity.slice(-100));
    }

    private initializeSecurityMonitoring(): void {
        setInterval(() => {
            this.cleanupExpiredBlocks();
            this.updateReputationScores();
        }, 60000); // Run every minute
    }

    private async notifyAdministrators(alert: any): Promise<void> {
        await this.adminService.handleSecurityAlert(alert);
    }

    private cleanupExpiredBlocks(): void {
        const now = Date.now();
        for (const [key, expiry] of this.temporaryBlocks.entries()) {
            if (expiry < now) {
                this.temporaryBlocks.delete(key);
            }
        }
    }

    private async isBlocked(address: string, ipAddress: string): Promise<boolean> {
        const now = Date.now();
        return (
            (this.temporaryBlocks.get(address) ?? 0) > now ||
            (this.temporaryBlocks.get(ipAddress) ?? 0) > now
        );
    }

    private checkRateLimits(address: string, ipAddress: string): boolean {
        const recentActivity = this.getRecentActivity(address, ipAddress);
        const requestsLastMinute = recentActivity.filter(
            a => a.timestamp > Date.now() - 60000
        ).length;

        return requestsLastMinute < this.config.rateLimits.maxRequestsPerMinute;
    }
}

export default SecurityService;
