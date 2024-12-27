import { Lucid, TxHash } from 'lucid-cardano';

export interface TransactionStatus {
    hash: string;
    status: 'pending' | 'confirmed' | 'failed';
    confirmations: number;
    timestamp: number;
    type: 'bet' | 'claim' | 'refund';
    amount?: bigint;
    betNumber?: number;
}

export interface TransactionNotification {
    type: 'success' | 'error' | 'info';
    message: string;
    timestamp: number;
    txHash?: string;
}

export class TransactionMonitor {
    private lucid: Lucid;
    private transactions: Map<string, TransactionStatus>;
    private listeners: Set<(notification: TransactionNotification) => void>;
    private checkInterval: NodeJS.Timer | null;

    constructor(lucid: Lucid) {
        this.lucid = lucid;
        this.transactions = new Map();
        this.listeners = new Set();
        this.checkInterval = null;
    }

    start() {
        if (this.checkInterval) return;

        this.checkInterval = setInterval(() => {
            this.checkTransactionStatuses();
        }, 10000); // Check every 10 seconds
    }

    stop() {
        if (this.checkInterval) {
            clearInterval(this.checkInterval);
            this.checkInterval = null;
        }
    }

    addTransaction(
        txHash: TxHash,
        type: TransactionStatus['type'],
        amount?: bigint,
        betNumber?: number
    ) {
        const status: TransactionStatus = {
            hash: txHash,
            status: 'pending',
            confirmations: 0,
            timestamp: Date.now(),
            type,
            amount,
            betNumber
        };

        this.transactions.set(txHash, status);
        this.notifyListeners({
            type: 'info',
            message: `Transaction submitted: ${this.getTransactionDescription(status)}`,
            timestamp: Date.now(),
            txHash
        });
    }

    subscribe(callback: (notification: TransactionNotification) => void) {
        this.listeners.add(callback);
        return () => this.listeners.delete(callback);
    }

    private async checkTransactionStatuses() {
        for (const [txHash, status] of this.transactions.entries()) {
            if (status.status === 'confirmed' || status.status === 'failed') continue;

            try {
                const confirmation = await this.lucid.awaitTx(txHash);
                if (confirmation) {
                    const newStatus = { ...status, status: 'confirmed', confirmations: 1 };
                    this.transactions.set(txHash, newStatus);
                    this.notifySuccess(newStatus);
                }
            } catch (error) {
                if (Date.now() - status.timestamp > 3600000) { // 1 hour timeout
                    const failedStatus = { ...status, status: 'failed' };
                    this.transactions.set(txHash, failedStatus);
                    this.notifyFailure(failedStatus);
                }
            }
        }
    }

    private notifyListeners(notification: TransactionNotification) {
        this.listeners.forEach(listener => listener(notification));
    }

    private notifySuccess(status: TransactionStatus) {
        this.notifyListeners({
            type: 'success',
            message: `Transaction confirmed: ${this.getTransactionDescription(status)}`,
            timestamp: Date.now(),
            txHash: status.hash
        });
    }

    private notifyFailure(status: TransactionStatus) {
        this.notifyListeners({
            type: 'error',
            message: `Transaction failed: ${this.getTransactionDescription(status)}`,
            timestamp: Date.now(),
            txHash: status.hash
        });
    }

    private getTransactionDescription(status: TransactionStatus): string {
        switch (status.type) {
            case 'bet':
                return `Bet of ${formatLovelace(status.amount!)} on number ${status.betNumber}`;
            case 'claim':
                return 'Claim winnings';
            case 'refund':
                return 'Refund bet';
            default:
                return 'Transaction';
        }
    }
}

function formatLovelace(amount: bigint): string {
    return `${Number(amount) / 1_000_000} ADA`;
}

export const createTransactionMonitor = (lucid: Lucid) => {
    return new TransactionMonitor(lucid);
};
