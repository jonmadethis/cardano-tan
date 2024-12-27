import { Lucid, Blockfrost } from 'lucid-cardano';

export class ContractService {
    private lucid: Lucid | null = null;
    private contractAddress: string;

    constructor(contractAddress: string) {
        this.contractAddress = contractAddress;
    }

    async initialize(blockfrostApiKey: string) {
        const api = new Blockfrost(
            'https://cardano-mainnet.blockfrost.io/api/v0',
            blockfrostApiKey
        );

        this.lucid = await Lucid.new(api);
    }

    async connectWallet(): Promise<string> {
        if (!this.lucid) throw new Error('Contract service not initialized');

        // Connect to available wallet (Nami, Eternl, etc.)
        const wallet = await window.cardano.enable();
        this.lucid.selectWallet(wallet);

        return await this.lucid.wallet.address();
    }

    async placeBet(betAmount: bigint, betNumber: number): Promise<string> {
        if (!this.lucid) throw new Error('Contract service not initialized');

        const tx = await this.lucid
            .newTx()
            .payToContract(
                this.contractAddress,
                { inline: this.createBetDatum(betNumber) },
                { lovelace: betAmount }
            )
            .complete();

        const signedTx = await tx.sign().complete();
        return await signedTx.submit();
    }

    async claimWinnings(utxo: any): Promise<string> {
        if (!this.lucid) throw new Error('Contract service not initialized');

        const tx = await this.lucid
            .newTx()
            .collectFrom([utxo], this.createClaimRedeemer())
            .complete();

        const signedTx = await tx.sign().complete();
        return await signedTx.submit();
    }

    async getGameState(): Promise<{
        phase: string;
        currentBets: Array<{amount: bigint; number: number}>;
        timeRemaining: number;
        winningNumber: number | null;
    }> {
        if (!this.lucid) throw new Error('Contract service not initialized');

        const utxos = await this.lucid.utxosAt(this.contractAddress);
        const datum = await this.lucid.datumOf(utxos[0]);

        return this.parseGameState(datum);
    }

    private createBetDatum(betNumber: number): any {
        return {
            constructor: 0,
            fields: [betNumber]
        };
    }

    private createClaimRedeemer(): any {
        return {
            constructor: 0,
            fields: []
        };
    }

    private parseGameState(datum: any) {
        // Parse the datum to extract game state
        return {
            phase: 'Betting',
            currentBets: [],
            timeRemaining: 300,
            winningNumber: null
        };
    }

    async getWalletBalance(): Promise<bigint> {
        if (!this.lucid) throw new Error('Contract service not initialized');
        
        const utxos = await this.lucid.wallet.getUtxos();
        return utxos.reduce(
            (acc, utxo) => acc + utxo.assets.lovelace || 0n,
            0n
        );
    }
}

export const createContractService = (contractAddress: string) => {
    return new ContractService(contractAddress);
};
