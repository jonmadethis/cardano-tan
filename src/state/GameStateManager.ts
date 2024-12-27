import { createContext, useContext } from 'react';
import { ContractService } from '../services/ContractService';

export interface GameState {
    phase: 'Betting' | 'Revealing' | 'Claiming' | 'Expired';
    currentBets: Array<{
        address: string;
        amount: bigint;
        number: number;
        timestamp: number;
    }>;
    timeRemaining: number;
    winningNumber: number | null;
    totalPot: bigint;
    minBet: bigint;
    maxBet: bigint;
    playerCount: number;
    lastUpdate: number;
}

export interface GameStateContextType {
    state: GameState;
    dispatch: (action: GameStateAction) => void;
}

export type GameStateAction =
    | { type: 'PLACE_BET'; payload: { address: string; amount: bigint; number: number } }
    | { type: 'REVEAL_RESULT'; payload: { number: number } }
    | { type: 'CLAIM_WINNINGS'; payload: { address: string } }
    | { type: 'UPDATE_TIME'; payload: { remaining: number } }
    | { type: 'SET_PHASE'; payload: { phase: GameState['phase'] } }
    | { type: 'RESET_GAME' };

export const initialState: GameState = {
    phase: 'Betting',
    currentBets: [],
    timeRemaining: 300,
    winningNumber: null,
    totalPot: 0n,
    minBet: 5_000_000n,
    maxBet: 100_000_000n,
    playerCount: 0,
    lastUpdate: Date.now()
};

export function gameStateReducer(state: GameState, action: GameStateAction): GameState {
    switch (action.type) {
        case 'PLACE_BET':
            return {
                ...state,
                currentBets: [
                    ...state.currentBets,
                    {
                        address: action.payload.address,
                        amount: action.payload.amount,
                        number: action.payload.number,
                        timestamp: Date.now()
                    }
                ],
                totalPot: state.totalPot + action.payload.amount,
                playerCount: new Set([...state.currentBets.map(b => b.address), action.payload.address]).size
            };

        case 'REVEAL_RESULT':
            return {
                ...state,
                phase: 'Claiming',
                winningNumber: action.payload.number,
                lastUpdate: Date.now()
            };

        case 'CLAIM_WINNINGS':
            return {
                ...state,
                currentBets: state.currentBets.filter(bet => bet.address !== action.payload.address)
            };

        case 'UPDATE_TIME':
            const newTimeRemaining = action.payload.remaining;
            if (newTimeRemaining <= 0 && state.phase === 'Betting') {
                return {
                    ...state,
                    phase: 'Revealing',
                    timeRemaining: 0
                };
            }
            return {
                ...state,
                timeRemaining: newTimeRemaining
            };

        case 'SET_PHASE':
            return {
                ...state,
                phase: action.payload.phase,
                lastUpdate: Date.now()
            };

        case 'RESET_GAME':
            return {
                ...initialState,
                lastUpdate: Date.now()
            };

        default:
            return state;
    }
}

export class GameStateManager {
    private contractService: ContractService;
    private updateInterval: number;
    private stateUpdateCallback: (state: GameState) => void;

    constructor(
        contractService: ContractService,
        updateInterval: number = 1000,
        stateUpdateCallback: (state: GameState) => void
    ) {
        this.contractService = contractService;
        this.updateInterval = updateInterval;
        this.stateUpdateCallback = stateUpdateCallback;
        this.startStateUpdates();
    }

    private async startStateUpdates() {
        setInterval(async () => {
            try {
                const newState = await this.contractService.getGameState();
                this.stateUpdateCallback(newState as GameState);
            } catch (error) {
                console.error('Failed to update game state:', error);
            }
        }, this.updateInterval);
    }

    async placeBet(amount: bigint, number: number): Promise<string> {
        return await this.contractService.placeBet(amount, number);
    }

    async claimWinnings(): Promise<string> {
        const utxo = await this.getWinningUtxo();
        return await this.contractService.claimWinnings(utxo);
    }

    private async getWinningUtxo() {
        // Implementation to find the winning UTXO
        return null;
    }
}

export const GameStateContext = createContext<GameStateContextType>({
    state: initialState,
    dispatch: () => null
});

export const useGameState = () => useContext(GameStateContext);
