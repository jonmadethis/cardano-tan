import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Camera, Loader2, RefreshCcw } from 'lucide-react';

const GameInterface = () => {
  const [gameState, setGameState] = useState({
    phase: 'Betting',
    currentBets: [],
    timeRemaining: 0,
    winningNumber: null,
    playerBalance: 0
  });

  const [betInput, setBetInput] = useState({
    amount: '',
    number: ''
  });

  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');

  const placeBet = async () => {
    setLoading(true);
    try {
      // Contract interaction logic will go here
      setGameState(prev => ({
        ...prev,
        currentBets: [...prev.currentBets, {
          amount: parseInt(betInput.amount),
          number: parseInt(betInput.number)
        }]
      }));
      setBetInput({ amount: '', number: '' });
    } catch (err) {
      setError('Failed to place bet');
    }
    setLoading(false);
  };

  return (
    <div className="w-full max-w-4xl mx-auto p-4">
      <Card className="mb-6">
        <CardHeader>
          <CardTitle>Fan-Tan Game</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col gap-4">
            <div className="grid grid-cols-2 gap-4">
              <div>
                <h3 className="font-semibold mb-2">Game Phase</h3>
                <p className="text-lg">{gameState.phase}</p>
              </div>
              <div>
                <h3 className="font-semibold mb-2">Time Remaining</h3>
                <p className="text-lg">{gameState.timeRemaining}s</p>
              </div>
            </div>

            {error && (
              <Alert variant="destructive" className="mb-4">
                <AlertDescription>{error}</AlertDescription>
              </Alert>
            )}

            <div className="grid grid-cols-2 gap-4">
              <Input
                type="number"
                placeholder="Bet Amount (ADA)"
                value={betInput.amount}
                onChange={(e) => setBetInput(prev => ({ ...prev, amount: e.target.value }))}
                min="1"
                max="100"
              />
              <Input
                type="number"
                placeholder="Number (1-4)"
                value={betInput.number}
                onChange={(e) => setBetInput(prev => ({ ...prev, number: e.target.value }))}
                min="1"
                max="4"
              />
            </div>

            <Button 
              onClick={placeBet} 
              disabled={loading || gameState.phase !== 'Betting'}
              className="w-full"
            >
              {loading ? (
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              ) : (
                'Place Bet'
              )}
            </Button>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Current Bets</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            {gameState.currentBets.map((bet, index) => (
              <div key={index} className="flex justify-between items-center p-2 bg-secondary rounded-lg">
                <span>Number: {bet.number}</span>
                <span>{bet.amount} ADA</span>
              </div>
            ))}
            {gameState.currentBets.length === 0 && (
              <p className="text-muted-foreground text-center">No bets placed yet</p>
            )}
          </div>
        </CardContent>
      </Card>
    </div>
  );
};

export default GameInterface;
