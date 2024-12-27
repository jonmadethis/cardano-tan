import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Wallet, CircleDollarSign } from 'lucide-react';

const WalletConnector = () => {
  const [wallet, setWallet] = useState({
    connected: false,
    address: '',
    balance: 0
  });

  const [error, setError] = useState('');

  const connectWallet = async () => {
    try {
      // Wallet connection logic will go here
      setWallet({
        connected: true,
        address: 'addr1...',
        balance: 100
      });
    } catch (err) {
      setError('Failed to connect wallet');
    }
  };

  return (
    <div className="p-4">
      {error && (
        <Alert variant="destructive" className="mb-4">
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {!wallet.connected ? (
        <Button onClick={connectWallet} className="w-full">
          <Wallet className="mr-2 h-4 w-4" />
          Connect Wallet
        </Button>
      ) : (
        <div className="flex flex-col gap-2">
          <div className="flex items-center justify-between p-2 bg-secondary rounded-lg">
            <span className="font-medium">Address:</span>
            <span className="text-sm truncate max-w-[200px]">{wallet.address}</span>
          </div>
          <div className="flex items-center justify-between p-2 bg-secondary rounded-lg">
            <span className="font-medium">Balance:</span>
            <div className="flex items-center">
              <CircleDollarSign className="mr-2 h-4 w-4" />
              <span>{wallet.balance} ADA</span>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default WalletConnector;
