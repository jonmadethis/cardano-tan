{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Criterion.Main
import           Control.Monad        (void)
import           Data.Default        (Default (..))
import qualified Data.Map            as Map
import           Ledger             
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator
import           FanTan.Types
import           FanTan.OnChain
import           FanTan.OffChain

main :: IO ()
main = defaultMain
    [ bgroup "Validation"
        [ bench "Single bet validation" $ whnf (validateBet defaultGameParams defaultDatum 5000000 1) defaultInfo
        , bench "Multiple bets validation" $ whnf (validateMultipleBets defaultGameParams defaultDatum) testBets
        ]
    , bgroup "Game Flow"
        [ bench "Complete game cycle" $ nf runGameCycle defaultGameParams
        , bench "Timeout handling" $ nf runTimeoutScenario defaultGameParams
        ]
    , bgroup "Transaction Construction"
        [ bench "Bet transaction" $ nf constructBetTx defaultBetParams
        , bench "Reveal transaction" $ nf constructRevealTx defaultRevealParams
        ]
    ]

validateMultipleBets :: FanTanParams -> FanTanDatum -> [(Integer, Integer)] -> Bool
validateMultipleBets params datum = all (\(amount, number) -> validateBet params datum amount number defaultInfo)

testBets :: [(Integer, Integer)]
testBets = [(5000000, 1), (6000000, 2), (7000000, 3), (8000000, 4)]

runGameCycle :: FanTanParams -> EmulatorTrace ()
runGameCycle params = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    void $ callEndpoint @"start" h1 params
    void $ Emulator.waitNSlots 1
    void $ callEndpoint @"bet" h2 (5000000, 1)
    void $ Emulator.waitNSlots 1
    void $ callEndpoint @"reveal" h1 12

runTimeoutScenario :: FanTanParams -> EmulatorTrace ()
runTimeoutScenario params = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    void $ callEndpoint @"start" h1 params
    void $ callEndpoint @"bet" h2 (5000000, 1)
    void $ Emulator.waitNSlots 100
    void $ callEndpoint @"refund" h2 ()

defaultBetParams :: BetParams
defaultBetParams = BetParams
    { betAmount = 5000000
    , betNumber = 1
    , betDeadline = slotToBeginPOSIXTime def 100
    }

defaultRevealParams :: RevealParams
defaultRevealParams = RevealParams
    { revealBeads = 12
    , revealGameId = 1
    }

constructBetTx :: BetParams -> Either String Tx
constructBetTx = undefined  -- Implementation for benchmark

constructRevealTx :: RevealParams -> Either String Tx
constructRevealTx = undefined  -- Implementation for benchmark
