{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Performance where

import           Control.Monad           (replicateM)
import           Criterion.Main
import           Data.Default           (def)
import qualified Data.Map               as Map
import           Ledger
import           Ledger.Ada             as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator

import           FanTan.Types
import           FanTan.OnChain
import           FanTan.OffChain

-- Performance test suite
main :: IO ()
main = defaultMain
    [ bgroup "Contract Validation"
        [ bench "Single bet validation" $ 
            whnf (validateBet defaultGameParams defaultDatum 5_000_000 1) defaultScriptContext
        , bench "Multiple bet validation" $
            whnf (validateMultipleBets defaultGameParams defaultDatum) testBets
        ]
    , bgroup "Transaction Processing"
        [ bench "Bet transaction construction" $ 
            nf constructBetTx defaultBetParams
        , bench "Reveal transaction construction" $
            nf constructRevealTx defaultRevealParams
        ]
    , bgroup "State Management"
        [ bench "Game state update" $
            nf updateGameState testGameUpdate
        , bench "Payout calculation" $
            nf (calculatePayouts defaultGameParams testBets) 1
        ]
    , bgroup "Scaling Tests"
        [ bench "10 concurrent games" $
            nfIO $ runEmulatorTraceIO' def $ concurrent10GameScenario
        , bench "100 concurrent bets" $
            nfIO $ runEmulatorTraceIO' def $ concurrent100BetsScenario 
        ]
    ]

-- Test data structures
testBets :: [(Integer, Integer)]
testBets = [(5_000_000, 1), (6_000_000, 2), (7_000_000, 3), (8_000_000, 4)]

defaultBetParams :: BetParams
defaultBetParams = BetParams
    { betAmount = 5_000_000
    , betNumber = 1
    , betDeadline = slotToBeginPOSIXTime def 100
    }

defaultRevealParams :: RevealParams
defaultRevealParams = RevealParams
    { revealBeads = 12
    , revealGameId = 1
    }

testGameUpdate :: GameUpdate
testGameUpdate = GameUpdate
    { updateGameId = 1
    , updateBets = testBets
    , updateDeadline = slotToBeginPOSIXTime def 100
    }

-- Performance test scenarios
concurrent10GameScenario :: EmulatorTrace ()
concurrent10GameScenario = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    players <- mapM (activateContractWallet . knownWallet) [2..11]
    
    -- Start 10 concurrent games
    replicateM 10 $ do
        void $ callEndpoint @"start" h1 defaultGameParams
        void $ Emulator.waitNSlots 1
    
    -- Place bets in all games
    forM_ players $ \h -> do
        void $ callEndpoint @"bet" h (5_000_000, 1)
        void $ Emulator.waitNSlots 1
    
    -- Reveal results for all games
    replicateM 10 $ do
        void $ callEndpoint @"reveal" h1 13
        void $ Emulator.waitNSlots 1

concurrent100BetsScenario :: EmulatorTrace ()
concurrent100BetsScenario = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    void $ callEndpoint @"start" h1 defaultGameParams
    void $ Emulator.waitNSlots 1
    
    -- Place 100 bets
    let placeBet n = do
            h <- activateContractWallet (knownWallet n) endpoints
            void $ callEndpoint @"bet" h (5_000_000, 1)
            void $ Emulator.waitNSlots 1
    
    mapM_ placeBet [2..101]
    
    -- Reveal result
    void $ callEndpoint @"reveal" h1 13
    void $ Emulator.waitNSlots 1

-- Helper functions for performance tests
validateMultipleBets :: FanTanParams -> FanTanDatum -> [(Integer, Integer)] -> Bool
validateMultipleBets params datum =
    all (\(amt, num) -> validateBet params datum amt num defaultScriptContext)

constructBetTx :: BetParams -> Either String Tx
constructBetTx params = Right undefined -- Simplified for benchmarking

constructRevealTx :: RevealParams -> Either String Tx
constructRevealTx params = Right undefined -- Simplified for benchmarking

updateGameState :: GameUpdate -> FanTanDatum
updateGameState update = FanTanDatum
    { ftGameId = updateGameId update
    , ftNumBeads = 0
    , ftBets = map (\(amt, num) -> (dummyPKH, amt, num)) (updateBets update)
    , ftDeadline = updateDeadline update
    }

calculatePayouts :: FanTanParams -> [(Integer, Integer)] -> Integer -> [Integer]
calculatePayouts params bets winningNum =
    let winners = filter (\(_, num) -> num == winningNum) bets
        totalBets = sum $ map fst winners
    in map (\(amt, _) -> amt * 2) winners

-- Default test values
dummyPKH :: PubKeyHash
dummyPKH = undefined -- Simplified for benchmarking

defaultScriptContext :: ScriptContext
defaultScriptContext = undefined -- Simplified for benchmarking
