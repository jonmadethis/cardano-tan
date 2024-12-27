{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Integration where

import           Control.Lens            ((^.))
import           Control.Monad           (void)
import           Data.Default           (def)
import qualified Data.Map               as Map
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.TimeSlot
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator
import           Test.Tasty
import           Test.Tasty.HUnit
import           Wallet.Emulator.Wallet

import           FanTan.Types
import           FanTan.OnChain
import           FanTan.OffChain

tests :: TestTree
tests = testGroup "Fan-Tan Integration Tests"
    [ testGroup "Basic Game Flow"
        [ testCase "Single player game cycle" testSinglePlayerGame
        , testCase "Multiple player game cycle" testMultiPlayerGame
        , testCase "Sequential games" testSequentialGames
        ]
    , testGroup "Timeout and Refund Scenarios"
        [ testCase "Timeout with refund" testTimeoutRefund
        , testCase "Multiple refunds after timeout" testMultipleRefunds
        , testCase "Refund before timeout fails" testEarlyRefund
        ]
    , testGroup "Game Mechanics"
        [ testCase "Correct winner selection" testWinnerSelection
        , testCase "Multiple winners same number" testMultipleWinners
        , testCase "Proportional payouts" testPayoutCalculation
        ]
    , testGroup "Error Handling"
        [ testCase "Invalid bet amount rejection" testInvalidBetAmount
        , testCase "Invalid bet number rejection" testInvalidBetNumber
        , testCase "Unauthorized dealer rejection" testUnauthorizedDealer
        , testCase "Double betting handling" testDoubleBetting
        ]
    ]

-- Test Helpers

setupGame :: EmulatorTrace (ContractHandle () FanTanSchema FanTanError, WalletNumber)
setupGame = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    void $ callEndpoint @"start" h1 defaultGameParams
    void $ Emulator.waitNSlots 1
    return (h1, 1)

defaultGameParams :: FanTanParams
defaultGameParams = FanTanParams
    { fpDealerPKH = mockWalletPaymentPubKeyHash $ knownWallet 1
    , fpMinBet    = 5_000_000
    , fpTimeout   = slotToBeginPOSIXTime def 100
    }

checkFunds :: WalletNumber -> Value -> EmulatorTrace ()
checkFunds walletNum expectedValue = do
    funds <- watchWalletFunds walletNum
    Extras.logInfo @String $ "Wallet " ++ show walletNum ++ " funds: " ++ show funds
    unless (funds == expectedValue) $
        throwError $ GenericError "Unexpected wallet funds"

-- Basic Game Flow Tests

testSinglePlayerGame :: IO ()
testSinglePlayerGame = do
    let trace = runEmulatorTrace $ do
            (h1, _) <- setupGame
            h2 <- activateContractWallet (knownWallet 2) endpoints
            
            -- Place bet
            void $ callEndpoint @"bet" h2 (5_000_000, 1)
            void $ Emulator.waitNSlots 1
            
            -- Reveal winning number
            void $ callEndpoint @"reveal" h1 13  -- Results in 1 (13 mod 4)
            void $ Emulator.waitNSlots 1
            
            -- Check final balance
            checkFunds 2 (Ada.lovelaceValueOf 10_000_000)

    result <- runEmulatorTraceIO trace
    assertBool "Single player game failed" =<< checkPredicateOptions
        defaultCheckOptions
        (walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 5_000_000))
        result

testMultiPlayerGame :: IO ()
testMultiPlayerGame = runEmulatorTraceIO $ do
    let trace = do
            (h1, _) <- setupGame
            h2 <- activateContractWallet (knownWallet 2) endpoints
            h3 <- activateContractWallet (knownWallet 3) endpoints
            
            -- Place multiple bets
            void $ callEndpoint @"bet" h2 (5_000_000, 1)
            void $ Emulator.waitNSlots 1
            void $ callEndpoint @"bet" h3 (7_000_000, 2)
            void $ Emulator.waitNSlots 1
            
            -- Reveal winning number
            void $ callEndpoint @"reveal" h1 14  -- Results in 2 (14 mod 4)
            void $ Emulator.waitNSlots 1
            
            -- Verify winner gets correct payout
            checkFunds 3 (Ada.lovelaceValueOf 19_000_000)

    void trace

-- Error Handling Tests

testInvalidBetAmount :: IO ()
testInvalidBetAmount = do
    let trace = runEmulatorTrace $ do
            (h1, _) <- setupGame
            h2 <- activateContractWallet (knownWallet 2) endpoints
            
            -- Attempt bet below minimum
            void $ callEndpoint @"bet" h2 (1_000_000, 1)
            void $ Emulator.waitNSlots 1
            
            -- Verify funds unchanged
            checkFunds 2 (Ada.lovelaceValueOf 100_000_000)

    result <- runEmulatorTraceIO trace
    assertBool "Invalid bet amount not rejected" =<< checkPredicateOptions
        defaultCheckOptions
        (walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0))
        result

testUnauthorizedDealer :: IO ()
testUnauthorizedDealer = do
    let trace = runEmulatorTrace $ do
            (h1, _) <- setupGame
            h2 <- activateContractWallet (knownWallet 2) endpoints
            
            -- Attempt reveal from non-dealer
            void $ callEndpoint @"reveal" h2 13
            void $ Emulator.waitNSlots 1

    result <- runEmulatorTraceIO trace
    assertBool "Unauthorized dealer not rejected" =<< checkPredicateOptions
        defaultCheckOptions
        (walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0))
        result

-- Game Mechanics Tests

testPayoutCalculation :: IO ()
testPayoutCalculation = do
    let trace = runEmulatorTrace $ do
            (h1, _) <- setupGame
            h2 <- activateContractWallet (knownWallet 2) endpoints
            h3 <- activateContractWallet (knownWallet 3) endpoints
            
            -- Place bets with different amounts
            void $ callEndpoint @"bet" h2 (5_000_000, 1)
            void $ callEndpoint @"bet" h3 (10_000_000, 1)
            void $ Emulator.waitNSlots 1
            
            -- Reveal winning number
            void $ callEndpoint @"reveal" h1 13  -- Results in 1
            void $ Emulator.waitNSlots 1
            
            -- Verify proportional payouts
            checkFunds 2 (Ada.lovelaceValueOf 10_000_000)
            checkFunds 3 (Ada.lovelaceValueOf 20_000_000)

    result <- runEmulatorTraceIO trace
    assertBool "Payout calculation failed" =<< checkPredicateOptions
        defaultCheckOptions
        (walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 5_000_000) .&&.
         walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 10_000_000))
        result

-- Additional Helper Functions

checkGameState :: AsContractError e => Contract w s e GameState
checkGameState = do
    utxos <- utxosAt (scriptAddress $ validator defaultGameParams)
    case Map.toList utxos of
        []           -> return NoGame
        ((_, o):_) -> do
            dat <- fromJsonBytes $ _ciTxOutDatum o
            return $ getGameState dat

getGameState :: FanTanDatum -> GameState
getGameState datum
    | ftNumBeads datum == 0 = Betting
    | otherwise = case ftBets datum of
        [] -> NoGame
        _  -> if all null (ftBets datum)
              then Finished
              else Revealing
