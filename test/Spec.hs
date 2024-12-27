{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QC

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
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fan-Tan Contract Tests"
    [ Tasty.testGroup "Unit Tests"
        [ validationTests
        , gameStateTests
        ]
    , Tasty.testGroup "Property Tests"
        [ betAmountTests
        , numberRangeTests
        ]
    , Tasty.testGroup "Integration Tests"
        [ gameFlowTests
        , timeoutTests
        ]
    ]

validationTests :: Tasty.TestTree
validationTests = Tasty.testGroup "Validation Tests"
    [ HUnit.testCase "Valid bet is accepted" $ do
        let params = defaultGameParams
            result = validateBet params defaultDatum 5000000 1 defaultInfo
        HUnit.assertBool "Should accept valid bet" result

    , HUnit.testCase "Invalid bet number is rejected" $ do
        let params = defaultGameParams
            result = validateBet params defaultDatum 5000000 5 defaultInfo
        HUnit.assertBool "Should reject invalid bet number" (not result)
    ]

gameStateTests :: Tasty.TestTree
gameStateTests = Tasty.testGroup "Game State Tests"
    [ HUnit.testCase "Game progresses from Betting to Revealing" $ do
        let trace = runEmulatorTrace $ do
                h1 <- activateContractWallet (knownWallet 1) endpoints
                h2 <- activateContractWallet (knownWallet 2) endpoints
                void $ callEndpoint @"start" h1 defaultGameParams
                void $ Emulator.waitNSlots 1
                void $ callEndpoint @"bet" h2 (5000000, 1)
        void $ runEmulatorTraceIO trace
    ]

betAmountTests :: Tasty.TestTree
betAmountTests = QC.testProperty "Bet amounts are properly validated" $
    QC.forAll (QC.choose (1, 100000000)) $ \amount ->
        let params = defaultGameParams
            result = validateBet params defaultDatum amount 1 defaultInfo
        in amount >= fpMinBet params == result

numberRangeTests :: Tasty.TestTree
numberRangeTests = QC.testProperty "Bet numbers are within valid range" $
    QC.forAll (QC.choose (-10, 15)) $ \number ->
        let params = defaultGameParams
            result = validateBet params defaultDatum 5000000 number defaultInfo
        in (number >= 1 && number <= 4) == result

gameFlowTests :: Tasty.TestTree
gameFlowTests = Tasty.testGroup "Game Flow Tests"
    [ HUnit.testCase "Complete game cycle" $ do
        let trace = runEmulatorTrace $ do
                h1 <- activateContractWallet (knownWallet 1) endpoints
                h2 <- activateContractWallet (knownWallet 2) endpoints
                void $ callEndpoint @"start" h1 defaultGameParams
                void $ Emulator.waitNSlots 1
                void $ callEndpoint @"bet" h2 (5000000, 1)
                void $ Emulator.waitNSlots 1
                void $ callEndpoint @"reveal" h1 12
        void $ runEmulatorTraceIO trace
    ]

timeoutTests :: Tasty.TestTree
timeoutTests = Tasty.testGroup "Timeout Tests"
    [ HUnit.testCase "Refund after timeout" $ do
        let trace = runEmulatorTrace $ do
                h1 <- activateContractWallet (knownWallet 1) endpoints
                h2 <- activateContractWallet (knownWallet 2) endpoints
                void $ callEndpoint @"start" h1 defaultGameParams
                void $ callEndpoint @"bet" h2 (5000000, 1)
                void $ Emulator.waitNSlots 100
                void $ callEndpoint @"refund" h2 ()
        void $ runEmulatorTraceIO trace
    ]

defaultGameParams :: FanTanParams
defaultGameParams = FanTanParams
    { fpDealerPKH = mockWalletPaymentPubKeyHash $ knownWallet 1
    , fpMinBet = 5000000
    , fpTimeout = slotToBeginPOSIXTime def 100
    }

defaultDatum :: FanTanDatum
defaultDatum = FanTanDatum
    { ftGameId = 1
    , ftNumBeads = 0
    , ftBets = []
    , ftDeadline = slotToBeginPOSIXTime def 100
    }

defaultInfo :: ScriptContext
defaultInfo = undefined  -- Mock context for unit tests
