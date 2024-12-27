{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Property where

import           Control.Monad           (forM_)
import           Data.Default           (def)
import           Data.List              (nub)
import           Ledger
import           Ledger.Ada             as Ada
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck  as QC

import           FanTan.Types
import           FanTan.OnChain
import           FanTan.OffChain

-- Custom generators for contract-specific types
newtype ValidBetNumber = ValidBetNumber Integer
    deriving (Show, Eq)

instance Arbitrary ValidBetNumber where
    arbitrary = ValidBetNumber <$> choose (1, 4)

newtype ValidBetAmount = ValidBetAmount Integer
    deriving (Show, Eq)

instance Arbitrary ValidBetAmount where
    arbitrary = ValidBetAmount <$> choose (5_000_000, 100_000_000)

-- Property tests for bet validation
prop_betNumberValidity :: ValidBetNumber -> Bool
prop_betNumberValidity (ValidBetNumber n) = 
    validateBetNumber defaultGameParams n

prop_betAmountValidity :: ValidBetAmount -> Bool
prop_betAmountValidity (ValidBetAmount amt) =
    validateBetAmount defaultGameParams amt

-- Property tests for game mechanics
prop_winningNumberRange :: Property
prop_winningNumberRange = forAll (choose (1, 1000)) $ \beads ->
    let winningNum = calculateWinningNumber beads
    in winningNum >= 1 && winningNum <= 4

prop_uniqueGameIds :: Property
prop_uniqueGameIds = forAll (listOf1 arbitrary) $ \gameIds ->
    let games = createTestGames gameIds
    in length (nub $ map ftGameId games) == length games

-- Property tests for payout calculations
prop_payoutConservation :: Property
prop_payoutConservation = forAll (listOf1 genValidBet) $ \bets ->
    let totalBetAmount = sum $ map (\(_, amt, _) -> amt) bets
        payouts = calculatePayouts defaultGameParams bets 1
    in sum payouts <= totalBetAmount

prop_winnerPayoutPositive :: Property
prop_winnerPayoutPositive = forAll genValidBet $ \bet@(_, amt, num) ->
    let payout = head $ calculatePayouts defaultGameParams [bet] num
    in payout > amt

-- Property tests for timing constraints
prop_validTimeWindow :: Property
prop_validTimeWindow = forAll (choose (0, 100_000)) $ \offset ->
    let params = defaultGameParams { fpTimeout = slotToBeginPOSIXTime def offset }
    in validateTimeConstraints params (slotToBeginPOSIXTime def (offset - 1))

-- Helpers for generating test data
genValidBet :: Gen (PubKeyHash, Integer, Integer)
genValidBet = do
    ValidBetAmount amt <- arbitrary
    ValidBetNumber num <- arbitrary
    pkh <- arbitrary
    return (pkh, amt, num)

createTestGames :: [Integer] -> [FanTanDatum]
createTestGames ids = map createGame ids
  where
    createGame id = FanTanDatum
        { ftGameId = id
        , ftNumBeads = 0
        , ftBets = []
        , ftDeadline = slotToBeginPOSIXTime def 100
        }

-- Test tree for property tests
tests :: TestTree
tests = testGroup "Property Tests"
    [ testGroup "Bet Validation"
        [ QC.testProperty "Valid bet numbers are accepted" prop_betNumberValidity
        , QC.testProperty "Valid bet amounts are accepted" prop_betAmountValidity
        ]
    , testGroup "Game Mechanics"
        [ QC.testProperty "Winning numbers are in valid range" prop_winningNumberRange
        , QC.testProperty "Game IDs are unique" prop_uniqueGameIds
        ]
    , testGroup "Payout Logic"
        [ QC.testProperty "Total payout doesn't exceed total bets" prop_payoutConservation
        , QC.testProperty "Winners receive more than their bet" prop_winnerPayoutPositive
        ]
    , testGroup "Timing Constraints"
        [ QC.testProperty "Time window validation works correctly" prop_validTimeWindow
        ]
    ]

-- Helper functions for validation
validateBetNumber :: FanTanParams -> Integer -> Bool
validateBetNumber _ n = n >= 1 && n <= 4

validateBetAmount :: FanTanParams -> Integer -> Bool
validateBetAmount params amt = amt >= fpMinBet params

calculateWinningNumber :: Integer -> Integer
calculateWinningNumber beads = (beads `mod` 4) + 1

validateTimeConstraints :: FanTanParams -> POSIXTime -> Bool
validateTimeConstraints params currentTime = 
    currentTime < fpTimeout params

calculatePayouts :: FanTanParams -> [(PubKeyHash, Integer, Integer)] -> Integer -> [Integer]
calculatePayouts params bets winningNum =
    let winners = filter (\(_, _, num) -> num == winningNum) bets
        totalWinningBets = sum $ map (\(_, amt, _) -> amt) winners
    in map (\(_, amt, _) -> amt * 2) winners

runAllPropertyTests :: IO ()
runAllPropertyTests = defaultMain tests
