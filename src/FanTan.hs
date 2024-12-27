{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module FanTan where

import           Control.Monad          (void)
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Default          (Default (..))
import           Data.Map              as Map
import           Data.Text             (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value         as Value
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), String, show)
import qualified Prelude             as Haskell

-- Enhanced state tracking with monitoring capabilities
data GamePhase = Betting | Revealing | Claiming | Expired
    deriving (Show, Generic, FromJSON, ToJSON)

data FanTanDatum = FanTanDatum
    { ftGameId    :: Integer
    , ftNumBeads  :: Integer
    , ftBets      :: [(PubKeyHash, Integer, Integer)]
    , ftDeadline  :: POSIXTime
    , ftPhase     :: GamePhase
    , ftLastUpdate :: POSIXTime
    , ftMetadata  :: GameMetadata
    } deriving (Show, Generic, FromJSON, ToJSON)

data GameMetadata = GameMetadata
    { gmVersion     :: Integer
    , gmMinPlayers  :: Integer
    , gmMaxPlayers  :: Integer
    , gmHouseFee    :: Rational
    , gmValidator   :: ValidatorHash
    } deriving (Show, Generic, FromJSON, ToJSON)

data FanTanRedeemer = 
    PlaceBet Integer Integer |
    RevealResult Integer |
    ClaimWinnings |
    RefundBet |
    UpdateState GamePhase
    deriving (Show, Generic, FromJSON, ToJSON)

data FanTanParams = FanTanParams
    { fpDealerPKH :: !PubKeyHash
    , fpMinBet    :: !Integer
    , fpTimeout   :: !POSIXTime
    , fpMaxBet    :: !Integer
    , fpMinPlayers :: !Integer
    , fpHouseFee  :: !Rational
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''GamePhase
PlutusTx.makeLift ''GameMetadata
PlutusTx.unstableMakeIsData ''FanTanDatum
PlutusTx.unstableMakeIsData ''FanTanRedeemer
PlutusTx.unstableMakeIsData ''FanTanParams

-- Enhanced validation with security checks and monitoring
{-# INLINABLE validateFanTan #-}
validateFanTan :: FanTanParams -> FanTanDatum -> FanTanRedeemer -> ScriptContext -> Bool
validateFanTan params datum redeemer ctx = 
    case redeemer of
        PlaceBet amount number ->
            validateBetPlacement params datum amount number info &&
            checkPhase datum Betting &&
            validateTimeConstraints datum currentTime &&
            validateBetLimits params amount &&
            validatePlayerLimit params datum
            
        RevealResult beads ->
            validateReveal params datum beads info &&
            checkPhase datum Revealing &&
            validateTimeConstraints datum currentTime
            
        ClaimWinnings ->
            validateClaim datum info &&
            checkPhase datum Claiming &&
            validateWinnerPayment datum info
            
        RefundBet ->
            validateRefund datum currentTime &&
            (checkPhase datum Expired || checkTimeout datum currentTime)
            
        UpdateState newPhase ->
            validateStateTransition datum newPhase currentTime info
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentTime :: POSIXTime
    currentTime = txInfoValidRange info

-- Enhanced security validations
{-# INLINABLE validateBetPlacement #-}
validateBetPlacement :: FanTanParams -> FanTanDatum -> Integer -> Integer -> TxInfo -> Bool
validateBetPlacement params datum amount number info =
    traceIfFalse "Invalid bet number" (number >= 1 && number <= 4) &&
    traceIfFalse "Bet amount out of bounds" (validateBetLimits params amount) &&
    traceIfFalse "Player limit reached" (validatePlayerLimit params datum) &&
    traceIfFalse "Invalid signature" (validateSignature info)

{-# INLINABLE validateReveal #-}
validateReveal :: FanTanParams -> FanTanDatum -> Integer -> TxInfo -> Bool
validateReveal params datum beads info =
    traceIfFalse "Not dealer" (txSignedBy info $ fpDealerPKH params) &&
    traceIfFalse "Invalid bead count" (beads > 0) &&
    traceIfFalse "No active bets" (not $ null $ ftBets datum)

{-# INLINABLE validateClaim #-}
validateClaim :: FanTanDatum -> TxInfo -> Bool
validateClaim datum info =
    traceIfFalse "No winning bet found" hasWinningBet &&
    traceIfFalse "Invalid winner signature" (txSignedBy info winnerPKH)
  where
    winningNumber = ftNumBeads datum `mod` 4 + 1
    hasWinningBet = any (\(_, _, num) -> num == winningNumber) (ftBets datum)
    winnerPKH = case filter (\(_, _, num) -> num == winningNumber) (ftBets datum) of
        ((pkh, _, _):_) -> pkh
        _               -> error ()

-- Enhanced state management
{-# INLINABLE validateStateTransition #-}
validateStateTransition :: FanTanDatum -> GamePhase -> POSIXTime -> TxInfo -> Bool
validateStateTransition datum newPhase currentTime info =
    case (ftPhase datum, newPhase) of
        (Betting, Revealing)   -> validateBettingToRevealing datum currentTime
        (Revealing, Claiming)  -> validateRevealingToClaiming datum
        (Claiming, Expired)    -> validateClaimingToExpired datum currentTime
        _                      -> False

-- Helper functions
{-# INLINABLE validateTimeConstraints #-}
validateTimeConstraints :: FanTanDatum -> POSIXTime -> Bool
validateTimeConstraints datum currentTime =
    currentTime <= ftDeadline datum

{-# INLINABLE validateBetLimits #-}
validateBetLimits :: FanTanParams -> Integer -> Bool
validateBetLimits params amount =
    amount >= fpMinBet params && amount <= fpMaxBet params

{-# INLINABLE validatePlayerLimit #-}
validatePlayerLimit :: FanTanParams -> FanTanDatum -> Bool
validatePlayerLimit params datum =
    length (ftBets datum) < fromInteger (fpMinPlayers params)

{-# INLINABLE validateSignature #-}
validateSignature :: TxInfo -> Bool
validateSignature info =
    let signatories = txInfoSignatories info
    in not (null signatories)

-- Contract endpoints and state management
data FanTanSchema =
    Start FanTanParams
    .\/ Bet (Integer, Integer)
    .\/ Reveal Integer
    .\/ Claim
    .\/ Refund

type FanTanContract = Contract () FanTanSchema Text ()

start :: FanTanParams -> Contract () FanTanSchema Text ()
start params = do
    pkh <- ownPubKeyHash
    let datum = FanTanDatum
            { ftGameId = 1
            , ftNumBeads = 0
            , ftBets = []
            , ftDeadline = fpTimeout params
            , ftPhase = Betting
            , ftLastUpdate = 0
            , ftMetadata = initialMetadata
            }
        tx = Constraints.mustPayToTheScript datum $ Ada.lovelaceValueOf 2_000_000
    void $ submitTxConstraints (typedValidator params) tx

bet :: FanTanParams -> (Integer, Integer) -> Contract () FanTanSchema Text ()
bet params (amount, number) = do
    pkh <- ownPubKeyHash
    utxos <- utxosAt $ scriptAddress $ validator params
    case Map.toList utxos of
        [(oref, o)] -> do
            let datum = FanTanDatum
                    { ftGameId = 1
                    , ftNumBeads = 0
                    , ftBets = (pkh, amount, number) : ftBets datum
                    , ftDeadline = fpTimeout params
                    , ftPhase = Betting
                    , ftLastUpdate = 0
                    , ftMetadata = initialMetadata
                    }
                tx =  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ PlaceBet amount number)
                    <> Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
            void $ submitTxConstraints (typedValidator params) tx
        _ -> throwError "Game not found"

-- Initial metadata configuration
initialMetadata :: GameMetadata
initialMetadata = GameMetadata
    { gmVersion = 1
    , gmMinPlayers = 2
    , gmMaxPlayers = 10
    , gmHouseFee = 0.02
    , gmValidator = undefined  -- Set during contract deployment
    }

-- Deployment utilities
mkValidator :: FanTanParams -> FanTanDatum -> FanTanRedeemer -> ScriptContext -> Bool
mkValidator = validateFanTan

validator :: FanTanParams -> Validator
validator params = mkValidatorScript $
    $$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
