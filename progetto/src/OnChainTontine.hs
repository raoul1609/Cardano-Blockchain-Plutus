{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module OnChainTontine where

import           Plutus.V1.Ledger.Value (valueOf, adaToken, adaSymbol)
import           Plutus.V2.Ledger.Api      (BuiltinData, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            Validator, mkValidatorScript, BuiltinByteString)
import           Plutus.V2.Ledger.Contexts (txSignedBy, txOutValue, txInInfoResolved, txInInfoOutRef,
                                            findTxInByTxOutRef, txInfoInputs, findOwnInput, txInfoFee
                                            )
import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift, unstableMakeIsData)
import           PlutusTx.Prelude          (Bool(..), traceIfFalse, (==), (&&), (.),
                                            Integer, otherwise, map, filter,
                                            Maybe(..), foldr, Ord(..), (+), length, (/=)
                                            , (||), elem)
import           Prelude                   (IO, FilePath)
import           Utilities                 (wrapValidator, writeValidatorToFile, writeDataToFile)
import qualified PlutusTx.IsData.Class

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data OperationType = OperationType {getOperationType :: BuiltinByteString}
unstableMakeIsData ''OperationType

data Transactor = Transactor {
    name :: BuiltinByteString
}
unstableMakeIsData ''Transactor

data Member = Member {
    identifier  :: BuiltinByteString,
    mane        :: BuiltinByteString,
    phonenumber :: BuiltinByteString
}
unstableMakeIsData ''Member

data TontineDatum = TontineDatum {
    operation    :: OperationType,
    transactor   :: Transactor,
    member       :: Member
}
unstableMakeIsData ''TontineDatum

data RedeemerData = RedeemerData {
    operationType :: OperationType,
    for           :: BuiltinByteString
}
unstableMakeIsData ''RedeemerData
--unstableMakeIsData ''TontineDatum

{-
    La tontine est parametre par :
    1- Le montant fixe de la cotisation
    2- Les cles publics des signataires
    3- Les periodes de cotisation
    4- Les cles publics des membres charges de l'ouverture et de fermeture de la tontine
    5- La liste des membres de la reunion 
    6- L'ordre pour beneficier 
-}

data ParamsData = ParamsData {
    minimumAmount :: Integer,
    payPubKeyHash :: [PubKeyHash],
    openClosePubKeyHash :: [PubKeyHash],
    membres :: [BuiltinByteString]
}
makeLift ''ParamsData

open       :: BuiltinByteString
open       = "OPEN"

close      :: BuiltinByteString
close      = "CLOSE"

tontine    :: BuiltinByteString
tontine    = "TONTINE"

pay        :: BuiltinByteString
pay        = "PAY"



{-# INLINABLE mkValidatorTontinard #-}
mkValidatorTontinard :: ParamsData -> TontineDatum -> RedeemerData -> ScriptContext -> Bool
mkValidatorTontinard pd  (TontineDatum (OperationType datumOt) _ _) (RedeemerData (OperationType to) membreid) ctx
    | to == open     = traceIfFalse "Tontine deja ouverte. Impossible d'ouvrir!" verifiertontineOuvrable
    | to == close    = traceIfFalse "Tontine deja Fermee. Impossible de fermer!"  verifierTontineFermable
    | to == tontine  = traceIfFalse "Merci de verifier que vous avez suffisament de fond ou que la tontine est deja ouverte" verifierTontineTontinable -- verifier que la tontine est ouverte et que le wallet a suffisalent de fond
    | to == pay      = traceIfFalse "Vous n'avez pas les droits de payer le beneficiaire!" verifierTontinePayable  -- Verifier que la tontine est fermee et que c'est le president qui a signe la transaction
    | otherwise      = traceIfFalse "Error 404. Action inconnue!" False

    where
        infoTransaction =  scriptContextTxInfo ctx
        --signatories     =  txInfoSignatories infoTransaction 
        --maybeDatumHash  =  findDatumHash mydatum infoTransaction
        --entreesTransaction = txInfoInputs infoTransaction
        verifiertontineOuvrable    =  datumOt == close &&  signatureVerifcationOpenClose
        verifierTontineFermable    =  datumOt == open &&  signatureVerifcationOpenClose
        verifierTontineTontinable  = datumOt == open && montantAcceptable && estMemmbre
        verifierTontinePayable     = datumOt == close && (length signatairesPay >= 1)

        signatairesOpenClose = filter (txSignedBy  infoTransaction) (openClosePubKeyHash pd)
        signatureVerifcationOpenClose = length signatairesOpenClose >= 1

        signatairesPay = filter (txSignedBy  infoTransaction) (payPubKeyHash pd)

        txininfos = txInfoInputs infoTransaction -- List of TxInInfo
        --Just inputBeingValidated = findOwnInput ctx -- retourne le utxo qui a declenche ce script
        --filterUtxos = filter (inputBeingValidated /=) txininfos
        utxosResolved = map txInInfoResolved txininfos -- List of TxOut (retire les index des TxOut)

        total = foldr (\res t -> t + valueOf (txOutValue res) adaSymbol adaToken) 0  utxosResolved

        fees = valueOf (txInfoFee infoTransaction) adaSymbol adaToken
        montantAcceptable = total >=  (fees  +  minimumAmount pd)

        estMemmbre = elem membreid (membres pd)


{-# INLINABLE  mkWrappedParameterizedtontineValidator #-}
mkWrappedParameterizedtontineValidator :: ParamsData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedtontineValidator = wrapValidator . mkValidatorTontinard

validator :: ParamsData -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedtontineValidator ||]) `applyCode` liftCode params)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: ParamsData -> IO ()
saveVal = writeValidatorToFile "/home/nkalla/Documents/tontine.plutus" . validator

savaDataToFile :: PlutusTx.IsData.Class.ToData a => FilePath -> a -> IO ()
savaDataToFile = writeDataToFile 