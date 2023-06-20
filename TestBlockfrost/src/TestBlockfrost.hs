{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TemplateHaskell #-}


module TestBlockfrost
  where

import Blockfrost.Client
import Blockfrost.Client.Types
import Blockfrost.API
import qualified Data.Text as T
import System.Process
import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))
--import Data.Text.Internal



funzione :: IO  [AddressUtxo]
funzione  = do
  print "test de fonctionnment"
  _ <- projectFromEnv
  let totextAddrExp = T.pack "addr_test1qrfvl09vznmcqth7mdrvhlvj8rwyx4x22k7kgs7st7e9cmgeswxsf9fae0wj4e3st46hung22dfgaqytuw78z5el86cqrxs0d8"
      something = Address totextAddrExp
  xs <- getAddressUtxos something
  --y <- getAddressInfo something
  print "++++++"
  return xs


-- cette fonction marche aussi tres bien, avec le node demarre
test :: IO ()
test = do
  _ <- projectFromEnv
  let someShellCommand = proc  "cardano-cli" ["query", "utxo", "--socket-path", "/home/nkalla/cardano-src/cardano-node/config_preprod/db/node.socket", "--testnet-magic", "1",
        "--address", "addr_test1qrfvl09vznmcqth7mdrvhlvj8rwyx4x22k7kgs7st7e9cmgeswxsf9fae0wj4e3st46hung22dfgaqytuw78z5el86cqrxs0d8"]
      shellCommand = shell "export CARDANO_NODE_SOCKET_PATH=/home/nkalla/cardano-src/cardano-node/config_preprod/db"
  (_,_,_,processHandle') <- createProcess shellCommand
  (_,_,_,processHandle1') <- createProcess someShellCommand
  _ <- waitForProcess processHandle'
  _ <- waitForProcess processHandle1'
  return ()

-- echo il mio modello, funziona benissimo, quando il node è up
testWithOtherFunction :: IO ()
testWithOtherFunction = do
  _ <- projectFromEnv
  let someShellCommand = proc  "cardano-cli" ["query", "tip", "--socket-path", "/home/nkalla/cardano-src/cardano-node/config_preprod/db/node.socket", "--testnet-magic", "1"]
      shellCommand = shell "export CARDANO_NODE_SOCKET_PATH=/home/nkalla/cardano-src/cardano-node/config_preprod/db"
  (_,_,_,processHandle) <- createProcess shellCommand
  (_,_,_,processHandle1) <- createProcess someShellCommand
  _ <- waitForProcess processHandle
  _ <- waitForProcess processHandle1
    -- Nothing -> do
    --   terminateProcess processHandle1
    --   print "le processus est termine"
    -- Just _ -> print "le processus est termine"
  return ()
  




testSendAdaTo :: String -> String ->  Int -> IO ()
testSendAdaTo addrExpediteur addrDestinataire montant = do
  -- je definis mon environnement
  --monEnv <- projectFromFile "pathToMonToken.txt"
  _ <- projectFromEnv
  -- conversion vers le type Text
  let totextAddrExp = T.pack addrExpediteur
      -- construction du type Address avec le constructeur Address
      toAddressExp = Address totextAddrExp  -- je pouvais aussi utiliser la fonction mkAddress en lui passant un type Text
      --toAddressDest = Address totextAddrDest
  -- j'interroge cardano pour avoir les infos sur mon wallet 
  (firstUtxo:_) <- getAddressUtxos toAddressExp
  -- je recupere le hash de la tx dont provient mon utxo info, et l'index
  let someTxHash = _addressUtxoTxHash firstUtxo -- le hash de la tx dont provient l'utxo
      someIndex = _addressUtxoOutputIndex firstUtxo
      idUtxoToUse = T.unpack (unTxHash someTxHash) ++ "#" ++ show someIndex
      toStringMontant = show montant
      utxoOut = addrDestinataire ++ "+" ++ toStringMontant
  --print firstUtxo
  print idUtxoToUse
  -- je construit les process : build , sign , submit
  let shellCommand = shell "export CARDANO_NODE_SOCKET_PATH=/home/nkalla/cardano-src/cardano-node/config_preprod/db/node.socket"

      shellTx = proc "cardano-cli" ["transaction", "build", "--socket-path", "/home/nkalla/cardano-src/cardano-node/config_preprod/db/node.socket",
          "--babbage-era", "--testnet-magic", "1", "--tx-in", idUtxoToUse, "--tx-out", utxoOut, "--change-address", 
          addrExpediteur, "--out-file", "./testInCode-tx.raw"]

      shellSign = proc "cardano-cli" ["transaction", "sign", "--signing-key-file", "/home/nkalla/monWallet/privateKey.skey", "--testnet-magic", "1",
        "--tx-body-file", "./testInCode-tx.raw", "--out-file", "./testInCode-tx.signed"]

      shellSubmit = proc "cardano-cli" ["transaction", "submit", "--tx-file", "./testInCode-tx.signed", "--testnet-magic", "1"]
  (_,_,_,processExport) <- createProcess shellCommand
  exitCodeExport <- waitForProcess processExport
  case exitCodeExport of
    ExitFailure _  -> print "le processus d'export est en cours !"
    ExitSuccess  -> do
      (_,_,_,processHandleTx) <- createProcess shellTx
      exitCodeTx <- waitForProcess processHandleTx
      case exitCodeTx of
        ExitFailure _ -> print "le processus de transaction est en cours !"
        ExitSuccess -> do
          (_,_,_,processHandleSign) <- createProcess shellSign
          exitCodeSign <- waitForProcess processHandleSign
          case exitCodeSign of
            ExitFailure _  -> print "la signature de la tx est en cours !"
            ExitSuccess  -> do
              (_,_,_,processHandleSubmit) <- createProcess shellSubmit
              exitCodeSubmit <- waitForProcess processHandleSubmit
              case exitCodeSubmit of 
                ExitFailure _  -> print "le submit est en cours !!"
                ExitSuccess-> do print "ok transacation reussite"






 


