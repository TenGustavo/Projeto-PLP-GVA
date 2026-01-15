module Persistence
  ( carregar
  , salvar
  ) where

import System.Directory (doesFileExist, renameFile, removeFile)
import Text.Read (readMaybe)
import Types (Estado, estadoInicial)

dbFile :: FilePath
dbFile = "data/gva.db"

tmpFile :: FilePath
tmpFile = "data/gva.tmp"

carregar :: IO Estado
carregar = do
  existe <- doesFileExist dbFile
  if not existe
    then pure estadoInicial
    else do
      conteudo <- readFile dbFile
      case readMaybe conteudo of
        Just st -> pure st
        Nothing -> do
          putStrLn "Aviso: gva.db inválido/corrompido. Iniciando estado vazio."
          pure estadoInicial

salvar :: Estado -> IO ()
salvar st = do
  writeFile tmpFile (show st)
  existe <- doesFileExist dbFile
  if existe then removeFile dbFile else pure ()
  renameFile tmpFile dbFile