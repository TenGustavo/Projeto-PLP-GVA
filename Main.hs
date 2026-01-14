{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist, renameFile, removeFile)
import Text.Read (readMaybe)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import GHC.Generics (Generic)

-- =========================
-- Tipos básicos (simples)
-- =========================

data Situacao = Cursando | Concluida | Trancada | Planejada
  deriving (Show, Read, Eq, Generic)

data TipoAtividade = Tarefa | Prova | Trabalho | Projeto | Leitura | OutroA
  deriving (Show, Read, Eq, Generic)

data StatusAtividade = Pendente | EmAndamento | Concluido
  deriving (Show, Read, Eq, Generic)

data TipoBloco = Aula | Compromisso
  deriving (Show, Read, Eq, Generic)

-- =========================
-- Registros principais
-- =========================

data Disciplina = Disciplina
  { dId          :: Int
  , dCodigo      :: String
  , dNome        :: String
  , dProfessor   :: String
  , dCargaHoraria:: Int
  , dSituacao    :: Situacao
  , dSemestre    :: String          -- ex: "2025.2" (texto simples)
  , dNotaFinal   :: Maybe Double    -- Ni (para IRA)
  , dPeriodo     :: Maybe Int       -- semestre em que foi cursada (Pi antes do min(6,...))
  } deriving (Show, Read, Eq, Generic)

data Atividade = Atividade
  { aId          :: Int
  , aDisciplinaId:: Int
  , aTitulo      :: String
  , aDescricao   :: String
  , aTipo        :: TipoAtividade
  , aStatus      :: StatusAtividade
  , aPrazo       :: String          -- manter como String para simplicidade (ex: "2026-01-13 23:59")
  } deriving (Show, Read, Eq, Generic)

data BlocoHorario = BlocoHorario
  { bId          :: Int
  , bDia         :: Int             -- 0..6
  , bInicioMin   :: Int             -- minutos do dia (0..1439)
  , bFimMin      :: Int
  , bTipo        :: TipoBloco
  , bDisciplinaId:: Maybe Int
  , bRotulo      :: String
  } deriving (Show, Read, Eq, Generic)

data Preferencias = Preferencias
  { pAntecedenciaHoras :: Int        -- usado para alertas simples (não temporal real aqui)
  } deriving (Show, Read, Eq, Generic)

data Estado = Estado
  { nextDiscId   :: Int
  , nextAtvId    :: Int
  , nextBlocoId  :: Int
  , disciplinas  :: [Disciplina]
  , atividades   :: [Atividade]
  , blocos       :: [BlocoHorario]
  , prefs        :: Preferencias
  } deriving (Show, Read, Eq, Generic)

estadoInicial :: Estado
estadoInicial = Estado
  { nextDiscId  = 1
  , nextAtvId   = 1
  , nextBlocoId = 1
  , disciplinas = []
  , atividades  = []
  , blocos      = []
  , prefs       = Preferencias { pAntecedenciaHoras = 48 }
  }

dbFile :: FilePath
dbFile = "gva.db"

tmpFile :: FilePath
tmpFile = "gva.tmp"

-- =========================
-- Persistência (Read/Show)
-- =========================

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
          putStrLn "Aviso: arquivo gva.db corrompido ou inválido. Iniciando estado vazio."
          pure estadoInicial

salvar :: Estado -> IO ()
salvar st = do
  writeFile tmpFile (show st)
  -- renameFile pode falhar se existir em alguns ambientes, então removemos antes
  existe <- doesFileExist dbFile
  if existe then removeFile dbFile else pure ()
  renameFile tmpFile dbFile
