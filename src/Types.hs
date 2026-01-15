{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Situacao(..)
  , TipoAtividade(..)
  , StatusAtividade(..)
  , TipoBloco(..)
  , Disciplina(..)
  , Atividade(..)
  , BlocoHorario(..)
  , Preferencias(..)
  , Estado(..)
  , estadoInicial
  , prompt, promptInt, promptDouble, menu
  , diaNome, fmtHora, parseHoraMin
  , conflita
  , iraIndividual
  ) where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Maybe (isJust)
import GHC.Generics (Generic)

-- Tipos básicos

data Situacao = Cursando | Concluida | Trancada | Planejada
  deriving (Show, Read, Eq, Generic)

data TipoAtividade = Tarefa | Prova | Trabalho | Projeto | Leitura | OutroA
  deriving (Show, Read, Eq, Generic)

data StatusAtividade = Pendente | EmAndamento | Concluido
  deriving (Show, Read, Eq, Generic)

data TipoBloco = Aula | Compromisso
  deriving (Show, Read, Eq, Generic)

-- =========================
-- Registros
-- =========================

data Disciplina = Disciplina
  { dId           :: Int
  , dCodigo       :: String
  , dNome         :: String
  , dProfessor    :: String
  , dCargaHoraria :: Int
  , dSituacao     :: Situacao
  , dSemestre     :: String
  , dNotaFinal    :: Maybe Double  -- Ni
  , dPeriodo      :: Maybe Int     -- período cursado (Pi antes do min(6,Pi))
  } deriving (Show, Read, Eq, Generic)

data Atividade = Atividade
  { aId           :: Int
  , aDisciplinaId :: Int
  , aTitulo       :: String
  , aDescricao    :: String
  , aTipo         :: TipoAtividade
  , aStatus       :: StatusAtividade
  , aPrazo        :: String        -- texto por simplicidade
  } deriving (Show, Read, Eq, Generic)

data BlocoHorario = BlocoHorario
  { bId           :: Int
  , bDia          :: Int           -- 0..6
  , bInicioMin    :: Int           -- minutos do dia
  , bFimMin       :: Int
  , bTipo         :: TipoBloco
  , bDisciplinaId :: Maybe Int
  , bRotulo       :: String
  } deriving (Show, Read, Eq, Generic)

data Preferencias = Preferencias
  { pAntecedenciaHoras :: Int
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
  { nextDiscId   = 1
  , nextAtvId    = 1
  , nextBlocoId  = 1
  , disciplinas  = []
  , atividades   = []
  , blocos       = []
  , prefs        = Preferencias { pAntecedenciaHoras = 48 }
  }

-- Helpers de CLI (simples)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

promptInt :: String -> IO Int
promptInt msg = do
  s <- prompt msg
  case readMaybe s of
    Just n  -> pure n
    Nothing -> putStrLn "Entrada inválida. Digite um número." >> promptInt msg

promptDouble :: String -> IO Double
promptDouble msg = do
  s <- prompt msg
  case readMaybe s of
    Just n  -> pure n
    Nothing -> putStrLn "Entrada inválida. Digite um número (ex: 8.5)." >> promptDouble msg

menu :: String -> [String] -> IO Int
menu titulo opcoes = do
  putStrLn ""
  putStrLn ("==== " ++ titulo ++ " ====")
  mapM_ putStrLn [ show i ++ ") " ++ o | (i,o) <- zip [0..] opcoes ]
  promptInt "Escolha: "

-- Horário: parsing/format

diaNome :: Int -> String
diaNome d = case d of
  0 -> "Dom"
  1 -> "Seg"
  2 -> "Ter"
  3 -> "Qua"
  4 -> "Qui"
  5 -> "Sex"
  6 -> "Sab"
  _ -> "?"

fmtHora :: Int -> String
fmtHora m =
  let h  = m `div` 60
      mi = m `mod` 60
      pad2 x = if x < 10 then '0':show x else show x
  in pad2 h ++ ":" ++ pad2 mi

parseHoraMin :: String -> Maybe Int
parseHoraMin s =
  case break (==':') s of
    (hh, ':' : mm) -> do
      h <- readMaybe hh
      m <- readMaybe mm
      if h >= 0 && h <= 23 && m >= 0 && m <= 59
        then Just (h*60 + m)
        else Nothing
    _ -> Nothing

-- Regra de conflito: iniNovo < fim && fimNovo > ini (mesmo dia)
conflita :: BlocoHorario -> BlocoHorario -> Bool
conflita novo existente =
  bDia novo == bDia existente &&
  bInicioMin novo < bFimMin existente &&
  bFimMin novo > bInicioMin existente

-- IRA (UFCG) - Individual

iraIndividual :: [Disciplina] -> Double
iraIndividual ds =
  let t = fromIntegral (sum [ dCargaHoraria d | d <- ds, dSituacao d == Trancada ])
      c = fromIntegral (sum [ dCargaHoraria d
                            | d <- ds
                            , dSituacao d == Trancada
                              || dSituacao d == Cursando
                              || dSituacao d == Concluida
                              || dSituacao d == Planejada
                            ])
      termos =
        [ (p * ch, p * ch * ni)
        | d <- ds
        , Just ni <- [dNotaFinal d]
        , Just per <- [dPeriodo d]
        , let p  = fromIntegral (min 6 per)
        , let ch = fromIntegral (dCargaHoraria d)
        ]
      den = sum [ w  | (w,_)  <- termos ]
      num = sum [ wn | (_,wn) <- termos ]
      mediaPond = if den == 0 then 0 else num / den
      fator = if c == 0 then 1 else 1 - (0.5 * t / c)
  in fator * mediaPond

