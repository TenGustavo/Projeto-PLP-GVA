module Horarios
  ( moduloHorarios
  ) where

import Data.List (find, sortOn)
import Data.Maybe (isJust)
import Types
import Persistence (salvar)
import Disciplinas (listarDisciplinas)

moduloHorarios :: Estado -> IO Estado
moduloHorarios st = do
  op <- menu "Horários"
        [ "Voltar"
        , "Adicionar bloco (impede choque)"
        , "Remover bloco"
        , "Listar horário semanal"
        ]
  case op of
    0 -> pure st
    1 -> adicionarBloco st
    2 -> removerBloco st
    3 -> listarBlocos st >> pure st
    _ -> putStrLn "Opção inválida." >> pure st

adicionarBloco :: Estado -> IO Estado
adicionarBloco st = do
  d <- promptInt "Dia (0=Dom,1=Seg,...,6=Sab): "
  iniS <- prompt "Início (HH:MM): "
  fimS <- prompt "Fim (HH:MM): "
  case (parseHoraMin iniS, parseHoraMin fimS) of
    (Just ini, Just fim) | ini < fim -> do
      putStrLn "Tipo: 1) Aula  2) Compromisso"
      tOpt <- promptInt "Escolha: "
      let tp = if tOpt == 1 then Aula else Compromisso

      didM <- if tp == Aula
                then do
                  listarDisciplinas st
                  did <- promptInt "ID da disciplina (ou 0 para não vincular): "
                  pure (if did == 0 then Nothing else Just did)
                else pure Nothing

      rot <- prompt "Rótulo (ex: 'LP', 'Treino', etc.): "

      let novo = BlocoHorario (nextBlocoId st) d ini fim tp didM rot
          conflitos = filter (conflita novo) (blocos st)

      if not (null conflitos)
        then do
          putStrLn "Conflito detectado. Não foi possível inserir."
          putStrLn "Conflita com:"
          mapM_ printB conflitos
          pure st
        else do
          let st' = st { nextBlocoId = nextBlocoId st + 1
                       , blocos      = novo : blocos st
                       }
          salvar st'
          putStrLn "Bloco inserido e salvo."
          pure st'
    _ -> putStrLn "Horário inválido (use HH:MM e início < fim)." >> pure st
  where
    printB b =
      putStrLn ("- #" ++ show (bId b) ++ " "
                ++ diaNome (bDia b) ++ " "
                ++ fmtHora (bInicioMin b) ++ "-" ++ fmtHora (bFimMin b)
                ++ " (" ++ show (bTipo b) ++ ") "
                ++ bRotulo b)

removerBloco :: Estado -> IO Estado
removerBloco st = do
  listarBlocos st
  i <- promptInt "ID do bloco para remover: "
  if isJust (find (\b -> bId b == i) (blocos st))
    then do
      let bs' = filter (\b -> bId b /= i) (blocos st)
          st' = st { blocos = bs' }
      salvar st'
      putStrLn "Bloco removido e salvo."
      pure st'
    else putStrLn "Bloco não encontrado." >> pure st

listarBlocos :: Estado -> IO ()
listarBlocos st = do
  putStrLn ""
  putStrLn "Horário semanal:"
  let bs = sortOn (\b -> (bDia b, bInicioMin b)) (blocos st)
  if null bs
    then putStrLn "(vazio)"
    else mapM_ printB bs
  where
    printB b =
      putStrLn $ unwords
        [ "#", show (bId b)
        , "|", diaNome (bDia b)
        , fmtHora (bInicioMin b) ++ "-" ++ fmtHora (bFimMin b)
        , "|", show (bTipo b)
        , "|", "Disc=", maybe "-" show (bDisciplinaId b)
        , "|", bRotulo b
        ]