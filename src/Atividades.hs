module Atividades
  ( moduloAtividades
  ) where

import Data.List (find)
import Data.Maybe (isJust)
import Types
import Persistence (salvar)
import Disciplinas (listarDisciplinas)

moduloAtividades :: Estado -> IO Estado
moduloAtividades st = do
  op <- menu "Atividades/Trabalhos"
        [ "Voltar"
        , "Criar atividade"
        , "Listar atividades"
        , "Atualizar status"
        , "Remover atividade"
        ]
  case op of
    0 -> pure st
    1 -> criarAtividade st
    2 -> listarAtividades st >> pure st
    3 -> atualizarStatusAtv st
    4 -> removerAtividade st
    _ -> putStrLn "Opção inválida." >> pure st

criarAtividade :: Estado -> IO Estado
criarAtividade st = do
  if null (disciplinas st)
    then putStrLn "Cadastre uma disciplina antes." >> pure st
    else do
      listarDisciplinas st
      did <- promptInt "ID da disciplina: "
      case find (\d -> dId d == did) (disciplinas st) of
        Nothing -> putStrLn "Disciplina inválida." >> pure st
        Just _  -> do
          titulo <- prompt "Título: "
          desc   <- prompt "Descrição: "
          putStrLn "Tipo: 1) Tarefa 2) Prova 3) Trabalho 4) Projeto 5) Leitura 6) Outro"
          tOpt <- promptInt "Escolha: "
          let tp = case tOpt of
                     1 -> Tarefa
                     2 -> Prova
                     3 -> Trabalho
                     4 -> Projeto
                     5 -> Leitura
                     _ -> OutroA
          prazo <- prompt "Prazo (texto, ex: 2026-01-13 23:59): "
          let a = Atividade (nextAtvId st) did titulo desc tp Pendente prazo
              st' = st { nextAtvId  = nextAtvId st + 1
                       , atividades = a : atividades st
                       }
          salvar st'
          putStrLn "Atividade criada e salva."
          pure st'

listarAtividades :: Estado -> IO ()
listarAtividades st = do
  putStrLn ""
  putStrLn "Atividades:"
  if null (atividades st)
    then putStrLn "(vazio)"
    else mapM_ printAtv (reverse (atividades st))
  where
    printAtv a = putStrLn $
      unwords [ "#", show (aId a)
              , "| Disc=", show (aDisciplinaId a)
              , "|", show (aStatus a)
              , "|", show (aTipo a)
              , "|", aTitulo a
              , "| Prazo:", aPrazo a
              ]

atualizarStatusAtv :: Estado -> IO Estado
atualizarStatusAtv st = do
  listarAtividades st
  i <- promptInt "ID da atividade: "
  case find (\a -> aId a == i) (atividades st) of
    Nothing -> putStrLn "Atividade não encontrada." >> pure st
    Just _  -> do
      putStrLn "Novo status: 1) Pendente 2) EmAndamento 3) Concluido"
      sOpt <- promptInt "Escolha: "
      let stt = case sOpt of
                  1 -> Pendente
                  2 -> EmAndamento
                  3 -> Concluido
                  _ -> Pendente
          as' = map (\a -> if aId a == i then a { aStatus = stt } else a) (atividades st)
          st' = st { atividades = as' }
      salvar st'
      putStrLn "Status atualizado e salvo."
      pure st'

removerAtividade :: Estado -> IO Estado
removerAtividade st = do
  listarAtividades st
  i <- promptInt "ID da atividade para remover: "
  if isJust (find (\a -> aId a == i) (atividades st))
    then do
      let as' = filter (\a -> aId a /= i) (atividades st)
          st' = st { atividades = as' }
      salvar st'
      putStrLn "Atividade removida e salva."
      pure st'
    else putStrLn "Atividade não encontrada." >> pure st