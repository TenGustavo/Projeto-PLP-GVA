module Main where

import Types
import Persistence (carregar)
import Disciplinas (moduloDisciplinas)
import Atividades (moduloAtividades)
import Horarios (moduloHorarios)

-- | Função principal
main :: IO ()
main = do
  putStrLn "======================================="
  putStrLn "  GVA - Gerenciador de Vida Acadêmica"
  putStrLn "======================================="

  estadoInicial <- carregar
  loopPrincipal estadoInicial


-- | Loop principal do sistema
loopPrincipal :: Estado -> IO ()
loopPrincipal estado = do
  opcao <- menu "Menu Principal"
    [ "Sair"
    , "Disciplinas"
    , "Atividades"
    , "Horários"
    , "Calcular IRA"
    ]

  case opcao of
    0 -> sair
    1 -> moduloDisciplinas estado >>= loopPrincipal
    2 -> moduloAtividades estado >>= loopPrincipal
    3 -> moduloHorarios estado >>= loopPrincipal
    4 -> do
           mostrarIRA estado
           loopPrincipal estado
    _ -> do
           putStrLn "Opção inválida."
           loopPrincipal estado


-- | Exibe o IRA do aluno
mostrarIRA :: Estado -> IO ()
mostrarIRA estado = do
  let ira = iraIndividual (disciplinas estado)
  putStrLn ""
  putStrLn "===== IRA (Índice de Rendimento Acadêmico) ====="
  putStrLn ("IRA: " ++ show ira)
  putStrLn ("IRA x 1000: " ++ show (ira * 1000))
  putStrLn "==============================================="


-- | Encerra o programa
sair :: IO ()
sair = do
  putStrLn ""
  putStrLn "Encerrando o sistema..."
  putStrLn "Até logo!"

