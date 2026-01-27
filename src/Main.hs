module Main where

import Types
import Persistence (carregar, salvar)
import Disciplinas (moduloDisciplinas)
import Atividades (moduloAtividades)
import Horarios (moduloHorarios)

main :: IO ()
main = do
  st <- carregar
  loopPrincipal st

loopPrincipal :: Estado -> IO ()
loopPrincipal st = do
  putStrLn ""
  putStrLn "=== Gerenciador de Vida Acadêmica (GVA) - CLI ==="
  putStrLn "(Persistência: gva.db)"
  mostrarAlertasSimples st

  op <- menu "Menu Principal"
        [ "Sair"
        , "Disciplinas"
        , "Atividades/Trabalhos"
        , "Agenda (não implementada detalhadamente)"
        , "Desempenho (IRA UFCG)"
        , "Notificações (configurar antecedência)"
        , "Horários (com bloqueio de choque)"
        ]

  case op of
    0 -> do
      salvar st
      putStrLn "Estado salvo. Encerrando."
    1 -> moduloDisciplinas st >>= loopPrincipal
    2 -> moduloAtividades st >>= loopPrincipal
    3 -> do
      putStrLn "Agenda: para manter simples, ficou como melhoria futura."
      loopPrincipal st
    4 -> do
      putStrLn ""
      putStrLn "=== Desempenho: IRA UFCG (Individual) ==="
      let ira = iraIndividual (disciplinas st)
      putStrLn ("IRA (escala ~0..10): " ++ show ira)
      putStrLn ("IRA x 1000 (formato comum): " ++ show (ira * 1000))
      putStrLn "Obs: só entra no IRA quem tem Nota Final (Ni) e Período (Pi)."
      loopPrincipal st
    5 -> do
      st' <- configurarNotificacoes st
      loopPrincipal st'
    6 -> moduloHorarios st >>= loopPrincipal
    _ -> putStrLn "Opção inválida." >> loopPrincipal st

mostrarAlertasSimples :: Estado -> IO ()
mostrarAlertasSimples st = do
  let pendentes = [ a | a <- atividades st, aStatus a /= Concluido ]
  if null pendentes
    then pure ()
    else do
      putStrLn ""
      putStrLn "ALERTAS (simples): atividades não concluídas:"
      mapM_ (\a -> putStrLn ("- [" ++ show (aStatus a) ++ "] "
                             ++ aTitulo a ++ " (prazo: " ++ aPrazo a ++ ")"))
            (take 5 pendentes)

configurarNotificacoes :: Estado -> IO Estado
configurarNotificacoes st = do
  putStrLn ""
  putStrLn ("Antecedência atual (horas): " ++ show (pAntecedenciaHoras (prefs st)))
  h <- promptInt "Nova antecedência em horas (ex: 48): "
  let st' = st { prefs = (prefs st) { pAntecedenciaHoras = h } }
  salvar st'
  putStrLn "Preferência salva."
  pure st'
