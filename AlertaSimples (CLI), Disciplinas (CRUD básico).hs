-- =========================
-- Alertas simples (CLI)
-- =========================

mostrarAlertasSimples :: Estado -> IO ()
mostrarAlertasSimples st = do
  let pendentes = [ a | a <- atividades st, aStatus a /= Concluido ]
  if null pendentes
    then pure ()
    else do
      putStrLn ""
      putStrLn "ALERTAS (simples): atividades não concluídas:"
      mapM_ (\a -> putStrLn ("- [" ++ show (aStatus a) ++ "] " ++ aTitulo a ++ " (prazo: " ++ aPrazo a ++ ")")) (take 5 pendentes)

-- =========================
-- Disciplinas (CRUD básico)
-- =========================

moduloDisciplinas :: Estado -> IO Estado
moduloDisciplinas st = do
  op <- menu "Disciplinas"
        [ "Voltar"
        , "Cadastrar disciplina"
        , "Listar disciplinas"
        , "Remover disciplina"
        , "Editar situação"
        , "Registrar nota final e período (para IRA)"
        ]
  case op of
    0 -> pure st
    1 -> cadastrarDisc st
    2 -> listarDisc st >> pure st
    3 -> removerDisc st
    4 -> editarSituacao st
    5 -> registrarNotaPeriodo st
    _ -> putStrLn "Opção inválida." >> pure st

cadastrarDisc :: Estado -> IO Estado
cadastrarDisc st = do
  codigo <- prompt "Código: "
  nome   <- prompt "Nome: "
  prof   <- prompt "Professor: "
  ch     <- promptInt "Carga horária (inteiro): "
  sem    <- prompt "Semestre (ex: 2025.2): "

  putStrLn "Situação: 1) Cursando  2) Concluida  3) Trancada  4) Planejada"
  sOpt <- promptInt "Escolha: "
  let sit = case sOpt of
              1 -> Cursando
              2 -> Concluida
              3 -> Trancada
              4 -> Planejada
              _ -> Cursando

  let duplicado = isJust (find (\d -> dCodigo d == codigo) (disciplinas st))
  if duplicado
    then do
      putStrLn "Já existe disciplina com esse código. Operação cancelada (simplicidade)."
      pure st
    else do
      let d = Disciplina (nextDiscId st) codigo nome prof ch sit sem Nothing Nothing
          st' = st { nextDiscId = nextDiscId st + 1
                   , disciplinas = d : disciplinas st
                   }
      salvar st'
      putStrLn "Disciplina cadastrada e salva."
      pure st'

listarDisc :: Estado -> IO ()
listarDisc st = do
  putStrLn ""
  putStrLn "Disciplinas:"
  if null (disciplinas st)
    then putStrLn "(vazio)"
    else mapM_ printDisc (reverse (disciplinas st))
  where
    printDisc d =
      putStrLn $ unwords
        [ "#", show (dId d)
        , "-", dCodigo d ++ ":", dNome d
        , "|", "CH=" ++ show (dCargaHoraria d)
        , "|", show (dSituacao d)
        , "|", "Sem=" ++ dSemestre d
        , "|", "NotaFinal=" ++ maybe "-" show (dNotaFinal d)
        , "|", "Periodo=" ++ maybe "-" show (dPeriodo d)
        ]

removerDisc :: Estado -> IO Estado
removerDisc st = do
  i <- promptInt "ID da disciplina para remover: "
  let ds = disciplinas st
  case find (\d -> dId d == i) ds of
    Nothing -> putStrLn "Disciplina não encontrada." >> pure st
    Just _  -> do
      -- remove vínculos junto (mais simples)
      let ds' = filter (\d -> dId d /= i) ds
          as' = filter (\a -> aDisciplinaId a /= i) (atividades st)
          bs' = map desvincular (blocos st)
          desvincular b =
            case bDisciplinaId b of
              Just did | did == i -> b { bDisciplinaId = Nothing, bRotulo = bRotulo b ++ " (desvinculado)" }
              _                   -> b
          st' = st { disciplinas = ds', atividades = as', blocos = bs' }
      salvar st'
      putStrLn "Disciplina removida (atividades removidas; blocos desvinculados) e salvo."
      pure st'

editarSituacao :: Estado -> IO Estado
editarSituacao st = do
  i <- promptInt "ID da disciplina: "
  case find (\d -> dId d == i) (disciplinas st) of
    Nothing -> putStrLn "Disciplina não encontrada." >> pure st
    Just _  -> do
      putStrLn "Nova situação: 1) Cursando  2) Concluida  3) Trancada  4) Planejada"
      sOpt <- promptInt "Escolha: "
      let sit = case sOpt of
                  1 -> Cursando
                  2 -> Concluida
                  3 -> Trancada
                  4 -> Planejada
                  _ -> Cursando
          ds' = map (\d -> if dId d == i then d { dSituacao = sit } else d) (disciplinas st)
          st' = st { disciplinas = ds' }
      salvar st'
      putStrLn "Situação atualizada e salva."
      pure st'

registrarNotaPeriodo :: Estado -> IO Estado
registrarNotaPeriodo st = do
  i <- promptInt "ID da disciplina: "
  case find (\d -> dId d == i) (disciplinas st) of
    Nothing -> putStrLn "Disciplina não encontrada." >> pure st
    Just _  -> do
      nota <- promptDouble "Nota final (Ni): "
      per  <- promptInt "Período/semestre em que foi cursada (Pi): "
      let ds' = map (\d -> if dId d == i then d { dNotaFinal = Just nota, dPeriodo = Just per } else d) (disciplinas st)
          st' = st { disciplinas = ds' }
      salvar st'
      putStrLn "Nota e período registrados e salvos."
      pure st'