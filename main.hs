import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Exception (catch, IOException)
import System.IO (hFlush, stdout)
import Data.List (isInfixOf, foldl')

type Inventario = Map.Map String Item
type ResultadoOperacao = (Inventario, LogEntry)

data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read)

data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)
  
-- Funcoes puras
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time item inv
  | Map.member (itemID item) inv = Left $ "Erro: Item com ID '" ++ itemID item ++ "' ja existe"
  | quantidade item <= 0 = Left "Erro: Quantidade deve ser maior que zero"
  | null (itemID item) || null (nome item) || null (categoria item) = Left "Erro: ID, nome e categoria nao podem ser vazios"
  | otherwise = Right (novoInv, logEntry)
  where
    novoInv = Map.insert (itemID item) item inv
    detalhesMsg = "Adicionado item ID: " ++ itemID item ++ ", Nome: " ++ nome item ++
                  ", Qtd: " ++ show (quantidade item) ++ ", Categoria: " ++ categoria item
    logEntry = LogEntry time Add detalhesMsg Sucesso

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time iid qtd inv
  | null iid = Left "Erro: ID nao pode ser vazio"
  | not (Map.member iid inv) = Left $ "Erro: Item ID: " ++ iid ++ " nao encontrado"
  | qtd <= 0 = Left "Erro: Quantidade deve ser maior que zero"
  | qtdAtual < qtd = Left $ "Erro: Item ID: " ++ iid ++ " - Estoque insuficiente. Disponivel: " ++ show (quantidade itemAtual) ++ ", Solicitado: " ++ show qtd
  | novaQtd == 0 = Right (Map.delete iid inv, criarLog "Item removido completamente")
  | otherwise = Right (Map.insert iid itemAtualizado inv, criarLog $ "Restante: " ++ show novaQtd)
  where
    Just itemAtual = Map.lookup iid inv
    qtdAtual = quantidade itemAtual
    novaQtd = qtdAtual - qtd
    itemAtualizado = itemAtual { quantidade = novaQtd }
    criarLog msg = LogEntry time Remove ("Removido " ++ show qtd ++ " de ID: " ++ iid ++ ". " ++ msg) Sucesso

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time iid novaQtd inv
  | null iid = Left "Erro: ID nao pode ser vazio"
  | not (Map.member iid inv) = Left $ "Erro: Item com ID '" ++ iid ++ "' nao encontrado"
  | novaQtd < 0 = Left "Erro: Quantidade nao pode ser negativa"
  | novaQtd == 0 = Right (Map.delete iid inv, criarLog "Estoque 0, item removido")
  | otherwise = Right (invAtualizado, logEntry)
  where
    Just itemAtual = Map.lookup iid inv
    itemAtualizado = itemAtual { quantidade = novaQtd }
    invAtualizado = Map.insert iid itemAtualizado inv
    detalhesMsg = "Atualizado item ID: " ++ iid ++ ", Nova quantidade: " ++ show novaQtd
    logEntry = LogEntry time Update detalhesMsg Sucesso
    criarLog msg = LogEntry time Update ("Atualizado quantidade " ++ show novaQtd ++ " de ID: " ++ iid ++ ". " ++ msg) Sucesso

queryFail :: UTCTime -> String -> AcaoLog -> LogEntry
queryFail time msg acao = LogEntry time acao msg (Falha msg)
-- Funcoes de Relatorio
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid = filter (isInfixOf ("ID: " ++ iid) . detalhes)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\e -> case status e of Falha _ -> True; _ -> False)

extrairID :: String -> Maybe String
extrairID s = case dropWhile (/= "ID:") (words s) of
  ("ID:" : idVal : _) -> Just (filter (\c -> c /= ',' && c /= '.') idVal)
  _ -> Nothing

formatarLog :: LogEntry -> String
formatarLog e = show (timestamp e) ++ " | " ++ show (acao e) ++ " | " ++ detalhes e

formatarItem :: Item -> String
formatarItem i = "ID: " ++ itemID i ++ " | Nome: " ++ nome i ++ 
                 " | Qtd: " ++ show (quantidade i) ++ " | Categoria: " ++ categoria i

-- Calcula o item com mais operacoes (movimentacoes), tratando empates
itemMaisMovimentado :: [LogEntry] -> Maybe [(String, Int)]
itemMaisMovimentado logs =
  case logsComID of
    [] -> Nothing
    _ -> let maxCount = maximum (Map.elems contadores)
             empatados = filter (\(_, count) -> count == maxCount) (Map.toList contadores)
         in Just empatados
  where
    logsComID = [(iid, e) | e <- logs, Just iid <- [extrairID (detalhes e)]]
    contadores = foldl' (\m (iid, _) -> Map.insertWith (+) iid 1 m) Map.empty logsComID

-- Obtem todos os IDs unicos dos logs
todosIDs :: [LogEntry] -> [String]
todosIDs logs = Map.keys $ Map.fromList [(iid, ()) | e <- logs, Just iid <- [extrairID (detalhes e)]]

-- Relatorio completo: erros, item mais movimentado e historico de todos os itens
gerarRelatorio :: [LogEntry] -> String
gerarRelatorio logs
  | null logs = "Nenhuma operacao registrada"
  | otherwise = unlines $ concat [
      [ "=== RELATORIO COMPLETO DO INVENTARIO ==="
      , "Total de operacoes: " ++ show (length logs)
      , ""
      ]
    , secaoErros
    , [ "" ]
    , secaoItemMaisMovimentado
    , [ ""
      , "=== HISTORICO POR ITEM ==="
      , ""
      ]
    , historicoPorTodosItens
    ]
  where
    erros = logsDeErro logs
    secaoErros = if null erros
      then ["=== LOGS DE ERRO ===", "Nenhum erro registrado"]
      else [ "=== LOGS DE ERRO ==="
           , "Total de erros: " ++ show (length erros)
           , ""
           ] ++ map formatarLog erros

    secaoItemMaisMovimentado = case itemMaisMovimentado logs of
      Nothing -> ["=== ITEM MAIS MOVIMENTADO ===", "Nenhum item encontrado"]
      Just [] -> ["=== ITEM MAIS MOVIMENTADO ===", "Nenhum item encontrado"]
      Just [(iid, count)] -> 
        [ "=== ITEM MAIS MOVIMENTADO ==="
        , "ID: " ++ iid
        , "Total de movimentacoes: " ++ show count
        ]
      Just itens -> 
        [ "=== ITENS MAIS MOVIMENTADOS (EMPATE) ===" ]
        ++ map (\(iid, count) -> "ID: " ++ iid ++ " - " ++ show count ++ " movimentacoes") itens

    ids = todosIDs logs
    historicoPorTodosItens = if null ids
      then ["Nenhum item registrado"]
      else concatMap gerarSecaoItem ids

    gerarSecaoItem iid =
      let historico = historicoPorItem iid logs
      in [ "--- Item ID: " ++ iid ++ " ---"
         , "Total de operacoes: " ++ show (length historico)
         , ""
         ] ++ map formatarLog historico ++ [""]

-- Persistencia
salvarInventario :: Inventario -> IO ()
carregarInventario :: IO Inventario
registrarLog :: LogEntry -> IO ()
carregarLogs :: IO [LogEntry]

salvarInventario inventario = writeFile "Inventario.dat" (show inventario)

carregarInventario =
  catch
    (do
        conteudo <- readFile "Inventario.dat"
        let inv = read conteudo
        inv `seq` return inv)
    (\(_ :: IOException) -> return Map.empty)

registrarLog logEntry = appendFile "Auditoria.log" (show logEntry ++ "\n")

carregarLogs =
  catch
    (do
        conteudo <- readFile "Auditoria.log"
        let logs = map read (lines conteudo)
        length logs `seq` return logs)
    (\(_ :: IOException) -> return [])
--Funcao de suporte
splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitOn delim rest

-- Loop principal com logs de falha automaticos
mainLoop :: Inventario -> IO ()
mainLoop inv = do
  putStrLn "\nComandos disponiveis:"
  putStrLn " add,<id>,<nome>,<quantidade>,<categoria>"
  putStrLn " remove,<id>,<quantidade>"
  putStrLn " update,<id>,<nova_quantidade>"
  putStrLn " list"
  putStrLn " report"
  putStrLn " exit"
  putStr "> "
  hFlush stdout
  comando <- getLine
  let partes = splitOn ',' comando
  case partes of
    ["add", iid, nm, qtd, cat] -> do
      case reads qtd :: [(Int, String)] of
        [(n, "")] -> do
          time <- getCurrentTime
          let item = Item iid nm n cat
          case addItem time item inv of
            Left err -> do
              let failLog = queryFail time err Add
              registrarLog failLog
              putStrLn err
              mainLoop inv
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              registrarLog logEntry
              putStrLn "Item adicionado com sucesso!"
              mainLoop novoInv
        _ -> do
          time <- getCurrentTime
          let failLog = queryFail time "Erro: quantidade invalida" Add
          registrarLog failLog
          putStrLn "Erro: quantidade deve ser um numero inteiro."
          mainLoop inv

    ["remove", iid, qtd] -> do
      case reads qtd :: [(Int, String)] of
        [(n, "")] -> do
          time <- getCurrentTime
          case removeItem time iid n inv of
            Left err -> do
              let failLog = queryFail time err Remove
              registrarLog failLog
              putStrLn err
              mainLoop inv
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              registrarLog logEntry
              putStrLn "Quantidade removida com sucesso!"
              mainLoop novoInv
        _ -> do
          time <- getCurrentTime
          let failLog = queryFail time "Erro: quantidade invalida" Remove
          registrarLog failLog
          putStrLn "Erro: quantidade deve ser um numero inteiro."
          mainLoop inv

    ["update", iid, qtd] -> do
      case reads qtd :: [(Int, String)] of
        [(n, "")] -> do
          time <- getCurrentTime
          case updateQty time iid n inv of
            Left err -> do
              let failLog = queryFail time err Update
              registrarLog failLog
              putStrLn err
              mainLoop inv
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              registrarLog logEntry
              putStrLn "Item atualizado!"
              mainLoop novoInv
        _ -> do
          time <- getCurrentTime
          let failLog = queryFail time "Erro: nova quantidade invalida" Update
          registrarLog failLog
          putStrLn "Erro: nova quantidade deve ser um numero inteiro."
          mainLoop inv

    ["list"] -> do
      if Map.null inv
        then putStrLn "\n=== Inventario Atual ===\nInventario vazio."
        else do
          putStrLn "\n=== Inventario Atual ==="
          mapM_ (putStrLn . formatarItem) (Map.elems inv)
      mainLoop inv



    ["report"] -> do
      logs <- carregarLogs
      putStrLn $ gerarRelatorio logs
      mainLoop inv

    ["exit"] -> do
      putStrLn "Saindo e salvando..."
      salvarInventario inv
      putStrLn "Ate logo!"

    _ -> do
      time <- getCurrentTime
      let failLog = queryFail time ("Comando invalido: " ++ comando) QueryFail
      registrarLog failLog
      putStrLn "Comando invalido! Use virgulas para separar os campos."
      mainLoop inv
main :: IO ()
main = do
  inventario <- carregarInventario
  _ <- carregarLogs
  putStrLn "Sistema iniciado com sucesso!"
  mainLoop inventario
