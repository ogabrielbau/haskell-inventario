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
