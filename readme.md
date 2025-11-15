# Sistema de Inventário em Haskell – RA2  
Pontifícia Universidade Católica do Paraná – PUCPR  
Curso: **Ciência da Computação**  
Disciplina: **Programação Funcional**  
Professor: **Frank Coelho de Alcantara**

---

## Integrantes do Grupo 

| Nome | GitHub |
|------|--------|
| Bernardo Czizyk | https://github.com/BBernardoC |
| Gabriel Baú | https://github.com/ogabrielbau |
| Gregory Keune | https://github.com/gregorykeune |
| Mateus Filipe | https://github.com/MateusMonfort |
---

## Link para Execução Online
GDB Online: https://onlinegdb.com/VWnmMKrUlA

## Especificação Técnica do Sistema

# Especificação Técnica do Sistema

## 1. Arquitetura do Sistema

O sistema implementa um **gerenciador de inventário funcional em Haskell**, seguindo rigorosamente:

- Separação entre **lógica pura** (funções sem IO)
- Lógica impura restrita ao **main** e **mainLoop**
- Persistência de estado em arquivo (`Inventario.dat`)
- Auditoria completa de operações em `Auditoria.log`
- Execução totalmente interativa via terminal
- Serialização automática usando `deriving (Show, Read)`

Todo o fluxo segue o que foi solicitado no enunciado da Atividade Avaliativa RA2.

---

# 2. Estrutura de Dados Conforme Especificação

Abaixo estão os tipos de dados definidos no sistema, seguindo a especificação do professor:

```haskell
-- Item do inventário
data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read)

-- Estrutura principal do inventário
type Inventario = Map.Map String Item

-- Tipos de ação para auditoria
data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read, Eq)

-- Status das operações
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

-- Entrada completa do log
data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read)

-- Resultado das operações
type ResultadoOperacao = (Inventario, LogEntry)
```

---

### 3. Funções de Lógica Pura Implementadas

#### Operações Principais (puras)

**addItem**  
Adiciona um item ao inventário. Valida ID duplicado, quantidade e campos obrigatórios.

**removeItem**  
Remove quantidade do item. Remove completamente caso a quantidade chegue a zero.

**updateQty**  
Atualiza a quantidade do item. Se a nova quantidade for igual a 0, o item é removido.

**validaQuantidade**  
Valida que as quantidades recebidas sejam positivas.

---

#### Funções de Análise e Relatório

**logsDeErro**  
Filtra apenas as entradas de log cujo status é `Falha`.

**historicoPorItem**  
Extrai o histórico completo de operações relacionado a um ID específico.

**itemMaisMovimentado**  
Determina qual item teve mais movimentações no sistema (tratando empates).

**gerarRelatorio**  
Gera o relatório completo exigido no enunciado, contendo:
- Total de operações
- Logs de erro
- Item mais movimentado
- Histórico por item

---

#### Forma Canônica Exigida

As funções seguem a assinatura funcional:

Either String ResultadoOperacao


Onde:

type ResultadoOperacao = (Inventario, LogEntry)





