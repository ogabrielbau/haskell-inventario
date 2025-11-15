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

### 4. Sistema de Persistência

#### Arquivos do Sistema

**Inventario.dat**  
Armazena o estado atual do inventário.  
É sobrescrito a cada operação bem-sucedida.

**Auditoria.log**  
Armazena todos os registros de auditoria, incluindo sucessos e falhas.  
Funciona em modo **append-only**.

---

#### Funções de I/O implementadas

- `salvarInventario`  
- `carregarInventario`  
- `registrarLog`  
- `carregarLogs`

Essas funções realizam toda a parte impura (IO) do sistema, mantendo a separação da lógica pura.

---

#### Tratamento de Exceções

A leitura utiliza `catch` para evitar crash na primeira execução, conforme solicitado na especificação:

```haskell
catch
  (do conteudo <- readFile "Inventario.dat"
      let inv = read conteudo
      inv `seq` return inv)
  (\(_ :: IOException) -> return Map.empty)
```

Isso garante que, se os arquivos não existirem, o sistema inicia com um inventário vazio sem falhar.


---


### 5. Comandos do Sistema

| Comando | Sintaxe | Descrição |
|---------|---------|-----------|
| **add** | `add,<id>,<nome>,<quantidade>,<categoria>` | Adiciona item ao inventário |
| **remove** | `remove,<id>,<quantidade>` | Remove quantidade do item |
| **update** | `update,<id>,<nova_quantidade>` | Atualiza o estoque do item |
| **list** | `list` | Lista todos os itens do inventário |
| **report** | `report` | Gera o relatório completo do sistema |
| **exit** | `exit` | Encerra o sistema |


---


### 6. Comportamento Especial

- Quando a quantidade chega a **zero** em `updateQty` ou `removeItem`, o item é automaticamente removido do inventário.  
- Toda operação — **sucesso ou falha** — gera uma `LogEntry`.  
- Itens sem nenhuma operação bem-sucedida **não aparecem no relatório final**, conforme definido no sistema.

---

### Evidências de Conformidade com a Rubrica

#### 1. Separação entre Lógica Pura e Impura

- **Funções puras**: `addItem`, `removeItem`, `updateQty`, `historicoPorItem`, `logsDeErro`, `itemMaisMovimentado`, `gerarRelatorio`.  
- **Funções impuras**: `main`, `mainLoop`, `salvarInventario`, `carregarInventario`, `registrarLog`, `carregarLogs`.

**Conforme exigido:** nenhuma função pura realiza operações de I/O.

---

#### 2. Persistência de Estado

- Leitura correta de `Inventario.dat` e `Auditoria.log` com uso de `catch`.  
- Gravação de `Inventario.dat` após operações bem-sucedidas.  
- Registro de **todas** as operações no arquivo `Auditoria.log`.  
- Sistema **não quebra** na primeira execução se os arquivos não existirem.

---

#### 3. Sistema de Auditoria

Cada entrada de auditoria (`LogEntry`) contém:

- **Timestamp** (`UTCTime`)  
- **Ação** (`Add`, `Remove`, `Update`, `QueryFail`)  
- **Mensagem detalhada**  
- **Status** (`Sucesso` ou `Falha String`)  

Atendendo exatamente ao solicitado na atividade RA2.

---

#### 4. Derivação de Show/Read

Todos os tipos necessários implementam:

```haskell
deriving (Show, Read)

Isso permite:

    Serialização automática em arquivos

    Desserialização segura

    Persistência correta do sistema
```






