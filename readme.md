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

# 1. Arquitetura do Sistema

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

# 3. Funções de Lógica Pura Implementadas

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

# 4. Sistema de Persistência

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


# 5. Comandos do Sistema

| Comando | Sintaxe | Descrição |
|---------|---------|-----------|
| **add** | `add,<id>,<nome>,<quantidade>,<categoria>` | Adiciona item ao inventário |
| **remove** | `remove,<id>,<quantidade>` | Remove quantidade do item |
| **update** | `update,<id>,<nova_quantidade>` | Atualiza o estoque do item |
| **list** | `list` | Lista todos os itens do inventário |
| **report** | `report` | Gera o relatório completo do sistema |
| **exit** | `exit` | Encerra o sistema |


---


# 6. Comportamento Especial

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
```
Isso permite:

    Serialização automática em arquivos

    Desserialização segura

    Persistência correta do sistema

---

# 7. Dados Mínimos para Teste

Para garantir o funcionamento correto das funções de relatório, auditoria e persistência, foram adicionados **pelo menos 10 itens distintos** ao inventário, conforme solicitado na especificação da atividade RA2.

#### Exemplo de inserções mínimas:

    add,001,Teclado,15,Informatica
    add,002,Mouse,20,Informatica
    add,003,Monitor,8,Informatica
    add,004,Notebook,5,Eletronicos
    add,005,Impressora,3,Escritorio
    add,006,Cadeira,12,Movel
    add,007,Mesa,6,Movel
    add,008,Tablet,10,Eletronicos
    add,009,Smartphone,25,Eletronicos
    add,010,Fone,30,Acessorios

Esses dados foram utilizados para validar:

- Persistência entre execuções  
- Geração do relatório completo  
- Funcionamento do cálculo de item mais movimentado  
- Registros de auditoria  
- Tratamento de erros e entradas inválidas  

---

# 8. Documentação dos Cenários de Teste

---

#### 🧪 Cenário 1 — Persistência de Estado

**Procedimento:**

1. Executar o sistema sem arquivos existentes.
2. Inserir os seguintes itens:

add,011,ItemA,10,CatA
add,012,ItemB,5,CatB
add,013,ItemC,8,CatC


3. Encerrar o programa com:

exit


4. Reiniciar o sistema.
5. Verificar o inventário com:

list


**Resultado esperado:**  
Os três itens devem aparecer corretamente após a reinicialização, comprovando que:

- O arquivo `Inventario.dat` foi gerado.
- Os dados foram serializados usando `Show`/`Read`.
- A função `carregarInventario` está funcionando corretamente.
- Não houve crashes por ausência de arquivos.

---

#### 🧪 Cenário 2 — Estoque Insuficiente

**Procedimento:**

1. Adicionar um item:

add,014,Teclado,10,Informatica


2. Tentar remover uma quantidade maior do que a disponível:

remove,014,15


3. Em seguida, listar o inventário:

list


**Resultado esperado:**

- O sistema exibe uma mensagem clara de erro informando estoque insuficiente.
- A quantidade permanece **10**.
- Uma entrada com `Falha` é registrada em `Auditoria.log`.

---

#### 🧪 Cenário 3 — Geração do Relatório

**Procedimento:**

1. Executar o Cenário 2 (para gerar erro).  
2. Pedir o relatório com:

report


**Resultado esperado:**

O relatório deve exibir:

- Quantidade total de operações
- Lista de erros registrados
- Item mais movimentado
- Histórico completo por item

Confirmando assim que:

- `itemMaisMovimentado` funciona corretamente  
- `logsDeErro` identifica todas as falhas  
- `historicoPorItem` lista todas as operações relevantes  
- `gerarRelatorio` consolida tudo no formato esperado

---

# 9. Instruções de Execução

#### Online GDB

Para executar o sistema diretamente no navegador:

1. Acesse o link do projeto no Online GDB.
2. Clique no botão **Run**.
3. Utilize o terminal integrado para enviar os comandos no formato:

    add,<id>,<nome>,<quantidade>,<categoria>
    remove,<id>,<quantidade>
    update,<id>,<nova_quantidade>
    list
    report
    exit
    
O sistema funcionará exatamente como em um ambiente local, incluindo persistência e geração de logs.

---

# 10. Conclusão

O sistema atende **integralmente** aos requisitos da Atividade Avaliativa RA2, demonstrando:

- Domínio dos conceitos de **programação funcional em Haskell**  
- **Separação correta** entre lógica pura e operações de I/O  
- **Persistência robusta** com arquivos `.dat` e `.log`  
- **Auditoria completa**, incluindo falhas e sucessos  
- **Tratamento adequado de erros** e entradas inválidas  
- **Geração completa de relatórios** conforme especificado  
- Conformidade total com as instruções fornecidas pelo professor  

O projeto está coerente, funcional, modular e pronto para avaliação.


