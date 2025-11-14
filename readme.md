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

### 1. Arquitetura Geral

O sistema implementa um gerenciador de inventário utilizando princípios de programação funcional em Haskell, com **separação rigorosa entre lógica pura e operações de I/O**, persistência em arquivos e auditoria completa de operações.

---

### 2. Estrutura do Projeto

```plaintext
/
├── Inventario.hs        # Código fonte principal
├── Inventario.dat       # Estado persistido do inventário
├── Auditoria.log        # Log de auditoria append-only
└── README.md
```

---

### 3. Tipos de Dados Conforme Especificação

O sistema define os seguintes tipos:

- `Item` (ID, nome, quantidade, categoria)
- `Inventario = Map String Item`
- `AcaoLog = AddItem | RemoveItem | UpdateItem | QueryFail`
- `StatusLog = Sucesso | Falha String`
- `LogEntry` (timestamp, ação, detalhes, status)

Todos derivam `Show` e `Read`, permitindo serialização e desserialização automática para os arquivos de persistência.

---

### 4. Funções de Lógica Pura Implementadas

#### Operações principais

- `addItem` – adiciona item ao inventário  
- `removeItem` – remove quantidade ou exclui item  
- `updateItem` – atualiza quantidade do item  
- `validaQuantidade` – valida quantidade positiva  

#### Funções de análise e relatório

- `historicoPorItem` – histórico por ID  
- `logsDeErro` – filtra somente falhas  
- `itemMaisMovimentado` – identifica itens mais movimentados  
- `gerarRelatorio` – gera relatório completo  

As funções seguem a assinatura:

Either String ResultadoOperacao

onde:

type ResultadoOperacao = (Inventario, LogEntry)

---



