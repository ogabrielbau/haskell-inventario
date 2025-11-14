# Sistema de Inventário em Haskell – RA2  
Pontifícia Universidade Católica do Paraná – PUCPR  
Curso: **Ciência da Computação**  
Disciplina: **Programação Funcional**  
Professor: **Frank Coelho de Alcantara**

---

## Integrantes do Grupo 

| Nome | GitHub |
|------|--------|
| Gabriel Baú | https://github.com/ogabrielbau |
| Bernardo Czizyk | https://github.com/BBernardoC |
| Mateus Filipe | https://github.com/MateusMonfort |
| Gregory Keune | https://github.com/gregorykeune |
---

## Link para Execução Online
GDB Online: https://onlinegdb.com/VWnmMKrUlA

# 1. Objetivo do Projeto

Este trabalho implementa um **sistema de gerenciamento de inventário** em Haskell, utilizando:

- Programação funcional e tipos algébricos  
- Separação total entre lógica pura e efeitos colaterais (IO)  
- Persistência de dados em arquivos (`Inventario.dat`)  
- Auditoria completa com log (`Auditoria.log`) em modo append-only  
- Execução interativa via terminal  
- Relatórios completos com análise de logs  

---

#2. Descrição Geral

O sistema permite:

- **Adicionar itens**  
- **Remover itens**  
- **Atualizar quantidade de itens**
- **Listar inventário**
- **Gerar relatório completo (report)**

Cada operação, incluindo falhas, é registrada no log.

O estado atual do inventário **persiste entre execuções**, pois é salvo em arquivo.

---

# 3. Estrutura do Projeto
/
├── Inventario.hs # Código fonte principal
├── Inventario.dat # Estado persistido do inventário
├── Auditoria.log # Log de auditoria append-only
└── README.md

# 4. Tipos de Dados

O sistema define:

- `Item`  
- `Inventario = Map String Item`
- `AcaoLog = AddItem | RemoveItem | UpdateItem | QueryFail`
- `StatusLog = Sucesso | Falha String`
- `LogEntry` (timestamp, ação, detalhes, status)

Todos derivam `Show` e `Read`, como exigido, possibilitando serialização e desserialização.

---


