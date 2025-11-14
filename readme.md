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

# 2. Descrição Geral

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
```plaintext
/
├── Inventario.hs        # Código fonte principal
├── Inventario.dat       # Estado persistido do inventário
├── Auditoria.log        # Log de auditoria append-only
└── README.md
```

# 4. Tipos de Dados

O sistema define:

- `Item`  
- `Inventario = Map String Item`
- `AcaoLog = AddItem | RemoveItem | UpdateItem | QueryFail`
- `StatusLog = Sucesso | Falha String`
- `LogEntry` (timestamp, ação, detalhes, status)

Todos derivam `Show` e `Read`, como exigido, possibilitando serialização e desserialização.

# 5. Lógica de Negócio (Funções Puras)

As funções puras do sistema são:

- `addItem`
- `removeItem`
- `updateItem`
- `validaQuantidade`
- `historicoPorItem`
- `itemMaisMovimentado`
- `logsDeErro`
- `gerarRelatorio`

As funções retornam:

Either String ResultadoOperacao


sendo:

type ResultadoOperacao = (Inventario, LogEntry)


Nenhuma função pura realiza IO, cumprindo totalmente a exigência de separação entre lógica pura e lógica impura.

---

# 6. Módulo de IO e Loop Principal

A função `main` realiza:

- Leitura segura do inventário e dos logs usando `catch`
- Loop interativo com comandos:

add <id> <nome> <quantidade> <categoria>
remove <id> <qtd>
update <id> <nova_qtd>
list
report
exit

- Salvamento em `Inventario.dat` após operações bem-sucedidas
- Registro de auditoria em `Auditoria.log` para sucesso ou falha

O sistema nunca mistura IO com lógica pura — tudo está no `mainLoop`.

---
# 7. Exemplos de Uso

### ➕ Adicionar item

add 10 teclado 5 perifericos


### ➖ Remover quantidade

remove 10 3


### 🔄 Atualizar quantidade

update 10 20


### 📋 Listar itens

list


### 📝 Gerar relatório completo

report


### ❌ Sair salvando

exit


---

 # 8. Cenários de Teste (Obrigatórios – RA2 seção 4.1)

A seguir estão os testes realizados conforme especificação.

---

## ✅ Cenário 1: Persistência de Estado (Sucesso)

1. Iniciar o programa sem arquivos.
2. Adicionar 3 itens:

add 1 caneta 10 escritorio
add 2 caderno 5 papelaria
add 3 borracha 8 papelaria

3. Sair com `exit`.
4. Arquivos gerados:
- `Inventario.dat`
- `Auditoria.log`
5. Reiniciar o programa.
6. Digitar:

list

7. Os 3 itens aparecem corretamente.

✔ Persistência funcionando.  
✔ Serialização funcionando.  
✔ Log sendo gerado corretamente.  

---

## ❌ Cenário 2: Erro de Lógica – Estoque Insuficiente

1. Adicionar item:

add 20 teclado 10 perifericos

2. Tentar remover quantidade maior:

remove 20 15

3. O sistema mostra:

Erro: Item ID: 20 - Estoque insuficiente. Disponivel: 10, Solicitado: 15

4. Verificar inventário:

list

Quantidade permanece **10**.
5. Verificar `Auditoria.log`:
- Entrada com `StatusLog (Falha ...)` aparece corretamente.

✔ Tratamento de falha funcionando.  
✔ Inventário não é modificado.  
✔ Log registra falha.  

---

## 📝 Cenário 3: Relatório de Erros

Após o cenário 2:

1. Executar:

report

2. O relatório lista:
- Total de erros
- Detalhes da tentativa de remoção inválida
- Histórico por item

✔ `logsDeErro` funcionando  
✔ `itemMaisMovimentado` funcionando  
✔ Relatório completo gerado corretamente  

---

# 9. Dados Mínimos

O inventário inclui **mais de 10 itens distintos**, conforme exigido no enunciado, adicionados durante os testes realizados.

---

# 10. Organização do Repositório

- Commits claros  
- Mensagens explicativas  
- README completo  
- Código estruturado e legível  

---

# 11. Conclusão

O sistema atende integralmente aos requisitos do RA2:

✔ Tipos de dados completos e serializáveis  
✔ Lógica funcional pura isolada  
✔ IO separado corretamente  
✔ Persistência funcionando  
✔ Logs de auditoria completos  
✔ Relatórios implementados  
✔ Testes documentados conforme solicitado  
✔ Execução em ambiente online (GDB/Replit)  
✔ README completo conforme especificado  

---






