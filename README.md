# Gerenciador de Vida Acadêmica (GVA)

Projeto desenvolvido na disciplina de **Paradigmas de Linguagens de Programação (PLP)**, utilizando a linguagem **Haskell**, com o objetivo de exercitar o **paradigma funcional** por meio de um sistema de linha de comando (CLI).

O sistema auxilia o estudante no gerenciamento de sua vida acadêmica, permitindo o controle de disciplinas, atividades, horários e desempenho acadêmico.

## Objetivo do Projeto

Desenvolver um sistema funcional, modular e persistente que aplique conceitos do paradigma funcional, tais como:
- uso de funções puras
- tipos algébricos
- imutabilidade de dados
- separação de responsabilidades por módulos

## Funcionalidades

- Cadastro e gerenciamento de disciplinas
- Registro de atividades e trabalhos acadêmicos
- Controle de horário semanal com bloqueio de conflitos
- Persistência de dados em arquivo
- Cálculo do **IRA (Índice de Rendimento Acadêmico)** conforme o modelo da UFCG
- Interface baseada em menu CLI

## Estrutura do Projeto

gva/
├─ README.md
├─ .gitignore
├─ src/
│ ├─ Main.hs
│ ├─ Types.hs
│ ├─ Persistence.hs
│ ├─ Disciplinas.hs
│ ├─ Atividades.hs
│ └─ Horarios.hs
└─ data/
└─ gva.db


## Descrição dos módulos

- **Main.hs**  
  Ponto de entrada do sistema. Contém o menu principal e coordena a navegação entre os módulos.

- **Types.hs**  
  Define os tipos de dados do sistema (disciplinas, atividades, horários, estado global) e funções auxiliares.

- **Persistence.hs**  
  Responsável pela leitura e escrita dos dados em arquivo, garantindo persistência entre execuções.

- **Disciplinas.hs**  
  Implementa o gerenciamento de disciplinas: cadastro, listagem, remoção e registro de notas e períodos.

- **Atividades.hs**  
  Gerencia atividades e trabalhos acadêmicos, incluindo criação, atualização de status e remoção.

- **Horarios.hs**  
  Controla o horário semanal do aluno, impedindo conflitos de horários automaticamente.

## Persistência de Dados

Os dados do sistema são armazenados no arquivo `data/gva.db`, utilizando serialização simples com `Show` e `Read`.

Esse arquivo é criado automaticamente na primeira execução do sistema.

## Cálculo do IRA

O sistema calcula o **IRA Individual** conforme o modelo da UFCG, considerando:
- notas finais das disciplinas
- carga horária
- período cursado
- penalização por trancamentos

O valor é exibido tanto na escala padrão quanto multiplicado por 1000.

## Como Compilar e Executar

Na raiz do projeto, execute:

```bash
ghc -isrc src/Main.hs -o gva
./gva

