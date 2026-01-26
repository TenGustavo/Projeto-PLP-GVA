# **Documento Adicional — Participação dos Integrantes e Aplicação de Programação Funcional**

**Projeto:** Gerenciador de Vida Acadêmica (GVA)  
 **Disciplina:** Paradigmas de Linguagens de Programação  
 **Linguagem:** Haskell

---

## **1\. Organização Geral do Projeto**

O **Gerenciador de Vida Acadêmica (GVA)** foi desenvolvido de forma modular, utilizando Haskell e colocando em prática os principais conceitos de programação funcional trabalhados ao longo da disciplina.

Desde o início, o grupo decidiu dividir o sistema em módulos bem definidos, para que cada parte tivesse uma responsabilidade clara. Isso facilitou tanto o desenvolvimento quanto a manutenção do código, além de permitir que cada integrante se aprofundasse em um aspecto diferente do paradigma funcional.

A estrutura modular do projeto também ajudou a aplicar, na prática, ideias como **abstração**, **separação de responsabilidades**, **uso de funções puras** e **modelagem de dados com tipos algébricos**.

---

## **2\. Participação dos Integrantes**

### **Arthur — Módulo Main.hs**

Arthur ficou responsável pelo módulo **Main**, que funciona como o ponto de entrada do sistema.

Ele desenvolveu toda a parte de integração entre os módulos e a interface em linha de comando (CLI), organizando o fluxo principal do programa. Foi ele quem estruturou o menu, as interações com o usuário e a forma como o sistema chama as funcionalidades dos outros módulos.

**Conceitos de programação funcional aplicados:**

* Separação entre a lógica do sistema e o controle de entrada e saída

* Uso do tipo **IO** apenas onde realmente necessário

* Organização do programa como composição de funções

---

### **Gustavo — Módulo Types.hs**

Gustavo foi responsável pelo módulo **Types**, que serve como base para todo o sistema.

Ele definiu os principais **tipos de dados** usados no projeto, como disciplinas, atividades, horários e o estado geral do sistema. Esse módulo centraliza as estruturas que são utilizadas pelos demais, garantindo padronização e segurança na manipulação das informações.

**Conceitos de programação funcional aplicados:**

* Uso de **tipos algébricos (data)** para modelar o domínio do problema

* Representação de diferentes categorias por meio de **uniões disjuntas**

* Valorização do forte sistema de tipos do Haskell, prevenindo erros ainda em tempo de compilação

* Modelagem de dados imutáveis

---

### **Leandro — Módulos Persistence.hs e Horarios.hs**

Leandro contribuiu em duas partes importantes do sistema: **persistência de dados** e **controle de horários**.

No módulo **Persistence**, ele implementou a leitura e escrita do estado do sistema em arquivos, permitindo que as informações não se perdessem ao encerrar o programa. Já no módulo **Horarios**, trabalhou com o controle da grade semanal do aluno, incluindo verificação de conflitos entre horários.

**Atividades desenvolvidas:**

* Serialização e desserialização de dados

* Garantia de persistência entre execuções

* Inserção, remoção e organização de horários

* Verificação automática de conflitos

**Conceitos de programação funcional aplicados:**

* Separação entre código puro e efeitos colaterais, concentrando E/S no tipo **IO**

* Criação de novos estados em vez de modificar dados existentes

* Uso de funções puras para verificar conflitos de horário

* Modelagem do problema de forma declarativa

---

### **Oscar — Módulo Disciplinas.hs**

Oscar ficou responsável pelo módulo **Disciplinas**, que gerencia as informações relacionadas às matérias do aluno.

Ele implementou as funções de cadastro, listagem e remoção de disciplinas, além do registro de notas e períodos cursados. Esse módulo também prepara os dados necessários para cálculos acadêmicos, como o IRA.

**Conceitos de programação funcional aplicados:**

* Uso de **listas** para armazenar coleções de disciplinas

* Transformação de dados por meio de **funções puras**

* Uso de **funções de ordem superior** e composição de funções

---

### **Igor — Módulo Atividades.hs**

Igor desenvolveu o módulo **Atividades**, responsável pelo controle de trabalhos, tarefas e outras demandas acadêmicas.

Ele implementou a criação de atividades vinculadas às disciplinas, atualização de status, além de listagem e remoção desses registros.

**Conceitos de programação funcional aplicados:**

* Representação das atividades com **tipos algébricos**

* Manipulação de listas de forma funcional

* Ausência de variáveis mutáveis, sempre gerando novos valores a partir dos anteriores

---

## **3\. Considerações Finais**

O desenvolvimento do GVA permitiu que o grupo aplicasse, de forma prática, vários conceitos centrais da programação funcional. Ao dividir o sistema em módulos e responsabilidades bem definidas, foi possível manter o código organizado, legível e coerente com os princípios estudados em sala.

Durante o projeto, trabalhamos diretamente com:

* Funções puras

* Imutabilidade

* Tipos algébricos

* Modularização

* Separação entre lógica e efeitos colaterais

A divisão das tarefas também ajudou cada integrante a se aprofundar em diferentes aspectos do paradigma funcional, tornando o aprendizado mais completo e colaborativo.

