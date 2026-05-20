       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTA01.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-SALDO            PIC 9(7)V99 VALUE 0.
       01  WS-EXIBI-SALDO      PIC Z(6)9,99.
       01  WS-VALOR            PIC 9(7)V99 VALUE 0.
       01  WS-OPCAO            PIC 9 VALUE 0.
       01  WS-SAIDA            PIC X VALUE 'N'.
       01  WS-ULT-OPERACAO     PIC X(20) VALUE SPACES.
       01  WS-LOGADO           PIC X VALUE 'N'.
           88 LOGADO VALUE 'S'.
           88 NAO-LOGADO VALUE 'N'.
       01  WS-LOGIN.
           03 LOGIN-AGENCIA    PIC 9(4).
           03 LOGIN-CONTA      PIC 9(5).
           03 LOGIN-HASH       PIC 9(18).

       01  WS-ENTRADA.    
           03 ENTRADA-AGENCIA    PIC 9(4).
           03 ENTRADA-CONTA      PIC 9(5).
           03 ENTRADA-SENHA      PIC X(10).
           03 ENTRADA-NOME       PIC X(50).
           03 ENTRADA-EMAIL      PIC X(50).
           03 ENTRADA-TELEFONE   PIC X(15).

       01 CADASTRO-AREA-DADOS.
           03 CADASTRO-NOME             PIC X(50).
           03 CADASTRO-EMAIL            PIC X(50).
           03 CADASTRO-TELEFONE         PIC X(15).
           03 CADASTRO-SENHA            PIC X(10).
           03 CADASTRO-RESPOSTA.
               05 CADASTRO-AGENCIA      PIC 9(4).
               05 CADASTRO-CONTA        PIC 9(5).
               05 CADASTRO-HASH         PIC 9(18).
               05 CADASTRO-COD-ERRO     PIC 9(03).
               05 CADASTRO-TXT-MSG-ERRO PIC X(100).

       01 HASHSHA2-AREA-DADOS.
           03 HASHSHA2-SENHA                    PIC X(010).
           03 HASHSHA2-RESPOSTA.
               05 HASHSHA2-HASH                     PIC 9(018).
               05 HASHSHA2-COD-ERRO                 PIC 9(003).
               05 HASHSHA2-TXT-MSG-ERRO             PIC X(100).

       PROCEDURE DIVISION.

       000000-PRINCIPAL        SECTION.
           PERFORM UNTIL WS-SAIDA = 'S'
               IF WS-LOGADO = 'N'
                   PERFORM EXIBIR-LOGIN
                   ACCEPT WS-OPCAO
                   EVALUATE WS-OPCAO
                       WHEN 1
                           PERFORM CADASTRAR
                       WHEN 2
                           PERFORM ENTRAR
                       WHEN 3
                           MOVE 'S' TO WS-SAIDA                           
                       WHEN OTHER
                           DISPLAY "Opcao invalida"
                   END-EVALUATE
               ELSE
                   PERFORM EXIBIR-MENU
                   ACCEPT WS-OPCAO
                   EVALUATE WS-OPCAO
                       WHEN 1
                           PERFORM CONSULTAR-SALDO
                       WHEN 2
                           PERFORM DEPOSITAR
                       WHEN 3
                           PERFORM SACAR
                       WHEN 4
                           MOVE 'N' TO WS-LOGADO
                       WHEN 5
                           MOVE 'S' TO WS-SAIDA
                       WHEN OTHER
                           DISPLAY "Opcao invalida"
                   END-EVALUATE
               END-IF
           END-PERFORM.

           DISPLAY "Programa encerrado."
           STOP RUN.
       0000099.
      *
       EXIBIR-LOGIN.
           DISPLAY " "
           DISPLAY "===== LOGIN ====="
           DISPLAY "1 - Cadastrar"
           DISPLAY "2 - Entrar"
           DISPLAY "3 - Sair"
           DISPLAY "Escolha uma opcao: " WITH NO ADVANCING.
      *
       CADASTRAR.
           DISPLAY "Nome: " WITH NO ADVANCING
           ACCEPT ENTRADA-NOME
           MOVE ENTRADA-NOME TO CADASTRO-NOME

           DISPLAY "Email: " WITH NO ADVANCING
           ACCEPT ENTRADA-EMAIL
           MOVE ENTRADA-EMAIL TO CADASTRO-EMAIL

           DISPLAY "Telefone: " WITH NO ADVANCING
           ACCEPT ENTRADA-TELEFONE
           MOVE ENTRADA-TELEFONE TO CADASTRO-TELEFONE

           DISPLAY "Senha: " WITH NO ADVANCING
           ACCEPT ENTRADA-SENHA
           MOVE ENTRADA-SENHA TO CADASTRO-SENHA

           CALL 'CADASTRO' USING CADASTRO-AREA-DADOS

           IF CADASTRO-COD-ERRO NOT EQUAL 0
               DISPLAY "Erro no cadastro: " 
               DISPLAY CADASTRO-TXT-MSG-ERRO
           ELSE
               DISPLAY "Cadastro realizado com sucesso!" 
               DISPLAY "Sua agencia é: "  WITH NO ADVANCING
               DISPLAY CADASTRO-AGENCIA
               DISPLAY "Sua conta é: "  WITH NO ADVANCING
               DISPLAY CADASTRO-CONTA
               MOVE CADASTRO-AGENCIA TO LOGIN-AGENCIA
               MOVE CADASTRO-CONTA TO LOGIN-CONTA
               MOVE CADASTRO-HASH TO LOGIN-HASH
           END-IF.
      *
       ENTRAR.
           DISPLAY "Agencia: " WITH NO ADVANCING
           ACCEPT ENTRADA-AGENCIA

           DISPLAY "Conta: " WITH NO ADVANCING
           ACCEPT ENTRADA-CONTA

           DISPLAY "Senha: " WITH NO ADVANCING
           ACCEPT ENTRADA-SENHA

           IF ENTRADA-AGENCIA NOT EQUAL LOGIN-AGENCIA
           OR ENTRADA-CONTA NOT EQUAL LOGIN-CONTA
               DISPLAY "Agencia ou conta invalida."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE ENTRADA-SENHA TO HASHSHA2-SENHA
           CALL 'HASHSHA2' USING HASHSHA2-AREA-DADOS

           IF HASHSHA2-COD-ERRO NOT EQUAL 0
               DISPLAY "Erro ao obter hash: " HASHSHA2-TXT-MSG-ERRO
           ELSE
               IF HASHSHA2-HASH EQUAL LOGIN-HASH
                   MOVE 'S' TO WS-LOGADO
                   DISPLAY "Login realizado com sucesso!"
               ELSE
                   DISPLAY "Senha invalida. Login falhou."
               END-IF
           END-IF.
      *
       EXIBIR-MENU.
           DISPLAY " "
           DISPLAY "===== CONTA BANCARIA ====="
           DISPLAY "1 - Consultar saldo"
           DISPLAY "2 - Depositar"
           DISPLAY "3 - Sacar"
           DISPLAY "4 - Logout"
           DISPLAY "5 - Sair"
           DISPLAY "Escolha uma opcao: " WITH NO ADVANCING.

       CONSULTAR-SALDO.
           DISPLAY " "
           MOVE WS-SALDO TO WS-EXIBI-SALDO
           DISPLAY "Saldo atual: " WS-EXIBI-SALDO
           DISPLAY "Ultima operacao: " WS-ULT-OPERACAO.

       DEPOSITAR.
           DISPLAY "Digite o valor do deposito: "
           ACCEPT WS-VALOR

           IF WS-VALOR > 0
               ADD WS-VALOR TO WS-SALDO
               MOVE "DEPOSITO" TO WS-ULT-OPERACAO
               DISPLAY "Deposito realizado com sucesso."
           ELSE
               DISPLAY "Valor invalido para deposito."
           END-IF.

       SACAR.
           DISPLAY "Digite o valor do saque: "
           ACCEPT WS-VALOR

           IF WS-VALOR <= 0
               DISPLAY "Valor invalido."
           ELSE
               IF WS-VALOR > WS-SALDO
                   DISPLAY "Saldo insuficiente."
               ELSE
                   SUBTRACT WS-VALOR FROM WS-SALDO
                   MOVE "SAQUE" TO WS-ULT-OPERACAO
                   DISPLAY "Saque realizado com sucesso."
               END-IF
           END-IF.
