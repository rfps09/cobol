       IDENTIFICATION                  DIVISION.
       PROGRAM-ID. HASHSHA2.
      *
       ENVIRONMENT                     DIVISION.
      *
       DATA                            DIVISION.
       WORKING-STORAGE                 SECTION.
      *
       77  VALIDASENHA                 PIC X(011) VALUE 'VALIDASENHA'.
      *
       01  WS-SENHA                    PIC X(010) VALUE SPACES.
       01  WS-TAMANHO                  PIC 9(003) VALUE 10.
       01  WS-I                        PIC 9(003).
       01  WS-ASCII                    PIC 9(005).
       01  WS-HASH                     PIC 9(018) VALUE 0.
       01  WS-RESTO                    PIC 9(018).
      *
       01 VALIDASENHA-AREA-DADOS.
          03 VALIDASENHA-SENHA                  PIC X(10).
          03 VALIDASENHA-RESPOSTA.
             05 VALIDASENHA-LETRA               PIC 9(01).
             05 VALIDASENHA-NUMERO              PIC 9(01).
             05 VALIDASENHA-ESPECIAL            PIC 9(01).
      *
       LINKAGE                         SECTION.
       01 HASHSHA2-AREA-DADOS.
          03 HASHSHA2-SENHA                     PIC X(010).
          03 HASHSHA2-RESPOSTA.
             05 HASHSHA2-HASH                   PIC 9(018).
             05 HASHSHA2-COD-ERRO               PIC 9(003).
             05 HASHSHA2-TXT-MSG-ERRO           PIC X(100).
      *
      ******************************************************************
       PROCEDURE DIVISION USING HASHSHA2-AREA-DADOS.
      ******************************************************************
       000000-PRINCIPAL                SECTION.
      ******************************************************************
           PERFORM 100000-ENTRADA.
           PERFORM 200000-PROCESSA.
           GOBACK.
      *
       000099-SAIDA.
      *
      ******************************************************************
       100000-ENTRADA                  SECTION.
      ******************************************************************
      *
           MOVE 0 TO VALIDASENHA-LETRA VALIDASENHA-NUMERO.
           MOVE 0 TO VALIDASENHA-ESPECIAL.
           MOVE SPACES TO VALIDASENHA-SENHA.
      *
           MOVE HASHSHA2-SENHA TO VALIDASENHA-SENHA

           CALL VALIDASENHA USING VALIDASENHA-AREA-DADOS.
           
           MOVE 1 TO HASHSHA2-COD-ERRO
      
           EVALUATE TRUE
             WHEN VALIDASENHA-LETRA EQUAL 1
               MOVE 'Precisa conter pelo menos uma letra' 
               TO HASHSHA2-TXT-MSG-ERRO
             WHEN VALIDASENHA-NUMERO EQUAL 1
               MOVE 'Precisa conter pelo menos um número' 
               TO HASHSHA2-TXT-MSG-ERRO
             WHEN VALIDASENHA-ESPECIAL EQUAL 1
               MOVE 'Precisa conter pelo menos um caractere especial' 
               TO HASHSHA2-TXT-MSG-ERRO
             WHEN OTHER
               MOVE 0 TO HASHSHA2-COD-ERRO
               MOVE SPACES TO HASHSHA2-TXT-MSG-ERRO
           END-EVALUATE

           IF HASHSHA2-COD-ERRO NOT EQUAL 0
               GOBACK
           END-IF.
      *
       100099-SAIDA.
      *
      ******************************************************************
       200000-PROCESSA                 SECTION.
      ******************************************************************
      *
           MOVE 0 TO WS-HASH.
           MOVE HASHSHA2-SENHA TO WS-SENHA.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TAMANHO
               COMPUTE WS-ASCII = FUNCTION ORD(WS-SENHA(WS-I:1))
               COMPUTE WS-HASH  = (WS-HASH * 31) + WS-ASCII
               COMPUTE WS-RESTO =
               FUNCTION MOD(WS-HASH 999999999999999999)
               MOVE WS-RESTO TO HASHSHA2-HASH
           END-PERFORM.
      *
