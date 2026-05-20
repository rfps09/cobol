       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDASENHA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-I                        PIC 9(02) VALUE 1.
       01  WS-CHAR                     PIC X.

       01  WS-TEM-LETRA                PIC X VALUE 'N'.
           88 TEM-LETRA                VALUE 'S'.
           88 NAO-TEM-LETRA            VALUE 'N'.

       01  WS-TEM-NUMERO               PIC X VALUE 'N'.
           88 TEM-NUMERO               VALUE 'S'.
           88 NAO-TEM-NUMERO           VALUE 'N'.

       01  WS-TEM-ESPECIAL             PIC X VALUE 'N'.
           88 TEM-ESPECIAL             VALUE 'S'.
           88 NAO-TEM-ESPECIAL         VALUE 'N'.

       LINKAGE                         SECTION.
       01 VALIDASENHA-AREA-DADOS.
          03 VALIDASENHA-SENHA                  PIC X(10).
          03 VALIDASENHA-RESPOSTA.
             05 VALIDASENHA-LETRA               PIC 9(01).
             05 VALIDASENHA-NUMERO              PIC 9(01).
             05 VALIDASENHA-ESPECIAL            PIC 9(01).
       PROCEDURE DIVISION USING VALIDASENHA-AREA-DADOS.

       000000-PRINCIPAL                SECTION.
      *
           PERFORM 100000-VERIFICA-SENHA.
           GOBACK.
      *
       000099-SAIDA.

       100000-VERIFICA-SENHA           SECTION.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LENGTH OF VALIDASENHA-SENHA
               MOVE VALIDASENHA-SENHA(WS-I:1) TO WS-CHAR
               EVALUATE TRUE
      *        VERIFICA LETRAS
                   WHEN WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                       SET TEM-LETRA TO TRUE
                   WHEN WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                       SET TEM-LETRA TO TRUE
      *        VERIFICA NÚMEROS
                   WHEN WS-CHAR >= '0' AND WS-CHAR <= '9'
                       SET TEM-NUMERO TO TRUE
      *        VERIFICA CARACTERES ESPECIAIS
                   WHEN WS-CHAR NOT = SPACE
                       SET TEM-ESPECIAL TO TRUE
               END-EVALUATE
           END-PERFORM.

           IF TEM-LETRA
               MOVE 0 TO VALIDASENHA-LETRA
           ELSE
               MOVE 1 TO VALIDASENHA-LETRA
           END-IF

           IF TEM-NUMERO
               MOVE 0 TO VALIDASENHA-NUMERO
           ELSE
               MOVE 1 TO VALIDASENHA-NUMERO
           END-IF

           IF TEM-ESPECIAL
               MOVE 0 TO VALIDASENHA-ESPECIAL
           ELSE
               MOVE 1 TO VALIDASENHA-ESPECIAL
           END-IF.

       100099-SAIDA.
