000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. VALIDA-CPF INITIAL.
000030 AUTHOR. WILKSON SILVA.
000040 DATE-WRITTEN. 16/01/2020.
000050*      ******** VALIDACAO CPF             *********             *
000060
000070 ENVIRONMENT DIVISION.
000080 SPECIAL-NAMES.
000090     DECIMAL-POINT IS COMMA.

000190 DATA DIVISION.
000230 WORKING-STORAGE SECTION.
000240
000250 copy "AREA-CPF.cpy".
       LINKAGE SECTION.
       77 WS-CPF      PIC 9(11).
       77 WS-RETORNO  PIC X(01).
000480
000700 PROCEDURE DIVISION USING WS-CPF
                                WS-RETORNO.

       VALIDA-CPF.

           MOVE WS-CPF     TO AREA-CPF
      *VALIDAÇÃO DIGITOS IGUAIS
           IF (NUM1 = NUM2)  AND
              (NUM2 = NUM3)  AND
              (NUM3 = NUM4)  AND
              (NUM4 = NUM5)  AND
              (NUM5 = NUM6)  AND
              (NUM6 = NUM7)  AND
              (NUM7 = NUM8)  AND
              (NUM8 = NUM9)  AND
              (NUM9 = NUM10) AND
              (NUM10 = NUM11)
              MOVE "S" TO WS-ERRO-CPF
           ELSE
      *VALIDAÇÃO DO PRIMEIRO DÍGITO
              COMPUTE SOMA1 = (NUM1 * 10) +
                              (NUM2 * 9)  +
                              (NUM3 * 8)  +
                              (NUM4 * 7)  +
                              (NUM5 * 6)  +
                              (NUM6 * 5)  +
                              (NUM7 * 4)  +
                              (NUM8 * 3)  +
                              (NUM9 * 2)

              COMPUTE SOMA1 = (SOMA1 * 10)
              DIVIDE SOMA1 BY 11 GIVING RESULT1 REMAINDER RESTO1

              IF RESTO1 = 10
                 MOVE 0 TO RESTO1
              END-IF

      *VALIDAÇÃO DO SEGUNDO DÍGITO
              COMPUTE SOMA2 = (NUM1 * 11) +
                              (NUM2 * 10) +
                              (NUM3 * 9)  +
                              (NUM4 * 8)  +
                              (NUM5 * 7)  +
                              (NUM6 * 6)  +
                              (NUM7 * 5)  +
                              (NUM8 * 4)  +
                              (NUM9 * 3)  +
                              (NUM10 * 2)

              COMPUTE SOMA2 = (SOMA2 * 10)
              DIVIDE SOMA2 BY 11 GIVING RESULT2 REMAINDER RESTO2

              IF RESTO2 = 10
                 MOVE 0 TO RESTO2
              END-IF

              IF (RESTO1 = NUM10) AND (RESTO2 = NUM11)
                 MOVE "N" TO WS-ERRO-CPF
              ELSE
                 MOVE "S" TO WS-ERRO-CPF
              END-IF
           END-IF.
           MOVE WS-ERRO-CPF TO WS-RETORNO.
       F-VALIDA-CPF. EXIT.
           GOBACK.
