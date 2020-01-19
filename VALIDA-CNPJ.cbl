000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. VALIDA-CNPJ INITIAL.
000030 AUTHOR. WILKSON SILVA.
000040 DATE-WRITTEN. 16/01/2020.
000050*      ******** VALIDACAO CNPJ *********             *
000060
000070 ENVIRONMENT DIVISION.
000080 SPECIAL-NAMES.
000090     DECIMAL-POINT IS COMMA.
000180

000190 DATA DIVISION.
000230 WORKING-STORAGE SECTION.
000240
000250 copy "AREA-CNPJ.cpy".
       LINKAGE SECTION.
       77 WS-CNPJ   PIC 9(14).
       77 WS-RETORNO  PIC X(01).
000480
000700 PROCEDURE DIVISION USING WS-CNPJ
                                WS-RETORNO.
003000
       VALIDA-CNPJ.
003010
003020     move WS-CNPJ to AREA-CNPJ
003030     compute CALC1 = (NUM1  * 5) +
003040                     (NUM2  * 4) +
003050                     (NUM3  * 3) +
003060                     (NUM4  * 2) +
003070                     (NUM5  * 9) +
003080                     (NUM6  * 8) +
003090                     (NUM7  * 7) +
003100                     (NUM8  * 6) +
003110                     (NUM9  * 5) +
003120                     (NUM10 * 4) +
003130                     (NUM11 * 3)+
003140                     (NUM12 * 2)
003150
           DIVIDE CALC1 BY 11 GIVING RESULT1 REMAINDER RESTO1
           IF RESTO1 < 2
              MOVE 0 TO RESTO1
           ELSE
              COMPUTE RESTO1 = 11 - RESTO1
003220     END-IF
           MOVE RESTO1 TO DIG1
003240
003250     COMPUTE CALC2 = (NUM1  * 6) +
003260                     (NUM2  * 5) +
003270                     (NUM3  * 4) +
003280                     (NUM4  * 3) +
003290                     (NUM5  * 2) +
003300                     (NUM6  * 9) +
003310                     (NUM7  * 8) +
003320                     (NUM8  * 7) +
003330                     (NUM9  * 6) +
003340                     (NUM10 * 5) +
003350                     (NUM11 * 4) +
003360                     (NUM12 * 3) +
003370                     (dig1  * 2)
           DIVIDE CALC2 BY 11 GIVING RESULT2 REMAINDER RESTO2
           IF RESTO2 < 2
              MOVE 0 TO RESTO2
           ELSE
              COMPUTE RESTO2 = 11 - RESTO2
           END-IF
003440
003450     IF (DIG1 = NUM13) AND (RESTO2 = NUM14)
003460        MOVE "N" TO WS-ERRO-CNPJ
003470     else
003480        MOVE "S" TO WS-ERRO-CNPJ
003490     end-if.
           MOVE  WS-ERRO-CNPJ TO WS-RETORNO.
003530 F-VALIDA-CNPJ. EXIT.
           GOBACK.
