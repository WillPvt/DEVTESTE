       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPORTACAO.
       AUTHOR. WILKSON SILVA.
       DATE-WRITTEN. 16/01/2020.
      *      ******** PROGRAMA DE IMPORTACAO DE CLIENTE E VENDEDOR

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
                  ORGANIZATION     IS INDEXED
                  RECORD KEY       IS CLI-CODIGO
                  ACCESS MODE      IS DYNAMIC
                  LOCK MODE        IS MANUAL
                  FILE STATUS      IS WS-RESULTADO-ACESSO.

           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
                  ORGANIZATION     IS INDEXED
                  RECORD KEY       IS VEN-CODIGO
                  ACCESS MODE      IS DYNAMIC
                  LOCK MODE        IS MANUAL
                  FILE STATUS      IS WS-RESULTADO-ACESSO.

           SELECT ARQ-IMPORTA-CLI ASSIGN TO DISK WS-LOCAL-ARQ-CLI
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-IMPORTA-VEN ASSIGN TO DISK WS-LOCAL-ARQ-VEN
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-LOG ASSIGN TO DISK WS-LOCAL-ARQ-LOG
                  ORGANIZATION IS LINE SEQUENTIAL.




       DATA DIVISION.
       FILE SECTION.
       COPY "ARQ-CLIENTE.FD".

       COPY "ARQ-VENDEDOR.FD".

       COPY "ARQ-IMPORTA-VEN.FD".

       COPY "ARQ-IMPORTA-CLI.FD".

       FD ARQ-LOG.
          01 REG-LOG.
             02 LOG-MENSAGEM  PIC X(100).
             02 LOG-CNPJ      PIC 9(14).



       WORKING-STORAGE SECTION.
       01 AUX-REGISTRO-CLIENTE.
          02 AUX-CODIGO            PIC 9(07).
          02 AUX-CNPJ              PIC 9(14).
          02 AUX-RAZAO-SOCIAL      PIC X(40).
          02 AUX-LATITUDE          PIC S9(03)V9(08).
          02 AUX-LONGITUDE         PIC S9(03)V9(08).

       01 AUX-REGISTRO-VENDEDOR.
          02 AUX-CODIGO-VEN            PIC 9(03).
          02 AUX-CPF-VEN               PIC 9(11).
          02 AUX-RAZAO-SOCIAL-VEN      PIC X(40).
          02 AUX-LATITUDE-VEN          PIC S9(03)V9(08).
          02 AUX-LONGITUDE-VEN         PIC S9(03)V9(08).


       77 SAI-COD               PIC 9(02) VALUE ZEROS.
       77 LINHA-TRACO           PIC X(80) VALUE ALL '-'.
       77 WS-TIPO-DADO-DISPLAY  PIC X(12) VALUE SPACES.
       77 LIMPA-LINHA           PIC X(80) VALUE SPACES.
       77 WID-ARQ-CLIENTE       PIC X(50) VALUE SPACES.
       77 WS-RESULTADO-ACESSO   PIC 9(02) VALUES ZEROS.
       77 TIPO-LEITURA          PIC X(02) VALUES SPACES.
       77 CONTROLE-FIM          PIC 9(02) VALUES ZEROS.
       77 OPCAO                 PIC A(01) VALUES SPACES.
       77 PAUSA                 PIC X(02) VALUES SPACES.
       77 MASCARA-DATA-CADASTRO PIC 99/99/99.
       77 WS-SAI                PIC 9(02) VALUE ZEROS.
       77 WS-RETORNO        PIC X VALUE SPACES.

       LINKAGE SECTION.
       77 DATA-DE-HOJE          PIC 99/99/99.
       77 LK-TIPO-DADO PIC 9(01). *> 01 - CLIENTE 02 - VENDEDOR

       SCREEN SECTION.
       01 LIMPA-TELA   BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.

       01 TELA-IMPORTA BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "  IMPORTACAO DE ARQUIVOS ".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "NOME DO ARQUIVO.....".

       01 TELA-FIM BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "  IMPORTACAO DE ARQUIVOS ".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 15 COLUMN 01
          VALUE "PROCESSO DE IMPORTACAO CONCLUIDO, TECLE ENTER PARA RETO
      -"RNAR AO MENU INICIAL ".

       PROCEDURE DIVISION USING DATA-DE-HOJE
                                LK-TIPO-DADO.

       INICIO.

           IF LK-TIPO-DADO = 01
              DISPLAY "CLIENTES " AT 0254
              DISPLAY TELA-IMPORTA AT 0101
              ACCEPT WS-LOCAL-ARQ-CLI           AT 0421
           ELSE
              DISPLAY "VENDEDORES " AT 0254
              DISPLAY TELA-IMPORTA AT 0101
              ACCEPT WS-LOCAL-ARQ-VEN           AT 0421
           END-IF

           MOVE "LOG.TXT"       TO WS-LOCAL-ARQ-LOG
           OPEN OUTPUT ARQ-LOG

           EVALUATE LK-TIPO-DADO
              WHEN 01
                PERFORM IMPORTA-CLIENTE
                   THRU F-IMPORTA-CLIENTE
                CLOSE ARQ-CLIENTE
                CLOSE ARQ-LOG
                CLOSE ARQ-IMPORTA-CLI
              WHEN 02
                PERFORM IMPORTA-VENDEDOR
                   THRU F-IMPORTA-VENDEDOR
                CLOSE ARQ-VENDEDOR
                CLOSE ARQ-IMPORTA-VEN
                CLOSE ARQ-LOG
           END-EVALUATE
           DISPLAY TELA-fim AT 0101
           ACCEPT PAUSA     AT 2478
           EXIT PROGRAM.

       IMPORTA-CLIENTE.
           MOVE "CLIENTE.DAT"   TO WID-ARQ-CLIENTE
           OPEN I-O ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-CLIENTE
              CLOSE ARQ-CLIENTE
              OPEN I-O ARQ-CLIENTE
           END-IF
           OPEN INPUT ARQ-IMPORTA-CLI
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO AO ABRIR O ARQUIVO SOLICITADO!" AT  0101
              ACCEPT PAUSA AT 2478
              EXIT PROGRAM
           END-IF
           PERFORM UNTIL WS-SAI = 99
              READ ARQ-IMPORTA-CLI NEXT
                 AT END
                    MOVE 99 TO WS-SAI
                    EXIT PERFORM
              END-READ
              PERFORM PEGA-CODIGO-CLI THRU F-PEGA-CODIGO-CLI
              MOVE IMP-CLI-CODIGO       TO  CLI-CODIGO
              MOVE IMP-CLI-CNPJ         TO  CLI-CNPJ
              MOVE SPACES TO LOG-MENSAGEM
001730        IF CLI-CNPJ = ZEROS
001740           MOVE "CNPJ EM BRANCO!" TO LOG-MENSAGEM
                 MOVE CLI-CNPJ TO LOG-CNPJ
                 WRITE REG-LOG
              ELSE
                 MOVE "S" TO WS-RETORNO
                 CALL "VALIDA-CNPJ" USING CLI-CNPJ
                                          WS-RETORNO
                 PERFORM CNPJ-DUPLICADO THRU F-CNPJ-DUPLICADO
                 IF WS-RETORNO = "N"
                    MOVE IMP-CLI-RAZAO-SOCIAL TO  CLI-RAZAO-SOCIAL
                    MOVE IMP-CLI-LATITUDE     TO  CLI-LATITUDE
                    MOVE IMP-CLI-LONGITUDE    TO  CLI-LONGITUDE
                    WRITE REGISTRO-CLIENTE
001740              MOVE "REGISTRO INCLUIDO!" TO LOG-MENSAGEM
                    MOVE CLI-CNPJ TO LOG-CNPJ
                    WRITE REG-LOG
                 ELSE
                    IF LOG-MENSAGEM = SPACES
001740                 MOVE "CNPJ INVALIDO!" TO LOG-MENSAGEM
                    END-IF
                    MOVE CLI-CNPJ TO LOG-CNPJ
                    WRITE REG-LOG
                 END-IF
001750        END-IF
      *PERFORMAR  VALIDACAO DE CPNJ E GRAVAR ARQUIVO DE LOG COM ERROS
      *SE DER ERRO NA VALIDACAO, NAO GRAVAR, SE NAO DER ERRO, GRAVAR
           END-PERFORM.

       F-IMPORTA-CLIENTE. EXIT.

       IMPORTA-VENDEDOR.
           MOVE "VENDEDOR.DAT"   TO WID-ARQ-VENDEDOR
           OPEN I-O ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-VENDEDOR
              CLOSE ARQ-VENDEDOR
              OPEN I-O ARQ-VENDEDOR
           END-IF

      *    MOVE WS-LOCAL-IMPORTA TO WS-LOCAL-ARQ
           OPEN INPUT ARQ-IMPORTA-VEN

           PERFORM UNTIL WS-SAI = 99
              READ ARQ-IMPORTA-VEN NEXT
                 AT END
                    MOVE 99 TO WS-SAI
                    EXIT PERFORM
              END-READ
              PERFORM PEGA-CODIGO-VEN THRU F-PEGA-CODIGO-VEN
              MOVE IMP-VEN-CODIGO       TO  VEN-CODIGO
              MOVE IMP-VEN-CPF          TO  VEN-CPF
              MOVE SPACES TO LOG-MENSAGEM
001730        IF VEN-CPF = ZEROS
001740           MOVE "CPF EM BRANCO!" TO LOG-MENSAGEM
                 MOVE VEN-CPF TO LOG-CNPJ
                 WRITE REG-LOG
              ELSE
                 MOVE "S" TO WS-RETORNO
                 CALL "VALIDA-CPF" USING VEN-CPF
                                          WS-RETORNO
                 PERFORM CPF-DUPLICADO THRU F-CPF-DUPLICADO
                 IF WS-RETORNO = "N"
                    MOVE IMP-VEN-RAZAO-SOCIAL TO  VEN-NOME
                    MOVE IMP-VEN-LATITUDE     TO  VEN-LATITUDE
                    MOVE IMP-VEN-LONGITUDE    TO  VEN-LONGITUDE
                    WRITE REGISTRO-VENDEDOR
001740              MOVE "REGISTRO INCLUIDO!" TO LOG-MENSAGEM
                    MOVE VEN-CPF TO LOG-CNPJ
                    WRITE REG-LOG
                 ELSE
                    IF LOG-MENSAGEM = SPACES
001740                 MOVE "CPF INVALIDO!" TO LOG-MENSAGEM
                    END-IF
                    MOVE VEN-CPF TO LOG-CNPJ
                    WRITE REG-LOG
                 END-IF
001750        END-IF
      *PERFORMAR  VALIDACAO DE CPNJ E GRAVAR ARQUIVO DE LOG COM ERROS
      *SE DER ERRO NA VALIDACAO, NAO GRAVAR, SE NAO DER ERRO, GRAVAR
           END-PERFORM.
       F-IMPORTA-VENDEDOR. EXIT.

       PEGA-CODIGO-CLI.
           MOVE 9999999 TO CLI-CODIGO
           PERFORM UNTIL SAI-COD = 99
              MOVE 9999999 TO CLI-CODIGO
              START ARQ-CLIENTE KEY LESS CLI-CODIGO
              IF WS-RESULTADO-ACESSO NOT = 00
                 DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
                      AT 2401
                 DISPLAY WS-RESULTADO-ACESSO AT 2440
                 ACCEPT  PAUSA               AT 2478
                 DISPLAY LIMPA-TELA          AT 2401
              END-IF
              READ ARQ-CLIENTE NEXT AT END
                MOVE 99  TO SAI-COD
              END-READ
              ADD 1 TO CLI-CODIGO
              READ ARQ-CLIENTE
              IF WS-RESULTADO-ACESSO = 23
                 MOVE CLI-CODIGO TO IMP-CLI-CODIGO
                 MOVE 99 TO SAI-COD
              END-IF
           END-PERFORM
           MOVE ZEROS TO SAI-COD
           .

       F-PEGA-CODIGO-CLI. EXIT.

       PEGA-CODIGO-VEN.
           MOVE 999 TO VEN-CODIGO
           PERFORM UNTIL SAI-COD = 99
              MOVE 999 TO VEN-CODIGO
              START ARQ-VENDEDOR KEY LESS VEN-CODIGO
              IF WS-RESULTADO-ACESSO NOT = 00
                 DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - VENDEDOR: "
                      AT 2401
                 DISPLAY WS-RESULTADO-ACESSO AT 2440
                 ACCEPT  PAUSA               AT 2478
                 DISPLAY LIMPA-TELA          AT 2401
              END-IF
              READ ARQ-VENDEDOR NEXT AT END
                MOVE 99  TO SAI-COD
              END-READ
              ADD 1 TO VEN-CODIGO
              READ ARQ-VENDEDOR
              IF WS-RESULTADO-ACESSO = 23
                 MOVE VEN-CODIGO TO IMP-VEN-CODIGO
                 MOVE 99 TO SAI-COD
              END-IF
           END-PERFORM
           MOVE ZEROS TO SAI-COD
           .

       F-PEGA-CODIGO-VEN. EXIT.

       CNPJ-DUPLICADO.
           MOVE CLI-CNPJ TO AUX-CNPJ
           MOVE CLI-CODIGO TO AUX-CODIGO
           MOVE ZEROS TO CLI-CODIGO
           START ARQ-CLIENTE KEY NOT LESS CLI-CODIGO
           IF WS-RESULTADO-ACESSO = 00
              PERFORM UNTIL EXIT
              MOVE ZEROS TO CLI-CNPJ
              READ ARQ-CLIENTE NEXT
              AT END
                 EXIT PERFORM
              END-READ
              IF AUX-CNPJ = CLI-CNPJ
                 MOVE "S" TO WS-RETORNO
                 MOVE "CNPJ DUPLICADO  " TO LOG-MENSAGEM
                 EXIT PERFORM
              END-IF
              END-PERFORM
           END-IF.
           MOVE AUX-CNPJ TO CLI-CNPJ.
           MOVE AUX-CODIGO TO CLI-CODIGO.
           START ARQ-CLIENTE KEY EQUAL CLI-CODIGO.
       F-CNPJ-DUPLICADO. EXIT.

       CPF-DUPLICADO.
         MOVE VEN-CODIGO TO AUX-CODIGO-VEN
         MOVE VEN-CPF    TO AUX-CPF-VEN
         MOVE ZEROS TO VEN-CODIGO
         START ARQ-VENDEDOR KEY NOT LESS VEN-CODIGO
         IF WS-RESULTADO-ACESSO = 00
            PERFORM UNTIL EXIT
               MOVE ZEROS TO VEN-CPF
               READ ARQ-VENDEDOR NEXT
                  AT END
                  EXIT PERFORM
               END-READ
               IF AUX-CPF-VEN = VEN-CPF
                  MOVE "S" TO WS-RETORNO
                  MOVE "CPF DUPLICADO  " TO LOG-MENSAGEM
                  EXIT PERFORM
               END-IF
            END-PERFORM
         END-IF.
         MOVE AUX-CPF-VEN TO VEN-CPF.
         MOVE AUX-CODIGO-VEN TO VEN-CODIGO.
         START ARQ-VENDEDOR KEY EQUAL VEN-CODIGO.
       F-CPF-DUPLICADO. EXIT.


