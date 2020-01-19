       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG03.
       AUTHOR. WILKSON SILVA.
       DATE-WRITTEN. 16/01/2020.
      * ------------ LISTAGEM DE CLIENTE --------------

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

           SELECT ARQ-CLIENTE-SORT ASSIGN TO "SORT".

           SELECT ARQ-CLIENTE-SAI ASSIGN TO "ARQ-CLIENTE-SAI.DAT"
                  ORGANIZATION     IS SEQUENTIAL.



           SELECT RELATORIO ASSIGN TO "RCLIENTE.TXT"
                            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "ARQ-CLIENTE.FD".

       FD RELATORIO.
       01 LINHA        PIC X(132).

       SD ARQ-CLIENTE-SORT.
       01 REG-ARQ-CLIENTE-SORT.
          02 CLI-CODIGO-SORT            PIC 9(07).
          02 CLI-CNPJ-SORT              PIC 9(14).
          02 CLI-RAZAO-SOCIAL-SORT      PIC X(40).
          02 CLI-LATITUDE-SORT          PIC s9(03)V9(08).
          02 CLI-LONGITUDE-SORT         PIC s9(03)V9(08).


       FD ARQ-CLIENTE-SAI.
       01 REG-ARQ-CLIENTE-SAI.
          02 CLI-CODIGO-SAI            PIC 9(07).
          02 CLI-CNPJ-SAI              PIC 9(14).
          02 CLI-RAZAO-SOCIAL-SAI      PIC X(40).
          02 CLI-LATITUDE-SAI          PIC s9(03)V9(08).
          02 CLI-LONGITUDE-SAI         PIC s9(03)V9(08).

       WORKING-STORAGE SECTION.
      *VARIAVEIS DA TELA
       77 WS-ORDEM             PIC X(01) VALUE SPACES.
       77 WS-CLASSIFICA        PIC X(01) VALUE SPACES.
       77 WS-CODIGO-CLIENTE    PIC 9(07) VALUE ZEROS.
       77 WS-NOME-CLIENTE      PIC X(40) VALUE SPACES.
      *-----------------------------------------------------------------
       77 LINHA-TRACO          PIC X(80) VALUE ALL '-'.
       77 CONTADOR-LINHA       PIC 9(02) VALUE ZERO.
       77 CONTADOR-PAGINA      PIC 9(03) VALUE ZERO.
       77 CONTROLE-FIM         PIC 9(01) VALUE ZEROS.
       77 PAUSA                PIC X(01).
       77 WS-RESULTADO-ACESSO       PIC 9(02) VALUE ZEROS.
       01 CABECALHO-1.
          02 FILLER PIC X(06) VALUE "CODIGO".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(30) VALUE " NOME CLIENTE".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(18) VALUE "           C.N.P.J".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(15) VALUE "       LATITUDE".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(12) VALUE "   LONGITUDE".

       01 CABECALHO-2.
          02 FILLER PIC X(132) VALUES ALL "-".

       01 DETALHE.
          02 DET-CLI-CODIGO            PIC 9(07).
          02 FILLER                    PIC X(02) VALUE SPACES.
          02 DET-CLI-RAZAO-SOCIAL      PIC X(40).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-CLI-CNPJ              PIC 9(14).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-CLI-LATITUDE          PIC s9(03)V9(08).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-CLI-LONGITUDE         PIC s9(03)V9(08).

       01 CABECALHO-TITULO.
          02 CAB-DATA     PIC X(08).
          02 FILLER       PIC X(41) VALUE SPACES.
          02 FILLER       PIC X(19) VALUE "RELACAO DE CLIENTES".
          02 FILLER       PIC X(51) VALUES SPACES.
          02 FILLER       PIC X(09) VALUE "PAGINA: ".
          02 CAB-PAGINA   PIC ZZ9.

       LINKAGE SECTION.
       77 DATA-DE-HOJE    PIC 99/99/99.

       SCREEN SECTION.
       01 LIMPA-TELA BLANK SCREEN
                     BACKGROUND-COLOR 1
                     FOREGROUND-COLOR 7.


       01 TELA-RELATORIO BLANK SCREEN
                         BACKGROUND-COLOR 1
                         FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "     Relatorio de Clientes     ".
          02 LINE 02 COLUMN 73 VALUE "PROG04".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "ORDENACAO ASCENDENTE (A) OU DESCEN
      -"DENTE (D)?....... ".
          02 LINE 06 COLUMN 01 VALUE "CLASSIFICACAO POR CODIGO (C) OU RA
      -"ZAO SOCIAL (R)?.. ".
          02 LINE 08 COLUMN 01 VALUE
       "********************************FILTROS*************************
      -"****************".
          02 LINE 10 COLUMN 01 VALUE "CODIGO DO CLIENTE...".
          02 LINE 12 COLUMN 01 VALUE "RAZAO SOCIAL........".



       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.
           MOVE DATA-DE-HOJE TO CAB-DATA
           DISPLAY TELA-RELATORIO
           MOVE "CLIENTE.DAT"   TO WID-ARQ-CLIENTE
           OPEN OUTPUT RELATORIO
           PERFORM IMPRIMIR-CABECALHO.


       ACCEPT-TELA.
           PERFORM UNTIL WS-ORDEM = "A" OR = "D"
              ACCEPT WS-ORDEM AT 0453
           END-PERFORM

           PERFORM UNTIL WS-CLASSIFICA = "C" OR = "R"
              ACCEPT WS-CLASSIFICA AT 0653
           END-PERFORM

           ACCEPT WS-CODIGO-CLIENTE  AT 1022

           IF WS-CODIGO-CLIENTE <> ZEROS
              OPEN INPUT  ARQ-CLIENTE
              PERFORM REL-POR-COD
                 THRU F-REL-POR-COD
           ELSE
              ACCEPT WS-NOME-CLIENTE  AT 1222
              IF WS-NOME-CLIENTE <> SPACES
                 OPEN INPUT ARQ-CLIENTE
                 PERFORM REL-POR-NOME
                    THRU F-REL-POR-NOME
              ELSE
                 PERFORM REL-GERAL
                    THRU F-REL-GERAL
              END-IF
           END-IF
           ACCEPT  PAUSA               AT 2478
           DISPLAY LIMPA-TELA
           DISPLAY "RELATORIO GERADO COM SUCESSO TECLE ENTER PARA RETORN
      -    "AR AO MENU INICIAL" AT 1503
           ACCEPT  PAUSA               AT 2478.
           CLOSE ARQ-CLIENTE
           CLOSE RELATORIO

           EXIT PROGRAM.

       REL-POR-COD.
           MOVE WS-CODIGO-CLIENTE TO CLI-CODIGO
           START ARQ-CLIENTE KEY IS EQUAL CLI-CODIGO
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
                   AT 2401
              DISPLAY WS-RESULTADO-ACESSO AT 2444
              ACCEPT  PAUSA               AT 2478
              DISPLAY LIMPA-TELA          AT 2401
      *       EXIT PERFORM
           END-IF
           READ ARQ-CLIENTE

           MOVE CLI-CODIGO         TO DET-CLI-CODIGO
           MOVE CLI-CNPJ           TO DET-CLI-CNPJ
           MOVE CLI-RAZAO-SOCIAL   TO DET-CLI-RAZAO-SOCIAL
           MOVE CLI-LATITUDE       TO DET-CLI-LATITUDE
           MOVE CLI-LONGITUDE      TO DET-CLI-LONGITUDE
           WRITE LINHA FROM DETALHE AFTER 1 LINES.



       F-REL-POR-COD. EXIT.


       REL-POR-NOME.
           MOVE ZEROS TO CONTROLE-FIM
           MOVE WS-CODIGO-CLIENTE TO CLI-CODIGO
           START ARQ-CLIENTE KEY IS NOT LESS CLI-CODIGO
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
                   AT 2401
              DISPLAY WS-RESULTADO-ACESSO AT 2444
              ACCEPT  PAUSA               AT 2478
              DISPLAY LIMPA-TELA          AT 2401
      *       EXIT PERFORM
           END-IF
           PERFORM UNTIL CONTROLE-FIM = 1
              READ ARQ-CLIENTE NEXT
                 AT END
                    MOVE 1 TO CONTROLE-FIM
                    EXIT PERFORM
              END-READ
              IF WS-NOME-CLIENTE = CLI-RAZAO-SOCIAL
                 MOVE CLI-CODIGO         TO DET-CLI-CODIGO
                 MOVE CLI-CNPJ           TO DET-CLI-CNPJ
                 MOVE CLI-RAZAO-SOCIAL   TO DET-CLI-RAZAO-SOCIAL
                 MOVE CLI-LATITUDE       TO DET-CLI-LATITUDE
                 MOVE CLI-LONGITUDE      TO DET-CLI-LONGITUDE
                 WRITE LINHA FROM DETALHE AFTER 1 LINES
              END-IF
           END-PERFORM.


       F-REL-POR-NOME. EXIT.

       REL-GERAL.
         IF WS-ORDEM = "A"
            IF WS-CLASSIFICA = "C"
               SORT ARQ-CLIENTE-SORT
                    ON ASCENDING KEY CLI-CODIGO-SORT
                    USING  ARQ-CLIENTE
                    GIVING ARQ-CLIENTE-SAI
            ELSE
               SORT ARQ-CLIENTE-SORT
                    ON ASCENDING KEY CLI-RAZAO-SOCIAL-SORT
                    USING  ARQ-CLIENTE
                    GIVING ARQ-CLIENTE-SAI
            END-IF
         ELSE
            IF WS-CLASSIFICA = "C"
               SORT ARQ-CLIENTE-SORT
                    ON DESCENDING KEY CLI-CODIGO-SORT
                    USING  ARQ-CLIENTE
                    GIVING ARQ-CLIENTE-SAI
            ELSE
               SORT ARQ-CLIENTE-SORT
                    ON DESCENDING KEY CLI-RAZAO-SOCIAL-SORT
                    USING  ARQ-CLIENTE
                    GIVING ARQ-CLIENTE-SAI
            END-IF
         END-IF.
         PERFORM IMPRIME-RELATORIO
            THRU F-IMPRIME-RELATORIO.
       F-REL-GERAL. EXIT.


       IMPRIMIR-CABECALHO.
           ADD 01 TO CONTADOR-PAGINA
           MOVE CONTADOR-PAGINA TO CAB-PAGINA
           WRITE LINHA FROM CABECALHO-TITULO AFTER PAGE
           WRITE LINHA FROM CABECALHO-2      AFTER 1 LINE
           WRITE LINHA FROM CABECALHO-1      AFTER 1 LINE
           WRITE LINHA FROM CABECALHO-2      AFTER 1 LINE
           MOVE  04 TO CONTADOR-LINHA.

       IMPRIME-RELATORIO.
           OPEN INPUT ARQ-CLIENTE-SAI
           MOVE ZEROS TO CONTROLE-FIM
           PERFORM UNTIL CONTROLE-FIM = 1
              READ ARQ-CLIENTE-SAI NEXT
                 AT END
                 MOVE 1 TO CONTROLE-FIM
                 EXIT PERFORM
              END-READ
              MOVE CLI-CODIGO-SAI           TO DET-CLI-CODIGO
              MOVE CLI-CNPJ-SAI             TO DET-CLI-CNPJ
              MOVE CLI-RAZAO-SOCIAL-SAI     TO DET-CLI-RAZAO-SOCIAL
              MOVE CLI-LATITUDE-SAI         TO DET-CLI-LATITUDE
              MOVE CLI-LONGITUDE-SAI        TO DET-CLI-LONGITUDE
              WRITE LINHA FROM DETALHE AFTER 1 LINES

           END-PERFORM
           CLOSE ARQ-CLIENTE-SAI.

       F-IMPRIME-RELATORIO. EXIT.

