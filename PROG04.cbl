       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG04.
       AUTHOR. WILKSON SILVA.
       DATE-WRITTEN. 16/01/2020.
      * ------------ LISTAGEM DE VENDEDOR --------------

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
                  ORGANIZATION     IS INDEXED
                  RECORD KEY       IS VEN-CODIGO
                  ACCESS MODE      IS DYNAMIC
                  LOCK MODE        IS MANUAL
                  FILE STATUS      IS WS-RESULTADO-ACESSO.

           SELECT ARQ-VENDEDOR-SORT ASSIGN TO "SORT".

           SELECT ARQ-VENDEDOR-SAI ASSIGN TO "ARQ-VENDEDOR-SAI.DAT"
                  ORGANIZATION     IS SEQUENTIAL.

           SELECT RELATORIO ASSIGN TO "RVENDEDOR.TXT"
                            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       COPY "ARQ-VENDEDOR.FD".

       FD RELATORIO.
       01 LINHA        PIC X(132).

       SD ARQ-VENDEDOR-SORT.
       01 REG-ARQ-VENDEDOR-SORT.
          02 VEN-CODIGO-SORT            PIC 9(03).
          02 VEN-CPF-SORT              PIC 9(11).
          02 VEN-RAZAO-SOCIAL-SORT      PIC X(40).
          02 VEN-LATITUDE-SORT          PIC s9(03)V9(08).
          02 VEN-LONGITUDE-SORT         PIC s9(03)V9(08).


       FD ARQ-VENDEDOR-SAI.
       01 REG-ARQ-VENDEDOR-SAI.
          02 VEN-CODIGO-SAI            PIC 9(03).
          02 VEN-CPF-SAI               PIC 9(11).
          02 VEN-RAZAO-SOCIAL-SAI      PIC X(40).
          02 VEN-LATITUDE-SAI          PIC s9(03)V9(08).
          02 VEN-LONGITUDE-SAI         PIC s9(03)V9(08).

       WORKING-STORAGE SECTION.
      *VARIAVEIS DA TELA
       77 WS-ORDEM              PIC X(01) VALUE SPACES.
       77 WS-CLASSIFICA         PIC X(01) VALUE SPACES.
       77 WS-CODIGO-VENDEDOR    PIC 9(03) VALUE ZEROS.
       77 WS-NOME-VENDEDOR      PIC X(40) VALUE SPACES.
      *-----------------------------------------------------------------
       77 LINHA-TRACO           PIC X(80) VALUE ALL '-'.
       77 CONTADOR-LINHA        PIC 9(02) VALUE ZERO.
       77 CONTADOR-PAGINA       PIC 9(03) VALUE ZERO.
       77 CONTROLE-FIM          PIC 9(01) VALUE ZEROS.
       77 PAUSA                 PIC X(01).
       77 WS-RESULTADO-ACESSO   PIC 9(02) VALUE ZEROS.
       01 CABECALHO-1.
          02 FILLER PIC X(06) VALUE "CODIGO".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(30) VALUE " NOME VENDEDOR".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(18) VALUE "           C.N.P.J".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(15) VALUE "       LATITUDE".
          02 FILLER PIC X(03) VALUE SPACES.
          02 FILLER PIC X(12) VALUE "   LONGITUDE".

       01 CABECALHO-2.
          02 FILLER PIC X(132) VALUES ALL "-".

       01 DETALHE.
          02 DET-VEN-CODIGO            PIC 9(03).
          02 FILLER                    PIC X(07) VALUE SPACES.
          02 DET-VEN-RAZAO-SOCIAL      PIC X(40).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-VEN-CPF               PIC 9(11).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-VEN-LATITUDE          PIC s9(03)V9(08).
          02 FILLER                    PIC X(03) VALUE SPACES.
          02 DET-VEN-LONGITUDE         PIC s9(03)V9(08).

       01 CABECALHO-TITULO.
          02 CAB-DATA     PIC X(08).
          02 FILLER       PIC X(41) VALUE SPACES.
          02 FILLER       PIC X(21) VALUE "RELACAO DE VENDEDORES".
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
             "     Relatorio de Vendedores     ".
          02 LINE 02 COLUMN 73 VALUE "PROG04".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 04 COLUMN 01 VALUE "ORDENACAO ASCENDENTE (A) OU DESCEN
      -"DENTE (D)?....... ".
          02 LINE 06 COLUMN 01 VALUE "CLASSIFICACAO POR CODIGO (C) OU RA
      -"ZAO SOCIAL (R)?.. ".
          02 LINE 08 COLUMN 01 VALUE
       "********************************FILTROS*************************
      -"****************".
          02 LINE 10 COLUMN 01 VALUE "CODIGO DO VENDEDOR...".
          02 LINE 12 COLUMN 01 VALUE "RAZAO SOCIAL........".



       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.
           MOVE "VENDEDOR.DAT"   TO WID-ARQ-VENDEDOR
           OPEN OUTPUT RELATORIO
           MOVE DATA-DE-HOJE TO CAB-DATA
           PERFORM IMPRIMIR-CABECALHO
           DISPLAY TELA-RELATORIO.

       ACCEPT-TELA.
           PERFORM UNTIL WS-ORDEM = "A" OR = "D"
              ACCEPT WS-ORDEM AT 0453
           END-PERFORM

           PERFORM UNTIL WS-CLASSIFICA = "C" OR = "R"
              ACCEPT WS-CLASSIFICA AT 0653
           END-PERFORM

           ACCEPT WS-CODIGO-VENDEDOR  AT 1022

           IF WS-CODIGO-VENDEDOR <> ZEROS
              OPEN INPUT ARQ-VENDEDOR
              PERFORM REL-POR-COD
                 THRU F-REL-POR-COD
           ELSE
              ACCEPT WS-NOME-VENDEDOR  AT 1222
              IF WS-NOME-VENDEDOR <> SPACES
                 OPEN INPUT ARQ-VENDEDOR
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
           CLOSE ARQ-VENDEDOR
           CLOSE RELATORIO

           EXIT PROGRAM.

       REL-POR-COD.
           MOVE WS-CODIGO-VENDEDOR TO VEN-CODIGO
           START ARQ-VENDEDOR KEY IS EQUAL VEN-CODIGO
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - VENDEDOR: "
                   AT 2401
              DISPLAY WS-RESULTADO-ACESSO AT 2444
              ACCEPT  PAUSA               AT 2478
              DISPLAY LIMPA-TELA          AT 2401
      *       EXIT PERFORM
           END-IF
           READ ARQ-VENDEDOR

           MOVE VEN-CODIGO         TO DET-VEN-CODIGO
           MOVE VEN-CPF            TO DET-VEN-CPF
           MOVE VEN-NOME           TO DET-VEN-RAZAO-SOCIAL
           MOVE VEN-LATITUDE       TO DET-VEN-LATITUDE
           MOVE VEN-LONGITUDE      TO DET-VEN-LONGITUDE
           WRITE LINHA FROM DETALHE AFTER 1 LINES.

       F-REL-POR-COD. EXIT.


       REL-POR-NOME.
           MOVE ZEROS TO CONTROLE-FIM
           MOVE WS-CODIGO-VENDEDOR TO VEN-CODIGO
           START ARQ-VENDEDOR KEY IS NOT LESS VEN-CODIGO
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - VENDEDOR: "
                   AT 2401
              DISPLAY WS-RESULTADO-ACESSO AT 2444
              ACCEPT  PAUSA               AT 2478
              DISPLAY LIMPA-TELA          AT 2401
      *       EXIT PERFORM
           END-IF
           PERFORM UNTIL CONTROLE-FIM = 1
              READ ARQ-VENDEDOR NEXT
                 AT END
                    MOVE 1 TO CONTROLE-FIM
                    EXIT PERFORM
              END-READ
              IF WS-NOME-VENDEDOR = VEN-NOME
                 MOVE VEN-CODIGO         TO DET-VEN-CODIGO
                 MOVE VEN-CPF            TO DET-VEN-CPF
                 MOVE VEN-NOME           TO DET-VEN-RAZAO-SOCIAL
                 MOVE VEN-LATITUDE       TO DET-VEN-LATITUDE
                 MOVE VEN-LONGITUDE      TO DET-VEN-LONGITUDE
                 WRITE LINHA FROM DETALHE AFTER 1 LINES
              END-IF
           END-PERFORM.


       F-REL-POR-NOME. EXIT.

       REL-GERAL.
         IF WS-ORDEM = "A"
            IF WS-CLASSIFICA = "C"
               SORT ARQ-VENDEDOR-SORT
                    ON ASCENDING KEY VEN-CODIGO-SORT
                    USING  ARQ-VENDEDOR
                    GIVING ARQ-VENDEDOR-SAI
            ELSE
               SORT ARQ-VENDEDOR-SORT
                    ON ASCENDING KEY VEN-RAZAO-SOCIAL-SORT
                    USING  ARQ-VENDEDOR
                    GIVING ARQ-VENDEDOR-SAI
            END-IF
         ELSE
            IF WS-CLASSIFICA = "C"
               SORT ARQ-VENDEDOR-SORT
                    ON DESCENDING KEY VEN-CODIGO-SORT
                    USING  ARQ-VENDEDOR
                    GIVING ARQ-VENDEDOR-SAI
            ELSE
               SORT ARQ-VENDEDOR-SORT
                    ON DESCENDING KEY VEN-RAZAO-SOCIAL-SORT
                    USING  ARQ-VENDEDOR
                    GIVING ARQ-VENDEDOR-SAI
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
           OPEN INPUT ARQ-VENDEDOR-SAI
           MOVE ZEROS TO CONTROLE-FIM
           PERFORM UNTIL CONTROLE-FIM = 1
              READ ARQ-VENDEDOR-SAI NEXT RECORD INTO
                 REG-ARQ-VENDEDOR-SAI AT END
                 MOVE 1 TO CONTROLE-FIM
                 EXIT PERFORM
              END-READ
              MOVE VEN-CODIGO-SAI           TO DET-VEN-CODIGO
              MOVE VEN-CPF-SAI              TO DET-VEN-CPF
              MOVE VEN-RAZAO-SOCIAL-SAI     TO DET-VEN-RAZAO-SOCIAL
              MOVE VEN-LATITUDE-SAI         TO DET-VEN-LATITUDE
              MOVE VEN-LONGITUDE-SAI        TO DET-VEN-LONGITUDE
              WRITE LINHA FROM DETALHE AFTER 1 LINES

           END-PERFORM
           CLOSE ARQ-VENDEDOR-SAI.
       F-IMPRIME-RELATORIO. EXIT.

