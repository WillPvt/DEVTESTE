       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG05.
       AUTHOR. WILKSON SILVA.
       DATE-WRITTEN. 18/01/2020.
      *      ******** PROGRAMA DE DISTRIBUICAO DE CARTEIRA CLIENTES

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

           SELECT ARQ-DIST ASSIGN TO DISK WS-LOCAL-ARQ-DIST
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY "ARQ-CLIENTE.FD".

       COPY "ARQ-VENDEDOR.FD".

       FD ARQ-DIST.
          01 REGISTRO-DIST.
             02 DIST-CLI-CODIGO            PIC 9(07).
             02 FILLER                     PIC X(01) VALUE ";".
             02 DIST-CLI-RAZAO-SOCIAL      PIC X(40).
             02 FILLER                     PIC X(01) VALUE ";".
             02 DIST-VEN-CODIGO            PIC 9(03).
             02 FILLER                     PIC X(01) VALUE ";".
             02 DIST-VEN-RAZAO-SOCIAL      PIC X(40).
             02 FILLER                     PIC X(01) VALUE ";".
             02 DIST-DISTANCIA             PIC S9(03)V9(08).

       WORKING-STORAGE SECTION.
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
       77 ERRO-VALIDACAO        PIC X VALUE SPACES.

       77 LAT1                  PIC S9(03)V9(08).
       77 LON1                  PIC S9(03)V9(08).
       77 LAT2                  PIC S9(03)V9(08).
       77 LON2                  PIC S9(03)V9(08).
       77 DLAT                  PIC S9(03)V9(08).
       77 DLON                  PIC S9(03)V9(08).
       77 RAIO                  PIC 9(04) VALUES 6371.
       77 A                     PIC S9(09)V9(08).
       77 C                     PIC S9(09)V9(08).
       77 D                     PIC S9(09)V9(08).
       77 R                     PIC S9(09)V9(08).
       77 DISTANCIA-ANTERIOR    PIC S9(03)V9(08).


       LINKAGE SECTION.
       77 DATA-DE-HOJE          PIC 99/99/99.

       SCREEN SECTION.
       01 LIMPA-TELA   BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.

       01 TELA-DISTRIBUI BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "  DISTRIBUICAO DA CARTEIRA DE CLIENTES ".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.

       01 TELA-FIM BLANK SCREEN
                       BACKGROUND-COLOR 1
                       FOREGROUND-COLOR 7.
          02 LINE 01 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 02 COLUMN 01 PIC X(08) FROM DATA-DE-HOJE.
          02 LINE 02 COLUMN 25 VALUE
             "  DISTRIBUICAO DE CLIENTES ".
          02 LINE 03 COLUMN 01 PIC X(80) FROM LINHA-TRACO.
          02 LINE 15 COLUMN 01
          VALUE "PROCESSO DE DISTRIBUICAO CONCLUIDO, TECLE ENTER PARA
      -"RETORNAR AO MENU INICIAL ".

       PROCEDURE DIVISION USING DATA-DE-HOJE.

       INICIO.
           DISPLAY TELA-DISTRIBUI AT 0101

           MOVE "DIST.CSV"       TO WS-LOCAL-ARQ-DIST
           OPEN OUTPUT ARQ-DIST

           MOVE "CLIENTE.DAT"   TO WID-ARQ-CLIENTE
           OPEN I-O ARQ-CLIENTE
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-CLIENTE
              CLOSE ARQ-CLIENTE
              OPEN I-O ARQ-CLIENTE
           END-IF

           MOVE "VENDEDOR.DAT"   TO WID-ARQ-VENDEDOR
           OPEN I-O ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = 00
              OPEN OUTPUT ARQ-VENDEDOR
              CLOSE ARQ-VENDEDOR
              OPEN I-O ARQ-VENDEDOR
           END-IF

           MOVE ZEROS TO CLI-CODIGO
           START ARQ-CLIENTE KEY NOT LESS CLI-CODIGO
           IF WS-RESULTADO-ACESSO NOT = 00
              DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
              AT 2401
              DISPLAY WS-RESULTADO-ACESSO AT 2440
              ACCEPT  PAUSA               AT 2478
              DISPLAY LIMPA-TELA          AT 2401
           ELSE
              PERFORM UNTIL EXIT
                 READ ARQ-CLIENTE NEXT AT END
                   EXIT PERFORM
                 END-READ

                 MOVE ZEROS            TO DISTANCIA-ANTERIOR
                 PERFORM DISTANCIA-VENDEDOR THRU F-DISTANCIA-VENDEDOR
              END-PERFORM
           END-IF
           CLOSE ARQ-DIST
           CLOSE ARQ-CLIENTE
           CLOSE ARQ-VENDEDOR
           DISPLAY TELA-FIM AT 0101
           ACCEPT  PAUSA    AT 2478
           EXIT PROGRAM.

       DISTANCIA-VENDEDOR.
           MOVE ZEROS TO VEN-CODIGO
              START ARQ-VENDEDOR KEY NOT LESS VEN-CODIGO
              IF WS-RESULTADO-ACESSO NOT = 00
                 DISPLAY "ERRO NO POSICIONAMENTO DA CHAVE - CLIENTE: "
                      AT 2401
                 DISPLAY WS-RESULTADO-ACESSO AT 2440
                 ACCEPT  PAUSA               AT 2478
                 DISPLAY LIMPA-TELA          AT 2401
              ELSE
                 PERFORM UNTIL EXIT
                    READ ARQ-VENDEDOR NEXT AT END
                    EXIT PERFORM
                    END-READ
                    PERFORM CALCULA-DISTANCIA THRU F-CALCULA-DISTANCIA
                    IF DIST-DISTANCIA < DISTANCIA-ANTERIOR  OR DISTANCIA
      --ANTERIOR = 0
                       MOVE CLI-CODIGO       TO DIST-CLI-CODIGO
                       MOVE CLI-RAZAO-SOCIAL TO DIST-CLI-RAZAO-SOCIAL
                       MOVE VEN-CODIGO       TO DIST-VEN-CODIGO
                       MOVE VEN-NOME         TO DIST-VEN-RAZAO-SOCIAL
                       MOVE DIST-DISTANCIA   TO DISTANCIA-ANTERIOR
                    END-IF
                 END-PERFORM
                 WRITE REGISTRO-DIST
                 MOVE SPACES TO REGISTRO-DIST

              END-IF.
       F-DISTANCIA-VENDEDOR. EXIT.

       CALCULA-DISTANCIA.
          MOVE CLI-LATITUDE     TO LAT1
          MOVE CLI-LONGITUDE    TO LON1
          MOVE VEN-LATITUDE     TO LAT2
          MOVE VEN-LONGITUDE    TO LON2
          MOVE 6371 TO R

          COMPUTE DLAT = (LAT2 - LAT1) * (FUNCTION PI / 180)
          COMPUTE DLON = (LON2 - LON1) * (FUNCTION PI / 180).

          COMPUTE A = FUNCTION SIN(DLAT / 2) * FUNCTION SIN(DLAT / 2) +
      - FUNCTION COS(LAT1 * (FUNCTION PI / 180)) * FUNCTION COS(LAT2 *
      - (FUNCTION PI / 180)) * FUNCTION SIN(DLON / 2) * FUNCTION SIN(
      - DLON / 2).
          COMPUTE C = 2 * FUNCTION ATAN(FUNCTION SQRT(A))
        COMPUTE D = (R * C) / 1000.
        MOVE D TO DIST-DISTANCIA.
       F-CALCULA-DISTANCIA. EXIT.
