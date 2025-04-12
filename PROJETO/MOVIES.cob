       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILMES.
      *======================================
      *== AUTOR: MATHEUS          EMPRESA: X
      *== OBJETIVO: Sistema de Gestão de Filmes
      *== DATA = 10/04/2025
      *== OBSERVAÇOES: Sistema CRUD
      *======================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILMES ASSIGN TO 'C:\cobol\PROJETO\FILMES.DAT'
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             FILE STATUS IS FILME-STATUS
             RECORD KEY IS FILMES-CHAVE.

       DATA DIVISION.
       FILE                         SECTION.
      *======ESTRUTURA DO REGISTRO======
        FD FILMES.
        01 FILMES-REG.
           05 FILMES-CHAVE.
             10 FILME-ID       PIC 9(05).
           05 FILME-TITULO     PIC X(50).
           05 FILME-GENERO     PIC X(30).
           05 FILME-DURACAO    PIC 9(03).
           05 FILME-DISTRIB    PIC X(40).
           05 FILME-NOTA       PIC 9(02).


      *============ VÁRIAVEIS ===============
       WORKING-STORAGE              SECTION.

        77 WRK-OPCAO          PIC X(1).
        77 WRK-MODULO         PIC X(25).
        77 WRK-TECLA          PIC X(1).
        77 FILME-STATUS       PIC 9(02).
        77 WRK-MSGERRO        PIC X(50).
        77 WRK-CONTREGISTRO   PIC 9(05) VALUE 0.
        77 LINE-CONTADOR      PIC 99 VALUE 5.


       SCREEN                       SECTION.
      *============ Estruta da Tela =========
        01  TELA.
           05 LIMPA-TELA.
              10 BLANK SCREEN.
              10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                 BACKGROUND-COLOR 2.
              10 LINE 01 COLUMN 15 PIC X(20) BACKGROUND-COLOR 2
                 FOREGROUND-COLOR 7 FROM 'SISTEMA DE FILMES'.
              10 LINE 02 COLUMN 01 PIC X(20) ERASE EOL
                 BACKGROUND-COLOR 1 FROM WRK-MODULO.

      *============ Tela de MENU ===========
        01 MENU.
           05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           05 LINE 08 COLUMN 15 VALUE '2 - CONSULTA'.
           05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
           05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
           05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
           05 LINE 12 COLUMN 15 VALUE 'X - SAIDA'.
           05 LINE 13 COLUMN 15 VALUE 'OPCAO......:'.
           05 LINE 13 COLUMN 28 USING WRK-OPCAO.

      *============ Tela para Registro de Filmes ===========
        01 TELA-REGISTRO.
             05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'ID'.
               10 COLUMN PLUS 2 PIC 9(05) USING FILME-ID
                   BLANK WHEN ZEROS.
             05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'TITULO....'.
               10 COLUMN PLUS 2 PIC X(50) USING FILME-TITULO.
               10 LINE 12 COLUMN 10 VALUE 'GENERO...'.
               10 COLUMN PLUS 2 PIC X(30) USING FILME-GENERO.
               10 LINE 13 COLUMN 10 VALUE 'DURACAO...'.
               10 COLUMN PLUS 2 PIC 9(03) USING FILME-DURACAO.
               10 LINE 14 COLUMN 10 VALUE 'DISTRIB...'.
               10 COLUMN PLUS 2 PIC X(40) USING FILME-DISTRIB.
               10 LINE 15 COLUMN 10 VALUE 'NOTA...'.
               10 COLUMN PLUS 2 PIC 9(02) USING FILME-NOTA.

      *============ Mensagens de Erro ou Confirmação ===========
        01 MOSTRA-ERRO.
             02 MSG-ERRO.
               10 LINE 18 COLUMN 01 ERASE EOL
                 BACKGROUND-COLOR 3.
               10 LINE 18 COLUMN 16 PIC X(50)
                          BACKGROUND-COLOR 3
                          FROM WRK-MSGERRO.
               10 COLUMN PLUS 2 PIC X(01) BACKGROUND-COLOR 3
                 USING WRK-TECLA.

      *============ Procedimentos Principais ===========
       PROCEDURE DIVISION.
       0001-PRINCIPAL               SECTION.
            PERFORM 1000-INICIAR THRU 1100-MONTATELA.
            PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO EQUAL 'X' or 'x'.
            PERFORM 3000-FINALIZAR.
            STOP RUN.

       1000-INICIAR.
            OPEN I-O FILMES.
            IF FILME-STATUS NOT = 0
              DISPLAY 'ERRO AO ABRIR O ARQUIVO, STATUS = ' FILME-STATUS
              STOP RUN
            END-IF.

       1100-MONTATELA.

             DISPLAY TELA.
             ACCEPT MENU.

       2000-PROCESSAR.
            MOVE ZEROS TO FILME-ID FILME-DURACAO FILME-NOTA
            WRK-CONTREGISTRO.
            MOVE SPACES TO FILME-TITULO FILME-GENERO FILME-DISTRIB
            MOSTRA-ERRO.
            EVALUATE WRK-OPCAO
               WHEN 1
                PERFORM 5000-INCLUIR
                MOVE SPACES TO WRK-MODULO
               WHEN 2
                PERFORM 6000-CONSULTAR
                MOVE SPACES TO WRK-MODULO
               WHEN 3
                 PERFORM 7000-ALTERAR
                 MOVE SPACES TO WRK-MODULO
               WHEN 4
                 PERFORM 8000-EXCLUIR
                 MOVE SPACES TO WRK-MODULO
               WHEN 5
                 PERFORM 9000-RELATORIOTELA
                 MOVE SPACES TO WRK-MODULO
               WHEN OTHER
               IF WRK-OPCAO NOT EQUAL MENU
                 MOVE 'ENTRE COM UMA OPCAO VALIDA.' TO WRK-MSGERRO
                 DISPLAY MOSTRA-ERRO
                 ACCEPT MSG-ERRO

               END-IF
            END-EVALUATE.
           PERFORM 1100-MONTATELA.

       3000-FINALIZAR.
           CLOSE FILMES.

       5000-INCLUIR.
           MOVE '  MODULO - INCLUSAO ' TO WRK-MODULO.
           DISPLAY TELA.
             ACCEPT TELA-REGISTRO.
             DISPLAY 'CONFIRMA GRAVACAO? (S/N)' AT 1610.
             ACCEPT WRK-TECLA AT 1637.

             IF WRK-TECLA EQUAL 'S' OR 's'
               WRITE FILMES-REG
                 INVALID KEY
                   MOVE SPACES TO WRK-TECLA
                   MOVE 'FILME JA CADASTRADO!' TO WRK-MSGERRO
                 NOT INVALID KEY
                   MOVE SPACES TO WRK-TECLA
                   MOVE 'FILME CADASTRADO COM SUCESSO!' TO WRK-MSGERRO
               END-WRITE
             ELSE
                 MOVE SPACES TO WRK-TECLA
                 MOVE  'GRAVACAO CANCELADA!' TO WRK-MSGERRO
             END-IF.
                DISPLAY MOSTRA-ERRO.
                ACCEPT MSG-ERRO.

       6000-CONSULTAR.
           MOVE '  MODULO - CONSULTA ' TO WRK-MODULO.
           DISPLAY TELA.
           PERFORM UNTIL WRK-TECLA = 'N' OR WRK-TECLA = 'n'
           MOVE ZEROS TO  FILME-ID
           MOVE SPACES TO WRK-TECLA SS-DADOS
           DISPLAY "Digite o ID do Filme para consulta: " AT 0810
           ACCEPT CHAVE
               READ FILMES KEY IS FILMES-CHAVE
                 INVALID KEY
                     MOVE 'FILME NAO ENCOTRADO' TO WRK-MSGERRO
                     DISPLAY MOSTRA-ERRO
                 NOT INVALID KEY
                     MOVE 'FILME ENCOTRADO' TO WRK-MSGERRO
                     DISPLAY MOSTRA-ERRO
                     DISPLAY SS-DADOS
               END-READ
               DISPLAY "Deseja realizar outra consulta? (S/N): " AT 1610
               ACCEPT WRK-TECLA AT 1652

              END-PERFORM.
                 DISPLAY MOSTRA-ERRO.


       7000-ALTERAR.
           MOVE '  MODULO - ALTERAR ' TO WRK-MODULO.
            DISPLAY TELA.
            DISPLAY "Digite o ID do Filme para alterar: " AT 0810.
            ACCEPT CHAVE

           READ FILMES
             IF FILME-STATUS EQUAL 0 THEN
                ACCEPT SS-DADOS
                DISPLAY "Confirma alteracao? (S/N): " AT 1810.
                ACCEPT WRK-TECLA AT 1837.
                IF WRK-TECLA = "S" OR WRK-TECLA = "s"
                 REWRITE FILMES-REG
                     INVALID KEY
                         MOVE "Registro nao alterado" TO WRK-MSGERRO
                         DISPLAY MOSTRA-ERRO
                         ACCEPT MSG-ERRO
                     NOT INVALID KEY
                         MOVE "Registro alterado" TO WRK-MSGERRO
                         DISPLAY MOSTRA-ERRO
                         ACCEPT MSG-ERRO
                 END-REWRITE
             ELSE
                 MOVE "Registro nao alterado" TO WRK-MSGERRO
                 DISPLAY MOSTRA-ERRO
                 ACCEPT MSG-ERRO
             END-IF.




       8000-EXCLUIR.
           MOVE '  MODULO - EXCLUSAO ' TO WRK-MODULO.
           DISPLAY TELA.
           DISPLAY "Digite o ID do filme a excluir: " AT 0810.
           ACCEPT CHAVE.
           READ FILMES
                   INVALID KEY
                      MOVE "Filme nao cadastrado" TO WRK-MSGERRO
                      DISPLAY MOSTRA-ERRO
                      ACCEPT MSG-ERRO
                   NOT INVALID KEY
                     DISPLAY SS-DADOS
                     DISPLAY "Deseja excluir? (S/N): " AT 1810
                     ACCEPT WRK-TECLA AT 1837.

           IF (WRK-TECLA = "S" OR WRK-TECLA = "s") AND FILME-STATUS = 0
           DELETE FILMES
             NOT INVALID KEY
                 MOVE "Filme excluido" TO WRK-MSGERRO
                 DISPLAY MOSTRA-ERRO
              END-DELETE
           ELSE
           MOVE "Filme nao excluido" TO WRK-MSGERRO
           DISPLAY MOSTRA-ERRO
           END-IF.
           ACCEPT MSG-ERRO.


       9000-RELATORIOTELA.

           MOVE '  MODULO - RELATORIO ' TO WRK-MODULO.
           DISPLAY TELA.
           MOVE 5 TO LINE-CONTADOR.
           MOVE 00001 TO FILME-ID.
           START FILMES KEY EQUAL FILME-ID.
             READ FILMES
               INVALID KEY
                   MOVE 'NENHUM REGISTRO ENCOTRADO' TO WRK-MSGERRO
               NOT INVALID KEY
                   DISPLAY ' RELATORIO DE FILMES ' AT 0224
                   DISPLAY '-----------------------------------' AT 0301
                   DISPLAY '-----------------------------------' AT 0336
                   DISPLAY '--------------' AT 0371
                   PERFORM UNTIL FILME-STATUS EQUAL 10
                   ADD 1 TO WRK-CONTREGISTRO
                       DISPLAY FILME-ID AT LINE LINE-CONTADOR
                           COLUMN 5
                       DISPLAY FILME-TITULO AT LINE LINE-CONTADOR
                            COLUMN 13
                       DISPLAY FILME-GENERO AT LINE LINE-CONTADOR
                            COLUMN 46
                       DISPLAY FILME-DURACAO AT LINE LINE-CONTADOR
                            COLUMN 59
                       DISPLAY FILME-DISTRIB AT LINE LINE-CONTADOR
                            COLUMN 65
                       DISPLAY FILME-NOTA AT LINE LINE-CONTADOR
                            COLUMN 82
                       ADD 2 TO LINE-CONTADOR
                       READ FILMES NEXT
                        IF LINE-CONTADOR > 16
                         MOVE 'PRESSIONE ALGUMA TECLA' TO WRK-MSGERRO
                         DISPLAY MOSTRA-ERRO
                         ACCEPT MOSTRA-ERRO
                         MOVE 'MODULO - RELATORIO ' TO WRK-MODULO
                         DISPLAY TELA
                   DISPLAY ' RELATORIO DE FILMES ' AT 0224
                   DISPLAY '-----------------------------------' AT 0301
                   DISPLAY '-----------------------------------' AT 0336
                   DISPLAY '--------------' AT 0371
                            MOVE 5 TO LINE-CONTADOR
                         END-IF
                   END-PERFORM
             END-READ.
               MOVE 'REGISTROS LIDOS' TO WRK-MSGERRO.
               MOVE WRK-CONTREGISTRO TO WRK-MSGERRO(17:05).
               DISPLAY MOSTRA-ERRO.
                     ACCEPT MSG-ERRO.
