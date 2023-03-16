       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
           SELECT PRODUTO ASSIGN TO "C:\COBOL\PRODUTOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS ID-PRODUTO
           FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD PRODUTO.
       01 REG-CONTATOS.
           03 ID-PRODUTO     PIC 9(3).
           03 NOME-PRODUTO   PIC X(15).
           03 PRC-PRODUTO    PIC 99(4)V99.
           03 IVA-PRODUTO    PIC 99.
           03 QT-PRODUTO     PIC 9(4).
           03 PF-PRODUTO     PIC 9(5)V99.
           03 PRC-COM-IVA    PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 PARAMETRES.
       02 PA-RETURN-CODE PIC 99 VALUE 0.
       01 WS-REG-CONTATO   PIC X(22).
       01 FILLER REDEFINES WS-REG-CONTATO.
           03 WS-ID-PRODUTO     PIC 9(3).
           03 WS-NOME-PRODUTO   PIC X(15).
           03 WS-PRC-PRODUTO    PIC 99(4)V99.
           03 WS-IVA-PRODUTO    PIC 99.
           03 WS-QT-PRODUTO     PIC 9(4).
           03 WS-PF-PRODUTO     PIC 9(5)V99.
       01 WS-MENU-OPCAO         PIC 9.
       77 WS-FS               PIC 99.
          88 FS-OK           VALUE 0.
       77 WS-EOF              PIC X.
           88 EOF-OK          VALUE 'F' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "MENU:"
            DISPLAY "1 - INSERIR NOVO PRODUTO"
            DISPLAY "2 - LISTAR PRODUTOS"
            DISPLAY "3 - ELEMINAR PRODUTO"
            DISPLAY "4 - BUSCA PRODUTO"
            DISPLAY "5 - ALTERAR PRODUTO"
            DISPLAY "6 - SAIR"

            ACCEPT WS-MENU-OPCAO

       EVALUATE WS-MENU-OPCAO
       WHEN 1
            PERFORM INSERIR
            GO TO MAIN-PROCEDURE


            WHEN 2

             CALL "LEITURA"
             USING PARAMETRES
             GO TO MAIN-PROCEDURE


           WHEN 3
           PERFORM ELEMINAR
           GO TO MAIN-PROCEDURE

            WHEN 4
            PERFORM CONSULTA
            GO TO MAIN-PROCEDURE

            WHEN 5
            PERFORM ALTERAR
            GO TO MAIN-PROCEDURE

            WHEN 6
            STOP RUN

            WHEN OTHER
            DISPLAY "escolha uma opcao"
            GO TO MAIN-PROCEDURE

       end-evaluate.

       INSERIR.
             DISPLAY "ID".
             ACCEPT WS-ID-PRODUTO

             DISPLAY "NOME DO PRODUTO: ".
             ACCEPT WS-NOME-PRODUTO.

             DISPLAY "PRECO DO PRODUTO: ".
             ACCEPT WS-PRC-PRODUTO.

             DISPLAY "IVA: ".
             ACCEPT WS-IVA-PRODUTO.

             DISPLAY "QUANTIDADE: ".
             ACCEPT WS-QT-PRODUTO.


             COMPUTE WS-PF-PRODUTO = WS-PRC-PRODUTO + WS-PRC-PRODUTO *
             WS-IVA-PRODUTO / 100


             OPEN I-O PRODUTO
           IF WS-FS EQUAL 35 THEN
               OPEN OUTPUT PRODUTO
           END-IF

           IF FS-OK THEN
             MOVE WS-ID-PRODUTO TO ID-PRODUTO
             MOVE WS-NOME-PRODUTO TO NOME-PRODUTO
             MOVE WS-PRC-PRODUTO TO PRC-PRODUTO
             MOVE WS-IVA-PRODUTO TO IVA-PRODUTO
             MOVE WS-QT-PRODUTO TO QT-PRODUTO
             MOVE WS-PF-PRODUTO TO PF-PRODUTO
            WRITE REG-CONTATOS
           INVALID KEY
           DISPLAY "CHAVE JA REGISTADA"
            NOT INVALID KEY
           DISPLAY "PRODUTO GRAVADO COM SUCESSO"
           END-WRITE
             ELSE
            DISPLAY "ERRO DE GRAVACAO"
            DISPLAY "ERRO DO FILE STATUS:" WS-FS
            END-IF
            CLOSE PRODUTO.


       ELEMINAR.
           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE

           OPEN I-O PRODUTO

           IF FS-OK THEN
             DISPLAY "QUAL O CODIGO:"
             ACCEPT ID-PRODUTO
             READ PRODUTO INTO WS-REG-CONTATO
             KEY IS ID-PRODUTO

           INVALID KEY
           DISPLAY "CONTATO NAO EXISTE"
            NOT INVALID KEY
            DISPLAY "NOME ATUAL" WS-NOME-PRODUTO


            DELETE PRODUTO RECORD
            DISPLAY "REGISTO APAGADO"
            CLOSE PRODUTO

            END-READ
            ELSE
                DISPLAY "ERROR AO ABRIR FICHEIRO"
                DISPLAY "ERRO FILE STATUS" WS-FS
                END-IF
                CLOSE PRODUTO.
       CONSULTA.

           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE

           OPEN INPUT PRODUTO

           IF FS-OK THEN
             DISPLAY "QUAL O CODIGO:"
             ACCEPT ID-PRODUTO
             READ PRODUTO INTO WS-REG-CONTATO
             KEY IS ID-PRODUTO

           INVALID KEY
           DISPLAY "PRODUTO NAO EXISTE"
            NOT INVALID KEY
            DISPLAY WS-ID-PRODUTO  " - " WS-NOME-PRODUTO
            END-READ
            ELSE
                DISPLAY "ERROR AO ABRIR FICHEIRO"
                DISPLAY "ERRO FILE STATUS" WS-FS
                END-IF
                CLOSE PRODUTO.

       ALTERAR.
           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE

           OPEN I-O PRODUTO

           IF FS-OK THEN
             DISPLAY "QUAL O CODIGO:"
             ACCEPT ID-PRODUTO
             READ PRODUTO INTO WS-REG-CONTATO
             KEY IS ID-PRODUTO

           INVALID KEY
           DISPLAY "CONTATO NAO EXISTE"
            NOT INVALID KEY
            DISPLAY "PRODUTO ATUAL " WS-NOME-PRODUTO
            DISPLAY "NOVO NOME"
            ACCEPT NOME-PRODUTO
            REWRITE REG-CONTATOS
            DISPLAY "PRODUTO ALTERADO!"
            CLOSE PRODUTO
            END-READ
            ELSE
                DISPLAY "ERROR AO ABRIR FICHEIRO"
                DISPLAY "ERRO FILE STATUS" WS-FS
                END-IF
                CLOSE PRODUTO.


       END PROGRAM YOUR-PROGRAM-NAME.
