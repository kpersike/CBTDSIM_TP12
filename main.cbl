       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBTDSIM-TP12.
       AUTHOR "Adriano Junior de Souza Almeida (CB3030644)"
              "Kaik Persike Maiorquino (CB3029689)".


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTES ASSIGN TO "CLIENTES.DAT"
            ORGANIZATION IS INDEXED
            ACCESS MODE IS DYNAMIC
            RECORD KEY IS CLI-CODIGO
            ALTERNATE RECORD KEY IS CLI-CPF
            FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTES.
           01 REG-CLIENTE.
           05 CLI-CODIGO       PIC 9(5).
           05 CLI-NOME         PIC X(40).
           05 CLI-CPF          PIC X(11).
           05 CLI-TELEFONE     PIC X(15).
           05 CLI-CIDADE       PIC X(30).
           05 CLI-ESTADO       PIC X(2).

       WORKING-STORAGE SECTION.
       01 WS-STATUS           PIC XX.
       01 WS-OPCAO            PIC 9.
       01 WS-TIPO-CONSULTA    PIC 9.
       01 WS-FIM              PIC 9 VALUE 0.
       01 WS-CONTADOR         PIC 9(5).
       01 WS-CODIGO           PIC 9(5).
       01 WS-CPF              PIC X(11).
       01 WS-NOVO-CLIENTE.
            05 WS-NOME          PIC X(40).
            05 WS-CPF-NOVO      PIC X(11).
            05 WS-TELEFONE      PIC X(15).
            05 WS-CIDADE        PIC X(30).
            05 WS-ESTADO        PIC X(2).

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM ABRIR-ARQUIVO

           IF WS-STATUS NOT = "00"
               DISPLAY "ERRO AO ABRIR ARQUIVO: " WS-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL WS-OPCAO = 6
               PERFORM MENU-PRINCIPAL
           END-PERFORM

           CLOSE ARQ-CLIENTES
           IF WS-STATUS NOT = "00"
                DISPLAY "ERRO AO FECHAR ARQUIVO: " WS-STATUS
           END-IF

           DISPLAY "PROGRAMA ENCERRADO."
           STOP RUN.

       ABRIR-ARQUIVO.
       OPEN I-O ARQ-CLIENTES

       EVALUATE WS-STATUS
            WHEN "00"
                DISPLAY "ARQUIVO ABERTO COM SUCESSO."
            WHEN "35"
                DISPLAY "CRIANDO ARQUIVO DE CLIENTES..."
                OPEN OUTPUT ARQ-CLIENTES
                IF WS-STATUS = "00"
                    CLOSE ARQ-CLIENTES
                    IF WS-STATUS = "00"
                        OPEN I-O ARQ-CLIENTES
                    END-IF
                END-IF
            WHEN OTHER
                DISPLAY "ERRO NA ABERTURA: " WS-STATUS
       END-EVALUATE.

       MENU-PRINCIPAL.
           DISPLAY SPACE
           DISPLAY "===== CADASTRO DE CLIENTES ====="
           DISPLAY "1 - INCLUIR CLIENTE"
           DISPLAY "2 - CONSULTAR CLIENTE"
           DISPLAY "3 - ALTERAR CLIENTE"
           DISPLAY "4 - EXCLUIR CLIENTE"
           DISPLAY "5 - LISTAR CLIENTES"
           DISPLAY "6 - ENCERRAR"
           DISPLAY "ESCOLHA UMA OPCAO: "
           ACCEPT WS-OPCAO

               EVALUATE WS-OPCAO
                WHEN 1 PERFORM INCLUIR-CLIENTE
                WHEN 2 PERFORM CONSULTAR-CLIENTE
                WHEN 3 PERFORM ALTERAR-CLIENTE
                WHEN 4 PERFORM EXCLUIR-CLIENTE
                WHEN 5 PERFORM LISTAR-CLIENTES
                WHEN 6 CONTINUE
                WHEN OTHER
                    DISPLAY "OPCAO INVALIDA."
           END-EVALUATE.

       INCLUIR-CLIENTE.
           DISPLAY SPACE
           DISPLAY "===== INCLUSAO ====="

               INITIALIZE REG-CLIENTE

               DISPLAY "CODIGO (5 DIGITOS): "
           ACCEPT CLI-CODIGO

               MOVE CLI-CODIGO TO WS-CODIGO
           READ ARQ-CLIENTES
            KEY IS CLI-CODIGO
           END-READ

           EVALUATE WS-STATUS
                WHEN "00"
                    DISPLAY "CODIGO JA CADASTRADO."
                WHEN "23"
                    INITIALIZE REG-CLIENTE
                    MOVE WS-CODIGO TO CLI-CODIGO

                            DISPLAY "CPF (11 DIGITOS): "
                    ACCEPT CLI-CPF

                            MOVE CLI-CPF TO WS-CPF
                    READ ARQ-CLIENTES
                        KEY IS CLI-CPF
                    END-READ

                            EVALUATE WS-STATUS
                        WHEN "00"
                            DISPLAY "CPF JA CADASTRADO."
                        WHEN "23"
                            INITIALIZE REG-CLIENTE
                            MOVE WS-CODIGO TO CLI-CODIGO
                            MOVE WS-CPF TO CLI-CPF

                                    DISPLAY "NOME: "
                            ACCEPT CLI-NOME
                            DISPLAY "TELEFONE: "
                            ACCEPT CLI-TELEFONE
                            DISPLAY "CIDADE: "
                            ACCEPT CLI-CIDADE
                            DISPLAY "ESTADO (UF): "
                            ACCEPT CLI-ESTADO

                                    WRITE REG-CLIENTE
                            EVALUATE WS-STATUS
                                WHEN "00"
                                    DISPLAY "CLIENTE CADASTRADO."
                                WHEN "22"
                                    DISPLAY "CODIGO OU CPF DUPLICADO."
                                WHEN OTHER
                                    DISPLAY "ERRO: " WS-STATUS
                            END-EVALUATE
                        WHEN OTHER
                            DISPLAY "ERRO AO VERIFICAR CPF: "
                                    WS-STATUS
                    END-EVALUATE
                WHEN OTHER
                    DISPLAY "ERRO AO VERIFICAR CODIGO: "
                            WS-STATUS
           END-EVALUATE.

       CONSULTAR-CLIENTE.
           DISPLAY SPACE
           DISPLAY "===== CONSULTA ====="
           DISPLAY "1 - CONSULTAR POR CODIGO"
           DISPLAY "2 - CONSULTAR POR CPF"
           DISPLAY "ESCOLHA: "
           ACCEPT WS-TIPO-CONSULTA

           EVALUATE WS-TIPO-CONSULTA
                WHEN 1
                    DISPLAY "INFORME O CODIGO: "
                    ACCEPT CLI-CODIGO
                    READ ARQ-CLIENTES
                        KEY IS CLI-CODIGO
                    END-READ
                WHEN 2
                    DISPLAY "INFORME O CPF: "
                    ACCEPT CLI-CPF
                    READ ARQ-CLIENTES
                        KEY IS CLI-CPF
                    END-READ
                WHEN OTHER
                    DISPLAY "OPCAO INVALIDA."
                    EXIT PARAGRAPH
           END-EVALUATE

           EVALUATE WS-STATUS
                WHEN "00"
                    PERFORM EXIBIR-CLIENTE
                WHEN "23"
                    DISPLAY "CLIENTE NAO ENCONTRADO."
                WHEN OTHER
                    DISPLAY "ERRO NA CONSULTA: " WS-STATUS
           END-EVALUATE.

       ALTERAR-CLIENTE.
           DISPLAY SPACE
           DISPLAY "===== ALTERACAO ====="
           DISPLAY "CODIGO DO CLIENTE: "
           ACCEPT CLI-CODIGO

           READ ARQ-CLIENTES
            KEY IS CLI-CODIGO
           END-READ

           EVALUATE WS-STATUS
                WHEN "00"
                    PERFORM EXIBIR-CLIENTE

                            DISPLAY "INFORME OS NOVOS DADOS."
                    DISPLAY "NOVO NOME: "
                    ACCEPT WS-NOME
                    DISPLAY "NOVO CPF: "
                    ACCEPT WS-CPF-NOVO
                    DISPLAY "NOVO TELEFONE: "
                    ACCEPT WS-TELEFONE
                    DISPLAY "NOVA CIDADE: "
                    ACCEPT WS-CIDADE
                    DISPLAY "NOVO ESTADO: "
                    ACCEPT WS-ESTADO

                            MOVE WS-NOME TO CLI-NOME
                    MOVE WS-CPF-NOVO TO CLI-CPF
                    MOVE WS-TELEFONE TO CLI-TELEFONE
                    MOVE WS-CIDADE TO CLI-CIDADE
                    MOVE WS-ESTADO TO CLI-ESTADO

                            REWRITE REG-CLIENTE

                            EVALUATE WS-STATUS
                        WHEN "00"
                            DISPLAY "CLIENTE ALTERADO."
                        WHEN "22"
                            DISPLAY "CPF JA CADASTRADO."
                        WHEN OTHER
                            DISPLAY "ERRO NA ALTERACAO: "
                                    WS-STATUS
                    END-EVALUATE
                WHEN "23"
                    DISPLAY "CLIENTE NAO ENCONTRADO."
                WHEN OTHER
                    DISPLAY "ERRO NA CONSULTA: " WS-STATUS
           END-EVALUATE.

       EXCLUIR-CLIENTE.
           DISPLAY SPACE
           DISPLAY "===== EXCLUSAO ====="
           DISPLAY "CODIGO DO CLIENTE: "
           ACCEPT CLI-CODIGO

               READ ARQ-CLIENTES
            KEY IS CLI-CODIGO
           END-READ

           EVALUATE WS-STATUS
                WHEN "00"
                    PERFORM EXIBIR-CLIENTE
                    DELETE ARQ-CLIENTES RECORD
                    END-DELETE

                            EVALUATE WS-STATUS
                        WHEN "00"
                            DISPLAY "CLIENTE EXCLUIDO."
                        WHEN "23"
                            DISPLAY "CLIENTE NAO ENCONTRADO."
                        WHEN OTHER
                            DISPLAY "ERRO NA EXCLUSAO: "
                                    WS-STATUS
                    END-EVALUATE
                WHEN "23"
                    DISPLAY "CLIENTE NAO ENCONTRADO."
                WHEN OTHER
                    DISPLAY "ERRO NA CONSULTA: " WS-STATUS
           END-EVALUATE.

       LISTAR-CLIENTES.
           DISPLAY SPACE
           DISPLAY "===== LISTAGEM DE CLIENTES ====="

           MOVE 0 TO WS-CONTADOR
           MOVE 0 TO WS-FIM
           MOVE ZERO TO CLI-CODIGO

           START ARQ-CLIENTES
            KEY IS GREATER THAN CLI-CODIGO
           END-START

           EVALUATE WS-STATUS
                WHEN "00"
                    PERFORM UNTIL WS-FIM = 1
                        READ ARQ-CLIENTES NEXT RECORD
                        END-READ

                                EVALUATE WS-STATUS
                            WHEN "00"
                                ADD 1 TO WS-CONTADOR
                                PERFORM EXIBIR-CLIENTE
                            WHEN "10"
                                MOVE 1 TO WS-FIM
                            WHEN OTHER
                                DISPLAY "ERRO NA LEITURA: "
                                        WS-STATUS
                                MOVE 1 TO WS-FIM
                        END-EVALUATE
                    END-PERFORM

                            DISPLAY "TOTAL DE CLIENTES: "
                            WS-CONTADOR
                WHEN "23"
                    DISPLAY "ARQUIVO VAZIO."
                WHEN OTHER
                    DISPLAY "ERRO NA LISTAGEM: " WS-STATUS
           END-EVALUATE.

       EXIBIR-CLIENTE.
           DISPLAY "--------------------------------"
           DISPLAY "CODIGO:   " CLI-CODIGO
           DISPLAY "NOME:     " FUNCTION TRIM(CLI-NOME)
           DISPLAY "CPF:      " CLI-CPF
           DISPLAY "TELEFONE: " FUNCTION TRIM(CLI-TELEFONE)
           DISPLAY "CIDADE:   " FUNCTION TRIM(CLI-CIDADE)
           DISPLAY "ESTADO:   " CLI-ESTADO
           DISPLAY "--------------------------------".

       END PROGRAM CBTDSIM-TP12.
