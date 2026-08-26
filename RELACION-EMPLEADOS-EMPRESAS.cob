      ******************************************************************
      * Author:alan
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELACION-EMPLEADOS-EMPRESAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EMPLEADOS ASSIGN TO DISK
               "..\EMPLEADOS3.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMP-TEMP ASSIGN TO DISK
               "..\EMPLEADOS3_TEMP.txt".

           SELECT EMPLEADOS-ORD ASSIGN TO DISK
               "..\EMPLEADOS3_ORD.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS ASSIGN TO DISK
               "..\EMPRESA.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS-IDX ASSIGN TO DISK
               "..\EMPRESAS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS RFC-EMPRESA-IDX
               FILE STATUS  IS WS-FILE-STATUS.

           SELECT RELACION ASSIGN TO DISK
               "..\RELACION.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT RELACION-TEMP ASSIGN TO DISK
               "..\RELACION_TEMP.txt".

           SELECT RELACION-ORD ASSIGN TO DISK
               "..\RELACION_ORD.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT REPORTE ASSIGN TO DISK
               "..\REPORTE.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

emman      SELECT ARCH-LOG  ASSIGN TO DISK
emman          "..\log"
emman          ORGANIZATION IS LINE SEQUENTIAL
emman          ACCESS MODE  IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLEADOS.

       01 REG-EMPLEADO.
           05 RFC-EMPLEADO     PIC X(13).
           05 FILLER           PIC X(02).
           05 NOMBRE           PIC X(20).
           05 FILLER           PIC X(02).
           05 APATERNO         PIC X(20).
           05 FILLER           PIC X(02).
           05 AMATERNO         PIC X(20).

       SD EMP-TEMP.

       01 REG-EMP-TEMP.
           05 TMP-RFC          PIC X(13).
           05 FILLER           PIC X(02).
           05 TMP-NOMBRE       PIC X(20).
           05 FILLER           PIC X(02).
           05 TMP-APATERNO     PIC X(20).
           05 FILLER           PIC X(02).
           05 TMP-AMATERNO     PIC X(20).

       FD EMPLEADOS-ORD.

       01 REG-EMPLEADO-ORD.
           05 ORD-RFC          PIC X(13).
           05 FILLER           PIC X(02).
           05 ORD-NOMBRE       PIC X(20).
           05 FILLER           PIC X(02).
           05 ORD-APATERNO     PIC X(20).
           05 FILLER           PIC X(02).
           05 ORD-AMATERNO     PIC X(20).

       FD EMPRESAS.

       01 REG-EMPRESA.
           05 RFC-EMPRESA      PIC X(12).
           05 FILLER           PIC X(02).
           05 NOMBRE-EMPRESA   PIC X(30).
           05 FILLER           PIC X(02).
           05 FCH-UP           PIC X(08).
           05 FILLER           PIC X(02).
           05 SALARIO          PIC X(09).

       FD EMPRESAS-IDX.

       01 REG-EMPRESA-IDX.
           05 RFC-EMPRESA-IDX      PIC X(12).
           05 NOMBRE-EMPRESA-IDX   PIC X(30).
           05 FCH-UP-IDX           PIC X(08).
           05 SALARIO-IDX          PIC X(09).

       FD RELACION.

       01 REG-RELACION.
           05 REL-NOMBRE-EMPRESA   PIC X(30).
           05 FILLER               PIC X(02).
           05 REL-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 REL-RFC-EMPLEADO     PIC X(13).
           05 FILLER               PIC X(02).
           05 REL-NOMBRE           PIC X(20).
           05 FILLER               PIC X(02).
           05 REL-APATERNO         PIC X(20).
           05 FILLER               PIC X(02).
           05 REL-AMATERNO         PIC X(20).
           05 FILLER               PIC X(02).
           05 REL-SALARIO          PIC X(09).


       SD RELACION-TEMP.

       01 REG-RELACION-TEMP.
           05 TMP-REL-EMPRESA      PIC X(30).
           05 FILLER               PIC X(02).
           05 TMP-REL-RFC-EMPRESA  PIC X(12).
           05 FILLER               PIC X(02).
           05 TMP-REL-RFC-EMPLEADO PIC X(13).
           05 FILLER               PIC X(02).
           05 TMP-REL-NOMBRE       PIC X(20).
           05 FILLER               PIC X(02).
           05 TMP-REL-APATERNO     PIC X(20).
           05 FILLER               PIC X(02).
           05 TMP-REL-AMATERNO     PIC X(20).
           05 FILLER               PIC X(02).
           05 TMP-REL-SALARIO      PIC X(09).


       FD RELACION-ORD.

       01 REG-RELACION-ORD.
           05 ORD-REL-EMPRESA      PIC X(30).
           05 FILLER               PIC X(02).
           05 ORD-REL-RFC-EMPRESA  PIC X(12).
           05 FILLER               PIC X(02).
           05 ORD-REL-RFC-EMPLEADO PIC X(13).
           05 FILLER               PIC X(02).
           05 ORD-REL-NOMBRE       PIC X(20).
           05 FILLER               PIC X(02).
           05 ORD-REL-APATERNO     PIC X(20).
           05 FILLER               PIC X(02).
           05 ORD-REL-AMATERNO     PIC X(20).
           05 FILLER               PIC X(02).
           05 ORD-REL-SALARIO      PIC X(09).


       FD REPORTE.

       01 REG-REPORTE.
           05 REP-EMPRESA          PIC X(30).
           05 FILLER               PIC X(02).
           05 REP-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 REP-RFC-EMPLEADO     PIC X(13).
           05 FILLER               PIC X(02).
           05 REP-NOMBRE           PIC X(20).
           05 FILLER               PIC X(02).
           05 REP-APATERNO         PIC X(20).
           05 FILLER               PIC X(02).
           05 REP-AMATERNO         PIC X(20).
           05 FILLER               PIC X(02).
           05 REP-SALARIO          PIC X(09).

emman  FD ARCH-LOG.

emman  01 REG-ARCH-LOG.
emman      05 LOG-FECHA            PIC X(02).
emman      05 SEP-1                PIC X(01).
emman      05 LOG-HORA             PIC X(06).
emman      05 SEP-2                PIC X(01).
emman      05 LOG-MENSAJE          PIC X(100).




       WORKING-STORAGE SECTION.

       01 WS-FIN-EMPLEADOS         PIC X VALUE 'N'.
       01 WS-FIN-EMPRESAS          PIC X VALUE 'N'.

       01 WS-INDICE                PIC 9(02) VALUE 1.
       01 WS-TOTAL-EMPLEADOS       PIC 9(02) VALUE 0.
       01 WS-TOTAL-EMPRESAS        PIC 9(02) VALUE 0.
       01 WS-TOTAL-RELACIONES      PIC 9(02) VALUE 0.
       01 WS-CONTADOR              PIC 9(02) VALUE 0.

       01 WS-FILE-STATUS           PIC X(02).

       01 TABLA-EMPLEADOS.
           05 TAB-EMPLEADO OCCURS 8 TIMES.
               10 TAB-RFC          PIC X(13).
               10 TAB-NOMBRE       PIC X(20).
               10 TAB-APATERNO     PIC X(20).
               10 TAB-AMATERNO     PIC X(20).


       PROCEDURE DIVISION.

       INICIO.
           OPEN OUTPUT ARCH-LOG
           DISPLAY "=========================================="
           DISPLAY "        ESTADISTICAS DEL PROGRAMA"
           DISPLAY "=========================================="

           SORT EMP-TEMP ON ASCENDING KEY TMP-RFC
               INPUT PROCEDURE IS CARGAR-EMPLEADOS
               GIVING EMPLEADOS-ORD

           PERFORM CARGAR-TABLA-EMPLEADOS
           DISPLAY "EMPLEADOS CARGADOS:       " WS-TOTAL-EMPLEADOS
           PERFORM CARGAR-EMPRESAS
           DISPLAY "EMPRESAS CARGADAS:        " WS-TOTAL-EMPRESAS
           PERFORM CREAR-RELACION
           DISPLAY "RELACIONES CREADAS:       " WS-TOTAL-RELACIONES

           SORT RELACION-TEMP  ON ASCENDING KEY TMP-REL-EMPRESA
                                                TMP-REL-RFC-EMPLEADO
               USING RELACION
               GIVING RELACION-ORD

           PERFORM CREAR-REPORTE
           DISPLAY "EMPRESAS EN EL REPORTE:   " WS-TOTAL-EMPRESAS
           DISPLAY "REGISTROS EN EL REPORTE:  " WS-CONTADOR

           DISPLAY "=========================================="
           DISPLAY " PROCESO TERMINADO CORRECTAMENTE"
           DISPLAY "=========================================="

           CLOSE ARCH-LOG
           STOP RUN.


       CARGAR-EMPLEADOS.
           OPEN INPUT EMPLEADOS
           MOVE 'N'   TO WS-FIN-EMPLEADOS

           PERFORM UNTIL WS-FIN-EMPLEADOS = 'S'
               READ EMPLEADOS
                  AT END
                      MOVE 'S'           TO WS-FIN-EMPLEADOS
                  NOT AT END
                      IF NOMBRE NOT = "NOMBRE"
                         MOVE RFC-EMPLEADO  TO TMP-RFC
                         MOVE NOMBRE        TO TMP-NOMBRE
                         MOVE APATERNO      TO TMP-APATERNO
                         MOVE AMATERNO      TO TMP-AMATERNO

                         RELEASE REG-EMP-TEMP
                         ADD 1              TO WS-TOTAL-EMPLEADOS
                      END-IF
               END-READ
           END-PERFORM

           CLOSE EMPLEADOS.

       CARGAR-EMPRESAS.
           OPEN INPUT  EMPRESAS
           OPEN OUTPUT EMPRESAS-IDX
           IF WS-FILE-STATUS NOT = "00"
              DISPLAY "ERROR AL CREAR ARCHIVO INDEXADO"
           ELSE
              MOVE 'N'   TO WS-FIN-EMPRESAS
              PERFORM UNTIL WS-FIN-EMPRESAS = 'S'
                 READ EMPRESAS
                    AT END
                       MOVE 'S'            TO WS-FIN-EMPRESAS
                    NOT AT END
                       IF NOMBRE-EMPRESA NOT = "NOMBRE-EMPRESA"
                          MOVE RFC-EMPRESA    TO RFC-EMPRESA-IDX
                          MOVE NOMBRE-EMPRESA TO NOMBRE-EMPRESA-IDX
                          MOVE FCH-UP         TO FCH-UP-IDX                                                 FCH-UP-IDX
                          MOVE SALARIO        TO SALARIO-IDX                                                 SALARIO-IDX

                          WRITE REG-EMPRESA-IDX
                             INVALID
                                DISPLAY "ERROR AL GRABAR EN INDEXADO"
                             NOT INVALID
                                CONTINUE
                          END-WRITE
                          ADD 1               TO WS-TOTAL-EMPRESAS
                       END-IF
                 END-READ
              END-PERFORM
           END-IF

           CLOSE EMPRESAS
                 EMPRESAS-IDX.


       CARGAR-TABLA-EMPLEADOS.
           OPEN INPUT EMPLEADOS-ORD
           MOVE 1  TO WS-INDICE

           PERFORM UNTIL WS-INDICE > 8
               READ EMPLEADOS-ORD
                  AT END
                       MOVE 9 TO WS-INDICE
                  NOT AT END
                     MOVE ORD-RFC          TO TAB-RFC(WS-INDICE)
                     MOVE ORD-NOMBRE       TO TAB-NOMBRE(WS-INDICE)
                     MOVE ORD-APATERNO     TO TAB-APATERNO(WS-INDICE)
                     MOVE ORD-AMATERNO     TO TAB-AMATERNO(WS-INDICE)

                     ADD 1 TO WS-INDICE
               END-READ
           END-PERFORM

           CLOSE EMPLEADOS-ORD.


       CREAR-RELACION.
           OPEN INPUT  EMPRESAS-IDX
                OUTPUT RELACION

           MOVE "NTD120101AB1"   TO RFC-EMPRESA-IDX
           READ EMPRESAS-IDX KEY IS RFC-EMPRESA-IDX
              INVALID KEY
                 DISPLAY "NTTDATA NO ENCONTRADA"
                 MOVE "NTTDATA NO ENCONTRADA EN EMPRESAS" TO LOG-MENSAJE
                 PERFORM ESCRIBIR-LOG
               NOT INVALID KEY
                   PERFORM RELACION-NTTDATA
           END-READ

           MOVE "DOM130215CD2"   TO RFC-EMPRESA-IDX
           READ EMPRESAS-IDX KEY IS RFC-EMPRESA-IDX
              INVALID KEY
                 DISPLAY "DOMINION NO ENCONTRADA"
                 MOVE "DOMINION NO ENCONTRADA EN EMPRESAS"
                   TO LOG-MENSAJE
                 PERFORM ESCRIBIR-LOG
              NOT INVALID KEY
                 PERFORM RELACION-DOMINION
           END-READ

           MOVE "MUB140320EF3"   TO RFC-EMPRESA-IDX
           READ EMPRESAS-IDX KEY IS RFC-EMPRESA-IDX
              INVALID KEY
                 DISPLAY "MUBEA NO ENCONTRADA"
                 MOVE "MUBEA NO ENCONTRADA EN EMPRESAS" TO LOG-MENSAJE
                 PERFORM ESCRIBIR-LOG
              NOT INVALID KEY
                 PERFORM RELACION-MUBEA
           END-READ

           MOVE "NEO150410GH4"   TO RFC-EMPRESA-IDX
           READ EMPRESAS-IDX KEY IS RFC-EMPRESA-IDX
              INVALID KEY
                 DISPLAY "NEORIS NO ENCONTRADA"
                 MOVE "NEORIS NO ENCONTRADA EN EMPRESAS" TO LOG-MENSAJE
                 PERFORM ESCRIBIR-LOG
              NOT INVALID KEY
                 PERFORM RELACION-NEORIS
           END-READ

           CLOSE EMPRESAS-IDX
                 RELACION.


emman  RELACION-NTTDATA.

           IF NOMBRE-EMPRESA-IDX = "NTTDATA"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8

               EVALUATE TAB-RFC(WS-INDICE)
                   WHEN "GARC850315AB1"
                   WHEN "LOPR900721CD2"
                   WHEN "MAMJ880412EF3"
                   WHEN "CRLA930225OP8"
                        PERFORM ESCRIBIR-RELACION
                   WHEN OTHER
                        STRING TAB-RFC(WS-INDICE)
                               " NO ENCONTRADO EN EMPRESA NTTDATA"
                               DELIMITED BY SIZE INTO LOG-MENSAJE
                        END-STRING
                        PERFORM ESCRIBIR-LOG
               END-EVALUATE
               ADD 1 TO WS-INDICE

               END-PERFORM

emman      END-IF.


emman  RELACION-DOMINION.

           IF NOMBRE-EMPRESA-IDX = "DOMINION"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8
                  IF TAB-RFC(WS-INDICE) = "GARC850315AB1" OR
                                          "ROSA920105GH4" OR
                                          "HEGA870923IJ5" OR
                                          "VEMA950617KL6"
                      PERFORM ESCRIBIR-RELACION
                  ELSE
                      STRING TAB-RFC(WS-INDICE)
                             " NO ENCONTRADO EN EMPRESA DOMINION"
                             DELIMITED BY SIZE INTO LOG-MENSAJE
                      END-STRING
                      PERFORM ESCRIBIR-LOG
                  END-IF
                  ADD 1 TO WS-INDICE
               END-PERFORM
emman      END-IF.


       RELACION-MUBEA.
           IF NOMBRE-EMPRESA-IDX = "MUBEA"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8
                  EVALUATE TRUE
                     WHEN TAB-RFC(WS-INDICE) = "LOPR900721CD2"
                     WHEN TAB-RFC(WS-INDICE) = "MAMJ880412EF3"
                     WHEN TAB-RFC(WS-INDICE) = "HEGA870923IJ5"
                     WHEN TAB-RFC(WS-INDICE) = "TORJ890830MN7"
                          PERFORM ESCRIBIR-RELACION
                     WHEN OTHER
                          STRING TAB-RFC(WS-INDICE)
                                 " NO ENCONTRADO EN EMPRESA MUBEA"
                                 DELIMITED BY SIZE INTO LOG-MENSAJE
                          END-STRING
                          PERFORM ESCRIBIR-LOG
                  END-EVALUATE
                  ADD 1 TO WS-INDICE
               END-PERFORM
           END-IF.


       RELACION-NEORIS.
           IF NOMBRE-EMPRESA-IDX = "NEORIS"
              MOVE 1 TO WS-INDICE
              PERFORM UNTIL WS-INDICE > 8
                 EVALUATE TAB-RFC(WS-INDICE)
                    WHEN "MAMJ880412EF3"
                    WHEN "ROSA920105GH4"
                    WHEN "VEMA950617KL6"
                    WHEN "TORJ890830MN7"
                    WHEN "CRLA930225OP8"
                         PERFORM ESCRIBIR-RELACION
                    WHEN OTHER
                         STRING TAB-RFC(WS-INDICE)
                                " NO ENCONTRADO EN EMPRESA NEORIS"
                                DELIMITED BY SIZE INTO LOG-MENSAJE
                         END-STRING
                         PERFORM ESCRIBIR-LOG
                 END-EVALUATE
                 ADD 1 TO WS-INDICE
              END-PERFORM
           END-IF.


       ESCRIBIR-RELACION.

           MOVE NOMBRE-EMPRESA-IDX       TO REL-NOMBRE-EMPRESA
           MOVE RFC-EMPRESA-IDX          TO REL-RFC-EMPRESA
           MOVE TAB-RFC(WS-INDICE)       TO REL-RFC-EMPLEADO
           MOVE TAB-NOMBRE(WS-INDICE)    TO REL-NOMBRE
           MOVE TAB-APATERNO(WS-INDICE)  TO REL-APATERNO
           MOVE TAB-AMATERNO(WS-INDICE)  TO REL-AMATERNO
           MOVE SALARIO-IDX              TO REL-SALARIO

           WRITE REG-RELACION

           ADD 1 TO WS-TOTAL-RELACIONES.

emman  ESCRIBIR-LOG.
           INITIALIZE REG-ARCH-LOG
           ACCEPT LOG-FECHA FROM DATE
           ACCEPT LOG-HORA  FROM TIME
           MOVE "|"           TO SEP-1 SEP-2

           WRITE REG-ARCH-LOG.


       CREAR-REPORTE.

           OPEN INPUT RELACION-ORD
           OUTPUT REPORTE

           MOVE SPACES TO REG-REPORTE

           MOVE "EMPRESA"       TO REP-EMPRESA
           MOVE "RFC-EMPRESA"   TO REP-RFC-EMPRESA
           MOVE "RFC-EMPLEADO"  TO REP-RFC-EMPLEADO
           MOVE "NOMBRE"        TO REP-NOMBRE
           MOVE "APATERNO"      TO REP-APATERNO
           MOVE "AMATERNO"      TO REP-AMATERNO
           MOVE "SALARIO"       TO REP-SALARIO

           WRITE REG-REPORTE

           MOVE 'N' TO WS-FIN-EMPRESAS

           PERFORM UNTIL WS-FIN-EMPRESAS = 'S'
              READ RELACION-ORD
                 AT END
                    MOVE 'S' TO WS-FIN-EMPRESAS
                 NOT AT END
                    MOVE ORD-REL-EMPRESA      TO REP-EMPRESA
                    MOVE ORD-REL-RFC-EMPRESA  TO REP-RFC-EMPRESA
                    MOVE ORD-REL-RFC-EMPLEADO TO REP-RFC-EMPLEADO
                    MOVE ORD-REL-NOMBRE       TO REP-NOMBRE
                    MOVE ORD-REL-APATERNO     TO REP-APATERNO
                    MOVE ORD-REL-AMATERNO     TO REP-AMATERNO
                    MOVE ORD-REL-SALARIO      TO REP-SALARIO

                    WRITE REG-REPORTE
                    ADD 1                     TO WS-CONTADOR
               END-READ
           END-PERFORM

           CLOSE RELACION-ORD
           REPORTE.
