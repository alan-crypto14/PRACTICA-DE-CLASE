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

           SELECT EMPRESA-TEMP ASSIGN TO DISK
               "..\EMPRESA_TEMP.txt".

           SELECT EMPRESAS-ORD ASSIGN TO DISK
               "..\EMPRESA_ORD.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

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

       SD EMPRESA-TEMP.

       01 REG-EMPRESA-TEMP.
           05 TMP-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 TMP-NOMBRE-EMPRESA   PIC X(30).
           05 FILLER               PIC X(02).
           05 TMP-FCH-UP           PIC X(08).
           05 FILLER               PIC X(02).
           05 TMP-SALARIO          PIC X(09).

       FD EMPRESAS-ORD.

       01 REG-EMPRESA-ORD.
           05 ORD-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 ORD-NOMBRE-EMPRESA   PIC X(30).
           05 FILLER               PIC X(02).
           05 ORD-FCH-UP           PIC X(08).
           05 FILLER               PIC X(02).
           05 ORD-SALARIO          PIC X(09).


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
emman      05 LOG-FECHA            PIC X(08).
emman      05 FILLER               PIC X(01) VALUE "|".
emman      05 LOG-HORA             PIC X(06).
emman      05 FILLER               PIC X(01) VALUE "|".
emman      05 LOG-MENSAJE          PIC X(13).
emman      05 FILLER               PIC X(01) VALUE " ".
emman      05 LOG-NOM-EMPLEADO     PIC X(30).
emman      05 FILLER               PIC X(01) VALUE " ".
emman      05 LOG-MENSAJE2         PIC X(30).
emman      05 FILLER               PIC X(01) VALUE " ".
emman      05 LOG-NOM-EMPRESA      PIC X(30).


       WORKING-STORAGE SECTION.

       01 WS-FIN-EMPLEADOS         PIC X VALUE 'N'.
       01 WS-FIN-EMPRESAS          PIC X VALUE 'N'.

       01 WS-INDICE                PIC 9(02) VALUE 1.

       01 WS-CONTADOR              PIC 9(03) VALUE 0.


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
           DISPLAY "     RELACION MUCHOS A MUCHOS"
           DISPLAY "        EMPLEADOS - EMPRESAS"
           DISPLAY "=========================================="

           SORT EMP-TEMP ON ASCENDING KEY TMP-RFC
               INPUT PROCEDURE IS CARGAR-EMPLEADOS
               GIVING EMPLEADOS-ORD

           SORT EMPRESA-TEMP ON ASCENDING KEY TMP-RFC-EMPRESA
                INPUT PROCEDURE IS CARGAR-EMPRESAS
                GIVING EMPRESAS-ORD

           PERFORM CARGAR-TABLA-EMPLEADOS
           DISPLAY "EMPLEADOS RELACIONADOS CARGADOS."
           PERFORM CREAR-RELACION
           DISPLAY "RELACIONES CREADAS: " WS-CONTADOR

           SORT RELACION-TEMP  ON ASCENDING KEY TMP-REL-EMPRESA
                                                TMP-REL-RFC-EMPLEADO
               USING RELACION
               GIVING RELACION-ORD

           PERFORM CREAR-REPORTE

           DISPLAY "=========================================="
           DISPLAY " PROCESO TERMINADO CORRECTAMENTE"
           DISPLAY " REGISTROS GENERADOS: " WS-CONTADOR
           DISPLAY "=========================================="

           CLOSE ARCH-LOG
           STOP RUN.


       CARGAR-EMPLEADOS.
           OPEN INPUT EMPLEADOS
           MOVE 'N'   TO WS-FIN-EMPLEADOS

           *> SALTAR ENCABEZADO

           PERFORM UNTIL WS-FIN-EMPLEADOS = 'S'
               READ EMPLEADOS
                  AT END
                      MOVE 'S'           TO WS-FIN-EMPLEADOS
                  NOT AT END
                      MOVE RFC-EMPLEADO  TO TMP-RFC
                      MOVE NOMBRE        TO TMP-NOMBRE
                      MOVE APATERNO      TO TMP-APATERNO
                      MOVE AMATERNO      TO TMP-AMATERNO

                      RELEASE REG-EMP-TEMP
               END-READ
           END-PERFORM

           CLOSE EMPLEADOS.

       CARGAR-EMPRESAS.
           OPEN INPUT EMPRESAS
           MOVE 'N'   TO WS-FIN-EMPRESAS

           *> SALTAR ENCABEZADO

           PERFORM UNTIL WS-FIN-EMPRESAS = 'S'
               READ EMPRESAS
                  AT END
                       MOVE 'S'            TO WS-FIN-EMPRESAS
                  NOT AT END
                       MOVE RFC-EMPRESA    TO TMP-RFC-EMPRESA
                       MOVE NOMBRE-EMPRESA TO TMP-NOMBRE-EMPRESA
                       MOVE FCH-UP         TO TMP-FCH-UP
                       MOVE SALARIO        TO TMP-SALARIO

                       RELEASE REG-EMPRESA-TEMP
               END-READ
           END-PERFORM

           CLOSE EMPRESAS.


       CARGAR-TABLA-EMPLEADOS.
           OPEN INPUT EMPLEADOS-ORD
           MOVE 1  TO WS-INDICE

           PERFORM UNTIL WS-INDICE > 8
               READ EMPLEADOS-ORD
                  AT END
                       MOVE 9 TO WS-INDICE
                  NOT AT END
                     DISPLAY "EMPLEADO CARGADO: " ORD-RFC
                     MOVE ORD-RFC          TO TAB-RFC(WS-INDICE)
                     MOVE ORD-NOMBRE       TO TAB-NOMBRE(WS-INDICE)
                     MOVE ORD-APATERNO     TO TAB-APATERNO(WS-INDICE)
                     MOVE ORD-AMATERNO     TO TAB-AMATERNO(WS-INDICE)

                     ADD 1 TO WS-INDICE
               END-READ
           END-PERFORM

           CLOSE EMPLEADOS-ORD.


       CREAR-RELACION.

           OPEN INPUT EMPRESAS-ORD
           OUTPUT RELACION

           MOVE 'N' TO WS-FIN-EMPRESAS

           PERFORM UNTIL WS-FIN-EMPRESAS = 'S'
              READ EMPRESAS-ORD
                 AT END
                    MOVE 'S'           TO WS-FIN-EMPRESAS
                 NOT AT END
                    PERFORM RELACION-NTTDATA
                    PERFORM RELACION-DOMINION
                    PERFORM RELACION-MUBEA
                    PERFORM RELACION-NEORIS
              END-READ
           END-PERFORM

           CLOSE EMPRESAS-ORD
                 RELACION.


emman  RELACION-NTTDATA.

           IF ORD-NOMBRE-EMPRESA = "NTTDATA"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8

               EVALUATE TAB-RFC(WS-INDICE)
                   WHEN "GARC850315AB1"
                   WHEN "LOPR900721CD2"
                   WHEN "MAMJ880412EF3"
                   WHEN "CRLA930225OP8"
                        PERFORM ESCRIBIR-RELACION
                   WHEN OTHER
                       PERFORM ESCRIBIR-LOG
               END-EVALUATE
               ADD 1 TO WS-INDICE

               END-PERFORM

emman      END-IF.


emman  RELACION-DOMINION.

           IF ORD-NOMBRE-EMPRESA = "DOMINION"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8
                  IF TAB-RFC(WS-INDICE) = "GARC850315AB1" OR
                                          "ROSA920105GH4" OR
                                          "HEGA870923IJ5" OR
                                          "VEMA950617KL6"
                      PERFORM ESCRIBIR-RELACION
                  ELSE
                      PERFORM ESCRIBIR-LOG
                  END-IF
                  ADD 1 TO WS-INDICE
               END-PERFORM
emman      END-IF.


       RELACION-MUBEA.
           IF ORD-NOMBRE-EMPRESA = "MUBEA"
               MOVE 1 TO WS-INDICE
               PERFORM UNTIL WS-INDICE > 8
                  EVALUATE TRUE
                     WHEN TAB-RFC(WS-INDICE) = "LOPR900721CD2"
                     WHEN TAB-RFC(WS-INDICE) = "MAMJ880412EF3"
                     WHEN TAB-RFC(WS-INDICE) = "HEGA870923IJ5"
                     WHEN TAB-RFC(WS-INDICE) = "TORJ890830MN7"
                        PERFORM ESCRIBIR-RELACION
                     WHEN OTHER
                        PERFORM ESCRIBIR-LOG
                  END-EVALUATE
                  ADD 1 TO WS-INDICE
               END-PERFORM
           END-IF.


       RELACION-NEORIS.
           IF ORD-NOMBRE-EMPRESA = "NEORIS"
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
                         PERFORM ESCRIBIR-LOG
                 END-EVALUATE
                 ADD 1 TO WS-INDICE
              END-PERFORM
           END-IF.


       ESCRIBIR-RELACION.

           MOVE ORD-NOMBRE-EMPRESA       TO REL-NOMBRE-EMPRESA
           MOVE ORD-RFC-EMPRESA          TO REL-RFC-EMPRESA
           MOVE TAB-RFC(WS-INDICE)       TO REL-RFC-EMPLEADO
           MOVE TAB-NOMBRE(WS-INDICE)    TO REL-NOMBRE
           MOVE TAB-APATERNO(WS-INDICE)  TO REL-APATERNO
           MOVE TAB-AMATERNO(WS-INDICE)  TO REL-AMATERNO
           MOVE ORD-SALARIO              TO REL-SALARIO

           WRITE REG-RELACION

           ADD 1 TO WS-CONTADOR.

emman  ESCRIBIR-LOG.
           ACCEPT LOG-FECHA FROM DATE
           ACCEPT LOG-HORA  FROM TIME
           MOVE "EL EMPLEADO:"                   TO LOG-MENSAJE
           MOVE TAB-NOMBRE(WS-INDICE)            TO LOG-NOM-EMPLEADO
           MOVE "NO SE ENCONTRA EN LA EMPRESA:"  TO LOG-MENSAJE2
           MOVE ORD-NOMBRE-EMPRESA               TO LOG-NOM-EMPRESA

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
               END-READ
           END-PERFORM

           CLOSE RELACION-ORD
           REPORTE.
