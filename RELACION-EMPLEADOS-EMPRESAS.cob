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

           SELECT EMPRESA-TXT ASSIGN TO DISK
               "..\EMPRESA.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS ASSIGN TO DISK
               "..\EMPRESA.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RFC-EMPRESA
               FILE STATUS IS WS-FILE-STATUS.

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


       FD EMPRESA-TXT.

       01 REG-EMPRESA-TXT.
           05 TXT-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 TXT-NOMBRE-EMPRESA   PIC X(30).
           05 FILLER               PIC X(02).
           05 TXT-FCH-UP           PIC X(08).
           05 FILLER               PIC X(02).
           05 TXT-SALARIO          PIC X(09).


       FD EMPRESAS.

       01 REG-EMPRESA.
           05 RFC-EMPRESA      PIC X(12).
           05 FILLER           PIC X(02).
           05 NOMBRE-EMPRESA   PIC X(30).
           05 FILLER           PIC X(02).
           05 FCH-UP           PIC X(08).
           05 FILLER           PIC X(02).
           05 SALARIO          PIC X(09).


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

            *>cambios realizados por Suazo<*
       FD REPORTE.

       01 REG-REPORTE.
           05 REP-EMPRESA          PIC X(30).
           05 FILLER               PIC X(02).
           05 REP-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 REP-EMPLEADO         PIC X(70).

       WORKING-STORAGE SECTION.
       01 WS-BANDERAS.
           05 WS-FIN-EMPLEADOS          PIC X VALUE 'N'.
           05 WS-FIN-EMPRESA-TXT        PIC X VALUE 'N'.
           05 WS-FIN-REPORTE            PIC X VALUE 'N'.
           05 WS-FIN-LECTURA            PIC X VALUE 'N'.
           05 WS-PRIMER-REGISTRO        PIC X VALUE 'S'.
           05 WS-PRIMER-EMPLEADO        PIC X VALUE 'S'.

       01 WS-FORMATO.
           05 WS-EMPRESA-ACTUAL        PIC X(30).
           05 WS-RFC-EMPRESA-ACTUAL    PIC X(12).

       01 WS-EMPLEADO-TEMP.
           05 WS-RFC-EMPLEADO       PIC X(13).
           05 WS-SEPARADOR          PIC X(03).
           05 WS-NOMBRE-EMPLEADO    PIC X(20).
           05 WS-APATERNO-EMPLEADO  PIC X(20).

       01 WS-INDICE                PIC 9(02) VALUE 1.
       01 WS-CONTADOR              PIC 9(03) VALUE 0.
       01 WS-FILE-STATUS           PIC XX.


       01 TABLA-EMPLEADOS.
           05 TAB-EMPLEADO OCCURS 8 TIMES.
               10 TAB-RFC          PIC X(13).
               10 TAB-NOMBRE       PIC X(20).
               10 TAB-APATERNO     PIC X(20).
               10 TAB-AMATERNO     PIC X(20).


       PROCEDURE DIVISION.

       INICIO.

           DISPLAY "=========================================="
           DISPLAY "     RELACION MUCHOS A MUCHOS"
           DISPLAY "        EMPLEADOS - EMPRESAS"
           DISPLAY "=========================================="

           SORT EMP-TEMP
               ON ASCENDING KEY TMP-RFC
               INPUT PROCEDURE IS CARGAR-EMPLEADOS
               GIVING EMPLEADOS-ORD

           PERFORM CREAR-ARCHIVO-EMPRESAS

           PERFORM CARGAR-TABLA-EMPLEADOS

           DISPLAY "EMPLEADOS RELACIONADOS CARGADOS."

           PERFORM CREAR-RELACION

           DISPLAY "RELACIONES CREADAS: " WS-CONTADOR

           SORT RELACION-TEMP
               ON ASCENDING KEY
                   TMP-REL-EMPRESA
                   TMP-REL-RFC-EMPLEADO
               USING RELACION
               GIVING RELACION-ORD

           PERFORM CREAR-REPORTE

           DISPLAY "=========================================="
           DISPLAY " PROCESO TERMINADO CORRECTAMENTE"
           DISPLAY " REGISTROS GENERADOS: " WS-CONTADOR
           DISPLAY "=========================================="

           STOP RUN.


       CARGAR-EMPLEADOS.

           OPEN INPUT EMPLEADOS

           MOVE 'N' TO WS-FIN-EMPLEADOS

           READ EMPLEADOS
               AT END
                   MOVE 'S' TO WS-FIN-EMPLEADOS
           END-READ

           PERFORM UNTIL WS-FIN-EMPLEADOS = 'S'

               READ EMPLEADOS
                   AT END
                       MOVE 'S' TO WS-FIN-EMPLEADOS

                   NOT AT END

                       MOVE RFC-EMPLEADO TO TMP-RFC
                       MOVE NOMBRE TO TMP-NOMBRE
                       MOVE APATERNO TO TMP-APATERNO
                       MOVE AMATERNO TO TMP-AMATERNO

                       RELEASE REG-EMP-TEMP

               END-READ

           END-PERFORM

           CLOSE EMPLEADOS.


       CREAR-ARCHIVO-EMPRESAS.

           MOVE 'N' TO WS-FIN-EMPRESA-TXT

           OPEN INPUT EMPRESA-TXT
                OUTPUT EMPRESAS

           IF WS-FILE-STATUS NOT = "00"

               DISPLAY "ERROR AL CREAR EMPRESA.DAT: "
                       WS-FILE-STATUS

           ELSE

               READ EMPRESA-TXT
                   AT END
                       MOVE 'S' TO WS-FIN-EMPRESA-TXT
               END-READ

               PERFORM UNTIL WS-FIN-EMPRESA-TXT = 'S'

                   READ EMPRESA-TXT

                       AT END
                           MOVE 'S' TO WS-FIN-EMPRESA-TXT

                       NOT AT END

                           MOVE TXT-RFC-EMPRESA
                               TO RFC-EMPRESA

                           MOVE TXT-NOMBRE-EMPRESA
                               TO NOMBRE-EMPRESA

                           MOVE TXT-FCH-UP
                               TO FCH-UP

                           MOVE TXT-SALARIO
                               TO SALARIO

                           WRITE REG-EMPRESA

                   END-READ

               END-PERFORM

           END-IF

           CLOSE EMPRESA-TXT
                 EMPRESAS.


       CARGAR-TABLA-EMPLEADOS.

           OPEN INPUT EMPLEADOS-ORD

           MOVE 1 TO WS-INDICE

           PERFORM UNTIL WS-INDICE > 8

               READ EMPLEADOS-ORD

                   AT END
                       MOVE 9 TO WS-INDICE

                   NOT AT END

                       DISPLAY "EMPLEADO CARGADO: " ORD-RFC

                       MOVE ORD-RFC
                           TO TAB-RFC(WS-INDICE)

                       MOVE ORD-NOMBRE
                           TO TAB-NOMBRE(WS-INDICE)

                       MOVE ORD-APATERNO
                           TO TAB-APATERNO(WS-INDICE)

                       MOVE ORD-AMATERNO
                           TO TAB-AMATERNO(WS-INDICE)

                       ADD 1 TO WS-INDICE

               END-READ

           END-PERFORM

           CLOSE EMPLEADOS-ORD.


       CREAR-RELACION.

           OPEN INPUT EMPRESAS
                OUTPUT RELACION

           MOVE "NTD120101AB1" TO RFC-EMPRESA

           READ EMPRESAS
               KEY IS RFC-EMPRESA

               INVALID KEY
                   DISPLAY "NTTDATA NO ENCONTRADA"

               NOT INVALID KEY
                   PERFORM RELACION-NTTDATA

           END-READ


           MOVE "DOM130215CD2" TO RFC-EMPRESA

           READ EMPRESAS
               KEY IS RFC-EMPRESA

               INVALID KEY
                   DISPLAY "DOMINION NO ENCONTRADA"

               NOT INVALID KEY
                   PERFORM RELACION-DOMINION

           END-READ


           MOVE "MUB140320EF3" TO RFC-EMPRESA

           READ EMPRESAS
               KEY IS RFC-EMPRESA

               INVALID KEY
                   DISPLAY "MUBEA NO ENCONTRADA"

               NOT INVALID KEY
                   PERFORM RELACION-MUBEA

           END-READ


           MOVE "NEO150410GH4" TO RFC-EMPRESA

           READ EMPRESAS
               KEY IS RFC-EMPRESA

               INVALID KEY
                   DISPLAY "NEORIS NO ENCONTRADA"

               NOT INVALID KEY
                   PERFORM RELACION-NEORIS

           END-READ

           CLOSE EMPRESAS
                 RELACION.


       RELACION-NTTDATA.

           IF NOMBRE-EMPRESA = "NTTDATA"

               MOVE 1 TO WS-INDICE

               PERFORM UNTIL WS-INDICE > 8

                   IF TAB-RFC(WS-INDICE) = "GARC850315AB1"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "LOPR900721CD2"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "MAMJ880412EF3"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "CRLA930225OP8"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   ADD 1 TO WS-INDICE

               END-PERFORM

           END-IF.


       RELACION-DOMINION.

           IF NOMBRE-EMPRESA = "DOMINION"

               MOVE 1 TO WS-INDICE

               PERFORM UNTIL WS-INDICE > 8

                   IF TAB-RFC(WS-INDICE) = "GARC850315AB1"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "ROSA920105GH4"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "HEGA870923IJ5"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "VEMA950617KL6"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   ADD 1 TO WS-INDICE

               END-PERFORM

           END-IF.


       RELACION-MUBEA.

           IF NOMBRE-EMPRESA = "MUBEA"

               MOVE 1 TO WS-INDICE

               PERFORM UNTIL WS-INDICE > 8

                   IF TAB-RFC(WS-INDICE) = "LOPR900721CD2"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "MAMJ880412EF3"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "HEGA870923IJ5"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "TORJ890830MN7"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   ADD 1 TO WS-INDICE

               END-PERFORM

           END-IF.


       RELACION-NEORIS.

           IF NOMBRE-EMPRESA = "NEORIS"

               MOVE 1 TO WS-INDICE

               PERFORM UNTIL WS-INDICE > 8

                   IF TAB-RFC(WS-INDICE) = "MAMJ880412EF3"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "ROSA920105GH4"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "VEMA950617KL6"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "TORJ890830MN7"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   IF TAB-RFC(WS-INDICE) = "CRLA930225OP8"
                       PERFORM ESCRIBIR-RELACION
                   END-IF

                   ADD 1 TO WS-INDICE

               END-PERFORM

           END-IF.


       ESCRIBIR-RELACION.

           MOVE NOMBRE-EMPRESA
               TO REL-NOMBRE-EMPRESA

           MOVE RFC-EMPRESA
               TO REL-RFC-EMPRESA

           MOVE TAB-RFC(WS-INDICE)
               TO REL-RFC-EMPLEADO

           MOVE TAB-NOMBRE(WS-INDICE)
               TO REL-NOMBRE

           MOVE TAB-APATERNO(WS-INDICE)
               TO REL-APATERNO

           MOVE TAB-AMATERNO(WS-INDICE)
               TO REL-AMATERNO

           MOVE SALARIO
               TO REL-SALARIO

           WRITE REG-RELACION

           ADD 1 TO WS-CONTADOR.

           *>MODIFICACIONES PARA EL REPORTE>*
       CREAR-REPORTE.

           OPEN INPUT RELACION-ORD
                OUTPUT REPORTE

           MOVE 'N' TO WS-FIN-LECTURA
           MOVE 'S' TO WS-PRIMER-REGISTRO
           MOVE 'S' TO WS-PRIMER-EMPLEADO

           PERFORM UNTIL WS-FIN-LECTURA = 'S'

               READ RELACION-ORD

                   AT END

                       MOVE 'S'
                           TO WS-FIN-LECTURA

                   NOT AT END

                       IF WS-PRIMER-REGISTRO = 'S'

                           MOVE ORD-REL-EMPRESA
                               TO WS-EMPRESA-ACTUAL

                           MOVE ORD-REL-RFC-EMPRESA
                               TO WS-RFC-EMPRESA-ACTUAL

                           MOVE 'N'
                               TO WS-PRIMER-REGISTRO

                           MOVE 'S'
                               TO WS-PRIMER-EMPLEADO

                           PERFORM ESCRIBIR-EMPLEADO

                       ELSE

                           IF ORD-REL-EMPRESA =
                              WS-EMPRESA-ACTUAL

                               MOVE 'N'
                                   TO WS-PRIMER-EMPLEADO

                               PERFORM ESCRIBIR-EMPLEADO

                           ELSE

                               MOVE ORD-REL-EMPRESA
                                   TO WS-EMPRESA-ACTUAL

                               MOVE ORD-REL-RFC-EMPRESA
                                   TO WS-RFC-EMPRESA-ACTUAL

                               MOVE 'S'
                                   TO WS-PRIMER-EMPLEADO

                               PERFORM ESCRIBIR-EMPLEADO

                           END-IF

                       END-IF

               END-READ

           END-PERFORM

           CLOSE RELACION-ORD
                 REPORTE.

           ESCRIBIR-EMPLEADO.

           MOVE SPACES TO REG-REPORTE

           IF WS-PRIMER-EMPLEADO = 'S'

               MOVE WS-EMPRESA-ACTUAL
                   TO REP-EMPRESA

               MOVE WS-RFC-EMPRESA-ACTUAL
                   TO REP-RFC-EMPRESA

           END-IF

           STRING
               ORD-REL-RFC-EMPLEADO
               " - "
               ORD-REL-NOMBRE
               " "
               ORD-REL-APATERNO
               " "
               ORD-REL-AMATERNO
               DELIMITED BY SIZE
               INTO REP-EMPLEADO
           END-STRING

           WRITE REG-REPORTE.
