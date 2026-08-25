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

         *>Cambie solo seccion de empresas, en vez de leer de registro en registro ahora lo lee de forma dinamica por una llave que en este caso sera el rfc
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

       FD EMPLEADOS
           RECORD CONTAINS 79 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMPLEADO.

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

       FD EMPLEADOS-ORD
           RECORD CONTAINS 79 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMPLEADO-ORD.

       01 REG-EMPLEADO-ORD.
           05 ORD-RFC          PIC X(13).
           05 FILLER           PIC X(02).
           05 ORD-NOMBRE       PIC X(20).
           05 FILLER           PIC X(02).
           05 ORD-APATERNO     PIC X(20).
           05 FILLER           PIC X(02).
           05 ORD-AMATERNO     PIC X(20).

       FD EMPRESAS
           RECORD CONTAINS 65 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMPRESA.

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

       FD EMPRESAS-ORD
           RECORD CONTAINS 65 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-EMPRESA-ORD.

       01 REG-EMPRESA-ORD.
           05 ORD-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X(02).
           05 ORD-NOMBRE-EMPRESA   PIC X(30).
           05 FILLER               PIC X(02).
           05 ORD-FCH-UP           PIC X(08).
           05 FILLER               PIC X(02).
           05 ORD-SALARIO          PIC X(09).


              FD RELACION
           RECORD CONTAINS 136 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-RELACION.

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


              FD RELACION-ORD
           RECORD CONTAINS 136 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-RELACION-ORD.

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


              FD REPORTE
           RECORD CONTAINS 136 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REG-REPORTE.

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


       WORKING-STORAGE SECTION.

       01 WS-FIN-EMPLEADOS         PIC X VALUE 'N'.
       01 WS-FIN-EMPRESAS          PIC X VALUE 'N'.
       01 WS-INDICE                PIC 9(02) VALUE 1.
       01 WS-CONTADOR              PIC 9(03) VALUE 0.
       *>Agregue un filestatus para que muestre que si las operaciones del indexado funcionan
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
           PERFORM CARGAR-TABLA-EMPLEADOS
           DISPLAY "EMPLEADOS RELACIONADOS CARGADOS."
           PERFORM CREAR-RELACION

           SORT EMPRESA-TEMP
           ON ASCENDING KEY TMP-RFC-EMPRESA
           INPUT PROCEDURE IS CARGAR-EMPRESAS
           GIVING EMPRESAS-ORD

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
           MOVE 'S'
           TO WS-FIN-EMPLEADOS

           NOT AT END

           MOVE RFC-EMPLEADO
           TO TMP-RFC

           MOVE NOMBRE
           TO TMP-NOMBRE

           MOVE APATERNO
           TO TMP-APATERNO

           MOVE AMATERNO
           TO TMP-AMATERNO

           RELEASE REG-EMP-TEMP

           END-READ

           END-PERFORM

           CLOSE EMPLEADOS.


       CARGAR-EMPRESAS.

           OPEN INPUT EMPRESAS

           MOVE 'N' TO WS-FIN-EMPRESAS

           READ EMPRESAS
               AT END
                   MOVE 'S' TO WS-FIN-EMPRESAS
           END-READ

           PERFORM UNTIL WS-FIN-EMPRESAS = 'S'

           READ EMPRESAS

           AT END
           MOVE 'S'
           TO WS-FIN-EMPRESAS

           NOT AT END

           MOVE RFC-EMPRESA
           TO TMP-RFC-EMPRESA

           MOVE NOMBRE-EMPRESA
           TO TMP-NOMBRE-EMPRESA

           MOVE FCH-UP
           TO TMP-FCH-UP

           MOVE SALARIO
           TO TMP-SALARIO

           RELEASE REG-EMPRESA-TEMP

           END-READ

           END-PERFORM

           CLOSE EMPRESAS.


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
       *>Crear relacion se modifica para poder buscar por rfc cada empresa, igualmente, se leia de forma secuencial
           OPEN I-O EMPRESAS

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

           IF ORD-NOMBRE-EMPRESA = "NTTDATA"

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

           IF ORD-NOMBRE-EMPRESA = "DOMINION"

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

           IF ORD-NOMBRE-EMPRESA = "MUBEA"

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

           IF ORD-NOMBRE-EMPRESA = "NEORIS"

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

           MOVE ORD-NOMBRE-EMPRESA
           TO REL-NOMBRE-EMPRESA

           MOVE ORD-RFC-EMPRESA
           TO REL-RFC-EMPRESA

           MOVE TAB-RFC(WS-INDICE)
           TO REL-RFC-EMPLEADO

           MOVE TAB-NOMBRE(WS-INDICE)
           TO REL-NOMBRE

           MOVE TAB-APATERNO(WS-INDICE)
           TO REL-APATERNO

           MOVE TAB-AMATERNO(WS-INDICE)
           TO REL-AMATERNO

           MOVE ORD-SALARIO
           TO REL-SALARIO

           WRITE REG-RELACION

           ADD 1 TO WS-CONTADOR.


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

           MOVE ORD-REL-EMPRESA
           TO REP-EMPRESA

           MOVE ORD-REL-RFC-EMPRESA
           TO REP-RFC-EMPRESA

           MOVE ORD-REL-RFC-EMPLEADO
           TO REP-RFC-EMPLEADO

           MOVE ORD-REL-NOMBRE
           TO REP-NOMBRE

           MOVE ORD-REL-APATERNO
           TO REP-APATERNO

           MOVE ORD-REL-AMATERNO
           TO REP-AMATERNO

           MOVE ORD-REL-SALARIO
           TO REP-SALARIO

           WRITE REG-REPORTE

           END-READ

           END-PERFORM

           CLOSE RELACION-ORD
           REPORTE.
