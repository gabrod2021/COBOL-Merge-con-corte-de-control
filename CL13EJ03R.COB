      *----------------------------------------------------------------*
      *      Merge - Corte control
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CL13EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT SUCURSA
           ASSIGN TO '../SUCURSAL-A.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSA.

       SELECT SUCURSB
           ASSIGN TO '../SUCURSAL-B.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSB.

       SELECT SUCURSC
           ASSIGN TO '../SUCURSAL-C.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSC.


       SELECT VENTATOT
           ASSIGN TO '../VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTATOT.

       SELECT TOTSALID
           ASSIGN TO '../TOTSALID.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TOTSALID.

       SELECT TOTFECHA
           ASSIGN TO '../TOTFECHA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TOTFECHA.

       DATA DIVISION.

       FILE SECTION.

       FD SUCURSA.
       01 ENTRADA-A.
          05 ENTRADA-A-FECHA               PIC X(10).
          05 ENTRADA-A-COD-VENTA           PIC X(04).
          05 ENTRADA-A-PRODUCTO            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V9(2).

       FD SUCURSB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-COD-VENTA           PIC X(04).
          05 ENTRADA-B-PRODUCTO            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V9(2).

       FD SUCURSC.
       01 ENTRADA-C.
          05 ENTRADA-C-FECHA               PIC X(10).
          05 ENTRADA-C-COD-VENTA           PIC X(04).
          05 ENTRADA-C-PRODUCTO            PIC X(30).
          05 ENTRADA-C-IMPORTE             PIC 9(8)V9(2).

       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA                  PIC X(10).
          05 SALIDA-COD-VENTA              PIC X(04).
          05 SALIDA-PRODUCTO               PIC X(30).
          05 SALIDA-IMPORTE                PIC 9(8)V9(2).

       FD TOTSALID.
       01 REG-TOTALES.
          05 TOT-ARCHIVO                   PIC X(01).
          05 TOT-IMPORTE                   PIC 9(08)v9(02).

       FD TOTFECHA.
       01 REG-TOTAL-FECHA.
          05 TOT-FECHA                     PIC X(10).
          05 TOT-FECHA-IMPORTE             PIC 9(08)v9(02).


       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-SUCURSA                      PIC X(2).
             88 FS-SUCURSA-OK                VALUE '00'.
             88 FS-SUCURSA-EOF               VALUE '10'.
          05 FS-SUCURSB                      PIC X(2).
             88 FS-SUCURSB-OK                VALUE '00'.
             88 FS-SUCURSB-EOF               VALUE '10'.
          05 FS-SUCURSC                      PIC X(2).
             88 FS-SUCURSC-OK                VALUE '00'.
             88 FS-SUCURSC-EOF               VALUE '10'.

          05 FS-VENTATOT                     PIC X(2).
             88 FS-VENTATOT-OK               VALUE '00'.
          05 FS-TOTSALID                     PIC X(2).
             88 FS-TOTSALID-OK               VALUE '00'.
          05 FS-TOTFECHA                     PIC X(2).
             88 FS-TOTFECHA-OK               VALUE '00'.

       01 WS-SUCURSA-EOF                     PIC X(1).
          88 WS-SUCURSA-EOF-YES              VALUE 'Y'.
          88 WS-SUCURSA-EOF-NO               VALUE 'N'.
       01 WS-SUCURSB-EOF                     PIC X(1).
          88 WS-SUCURSB-EOF-YES              VALUE 'Y'.
          88 WS-SUCURSB-EOF-NO               VALUE 'N'.
       01 WS-SUCURSC-EOF                     PIC X(1).
          88 WS-SUCURSC-EOF-YES              VALUE 'Y'.
          88 WS-SUCURSC-EOF-NO               VALUE 'N'.

       01 WS-PROCESAR                        PIC X(1).
          88 SW-PROCESAR-A                   VALUE 'A'.
          88 SW-PROCESAR-B                   VALUE 'B'.
          88 SW-PROCESAR-C                   VALUE 'C'.

       01 WS-VAR-AUX.
          05 WS-CLAVE-FECHA                  PIC X(10)     VALUE SPACES.

          05 WS-SUCURSA-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-SUCURSB-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-SUCURSC-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-SALIDA-CANT-REG              PIC 9(5)      VALUE 0.

          05 WS-TOTSALID-CANT-REG            PIC 9(5)      VALUE 0.
          05 WS-TOTSALID-TOTAL-A             PIC 9(8)V9(2) VALUE 0.
          05 WS-TOTSALID-TOTAL-B             PIC 9(8)V9(2) VALUE 0.
          05 WS-TOTSALID-TOTAL-C             PIC 9(8)V9(2) VALUE 0.
          05 WS-TOTFECHA-CANT-REG            PIC 9(5)      VALUE 0.

          05 WS-IMP-FECH                     PIC 9(8)V9(2) VALUE 0.
          05 WS-FECHA-ANT                    PIC X(10)     VALUE SPACES.

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

           IF FS-SUCURSA-OK OR FS-SUCURSB-OK OR FS-SUCURSC-OK

                PERFORM 3000-CLAVE-MENOR
                   THRU 3000-CLAVE-MENOR-EXIT

              MOVE WS-CLAVE-FECHA TO WS-FECHA-ANT

              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-SUCURSA-EOF-YES AND
                      WS-SUCURSB-EOF-YES AND
                      WS-SUCURSC-EOF-YES

               PERFORM 7200-GRABAR-ARCHIVO-TOTFEC
                  THRU 7200-GRABAR-ARCHIVO-TOTFEC-EXIT

              PERFORM 3900-PROCESAR-TOTSALID
                 THRU 3900-PROCESAR-TOTSALID-EXIT

              PERFORM 8000-CERRAR-ARCHIVOS
                 THRU 8000-CERRAR-ARCHIVOS-EXIT

              PERFORM 9000-TOTALES-ARCHIVO
                 THRU 9000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.

           PERFORM 4100-ABRIR-ARCHIVO-SUCURSA
              THRU 4100-ABRIR-ARCHIVO-SUCURSA-EXIT.

           PERFORM 4200-ABRIR-ARCHIVO-SUCURSB
              THRU 4200-ABRIR-ARCHIVO-SUCURSB-EXIT.

           PERFORM 4300-ABRIR-ARCHIVO-SUCURSC
              THRU 4300-ABRIR-ARCHIVO-SUCURSC-EXIT.

           PERFORM 4400-ABRIR-ARCHIVO-SALIDA
              THRU 4400-ABRIR-ARCHIVO-SALIDA-EXIT.

           PERFORM 4500-ABRIR-ARCHIVO-TOTSAL
              THRU 4500-ABRIR-ARCHIVO-TOTSAL-EXIT.

           PERFORM 4600-ABRIR-ARCHIVO-TOTFEC
              THRU 4600-ABRIR-ARCHIVO-TOTFEC-EXIT.


       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      *----SI AUN LOS DOS ARCHIVOS TIENEN REGISTROS POR LEER
                PERFORM 3000-CLAVE-MENOR
                   THRU 3000-CLAVE-MENOR-EXIT

                EVALUATE TRUE
                  WHEN SW-PROCESAR-A
                       PERFORM 3100-PROCESAR-SUCURSA
                          THRU 3100-PROCESAR-SUCURSA-EXIT
                          UNTIL NOT FS-SUCURSA-OK
                             OR WS-CLAVE-FECHA < ENTRADA-A-FECHA
                  WHEN SW-PROCESAR-B
                        PERFORM 3200-PROCESAR-SUCURSB
                          THRU 3200-PROCESAR-SUCURSB-EXIT
                         UNTIL NOT FS-SUCURSB-OK
                            OR WS-CLAVE-FECHA < ENTRADA-B-FECHA
                  WHEN SW-PROCESAR-C
                        PERFORM 3300-PROCESAR-SUCURSC
                          THRU 3300-PROCESAR-SUCURSC-EXIT
                         UNTIL NOT FS-SUCURSC-OK
                            OR WS-CLAVE-FECHA < ENTRADA-C-FECHA
              END-EVALUATE.


       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-CLAVE-MENOR.
      *----SELECCIONO LA FECHA MENOR

           IF ENTRADA-A-FECHA <= ENTRADA-B-FECHA AND
              ENTRADA-A-FECHA <= ENTRADA-C-FECHA
                MOVE ENTRADA-A-FECHA    TO WS-CLAVE-FECHA
                SET  SW-PROCESAR-A      TO TRUE
           ELSE
                IF ENTRADA-B-FECHA <= ENTRADA-C-FECHA
                   MOVE ENTRADA-B-FECHA TO WS-CLAVE-FECHA
                   SET  SW-PROCESAR-B   TO TRUE
                ELSE
                   MOVE ENTRADA-C-FECHA TO WS-CLAVE-FECHA
                   SET  SW-PROCESAR-C   TO TRUE

                END-IF
           END-IF.

       3000-CLAVE-MENOR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-PROCESAR-SUCURSA.


           MOVE ENTRADA-A             TO SALIDA-TOT.
           ADD ENTRADA-A-IMPORTE      TO WS-TOTSALID-TOTAL-A.

           IF  WS-FECHA-ANT EQUAL ENTRADA-A-FECHA
               ADD ENTRADA-A-IMPORTE  TO WS-IMP-FECH
           ELSE

               PERFORM 7200-GRABAR-ARCHIVO-TOTFEC
                  THRU 7200-GRABAR-ARCHIVO-TOTFEC-EXIT

               MOVE ZEROS             TO WS-IMP-FECH
               ADD ENTRADA-A-IMPORTE  TO WS-IMP-FECH
               MOVE ENTRADA-A-FECHA   TO WS-FECHA-ANT
           END-IF.

           PERFORM 7000-GRABAR-ARCHIVO-SAL
              THRU 7000-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 6100-LEER-SUCURSA
              THRU 6100-LEER-SUCURSA-EXIT.


       3100-PROCESAR-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-PROCESAR-SUCURSB.

           MOVE ENTRADA-B             TO SALIDA-TOT.
           ADD ENTRADA-B-IMPORTE      TO WS-TOTSALID-TOTAL-B.

           IF  WS-FECHA-ANT EQUAL ENTRADA-B-FECHA
               ADD ENTRADA-B-IMPORTE  TO   WS-IMP-FECH
           ELSE

               PERFORM 7200-GRABAR-ARCHIVO-TOTFEC
                  THRU 7200-GRABAR-ARCHIVO-TOTFEC-EXIT

               MOVE ZEROS             TO WS-IMP-FECH
               ADD ENTRADA-B-IMPORTE  TO WS-IMP-FECH
               MOVE ENTRADA-B-FECHA   TO WS-FECHA-ANT
           END-IF.

           PERFORM 7000-GRABAR-ARCHIVO-SAL
              THRU 7000-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 6200-LEER-SUCURSB
              THRU 6200-LEER-SUCURSB-EXIT.

       3200-PROCESAR-SUCURSB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-PROCESAR-SUCURSC.

           MOVE ENTRADA-C            TO SALIDA-TOT.
           ADD ENTRADA-C-IMPORTE     TO WS-TOTSALID-TOTAL-C.

           IF  WS-FECHA-ANT EQUAL ENTRADA-C-FECHA
               ADD ENTRADA-C-IMPORTE TO WS-IMP-FECH
           ELSE

               PERFORM 7200-GRABAR-ARCHIVO-TOTFEC
                  THRU 7200-GRABAR-ARCHIVO-TOTFEC-EXIT

               MOVE ZEROS            TO WS-IMP-FECH
               ADD ENTRADA-C-IMPORTE TO WS-IMP-FECH
               MOVE ENTRADA-C-FECHA  TO WS-FECHA-ANT
           END-IF.

           PERFORM 7000-GRABAR-ARCHIVO-SAL
              THRU 7000-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 6300-LEER-SUCURSC
              THRU 6300-LEER-SUCURSC-EXIT.


       3300-PROCESAR-SUCURSC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3900-PROCESAR-TOTSALID.

           MOVE 'A'                    TO TOT-ARCHIVO.
           MOVE WS-TOTSALID-TOTAL-A    TO TOT-IMPORTE.

           PERFORM 7100-GRABAR-ARCHIVO-TOTSAL
              THRU 7100-GRABAR-ARCHIVO-TOTSAL-EXIT.

           MOVE 'B'                    TO TOT-ARCHIVO.
           MOVE WS-TOTSALID-TOTAL-B    TO TOT-IMPORTE.

           PERFORM 7100-GRABAR-ARCHIVO-TOTSAL
              THRU 7100-GRABAR-ARCHIVO-TOTSAL-EXIT.

           MOVE 'C'                    TO TOT-ARCHIVO.
           MOVE WS-TOTSALID-TOTAL-C    TO TOT-IMPORTE.

           PERFORM 7100-GRABAR-ARCHIVO-TOTSAL
              THRU 7100-GRABAR-ARCHIVO-TOTSAL-EXIT.


       3900-PROCESAR-TOTSALID-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4100-ABRIR-ARCHIVO-SUCURSA.

           OPEN INPUT SUCURSA.

           EVALUATE FS-SUCURSA
               WHEN '00'
                    PERFORM 6100-LEER-SUCURSA
                       THRU 6100-LEER-SUCURSA-EXIT
               WHEN '35'
                    SET WS-SUCURSA-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    SET WS-SUCURSA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4100-ABRIR-ARCHIVO-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4200-ABRIR-ARCHIVO-SUCURSB.

           OPEN INPUT SUCURSB.

           EVALUATE FS-SUCURSB
               WHEN '00'
                    PERFORM 6200-LEER-SUCURSB
                       THRU 6200-LEER-SUCURSB-EXIT
               WHEN '35'
                    SET WS-SUCURSB-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
      * SI NO EXISTE EL ARCHIVO B DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4200-ABRIR-ARCHIVO-SUCURSB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4300-ABRIR-ARCHIVO-SUCURSC.

           OPEN INPUT SUCURSC.

           EVALUATE FS-SUCURSC
               WHEN '00'
                    PERFORM 6300-LEER-SUCURSC
                       THRU 6300-LEER-SUCURSC-EXIT
               WHEN '35'
                    SET WS-SUCURSC-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSC'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSC
      * SI NO EXISTE EL ARCHIVO C DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSC
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4300-ABRIR-ARCHIVO-SUCURSC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4400-ABRIR-ARCHIVO-SALIDA.

           OPEN OUTPUT VENTATOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTATOT'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4400-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4500-ABRIR-ARCHIVO-TOTSAL.

           OPEN OUTPUT TOTSALID.

           EVALUATE FS-TOTSALID
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO TOTSALID'
                    DISPLAY 'FILE STATUS: ' FS-TOTSALID
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4500-ABRIR-ARCHIVO-TOTSAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4600-ABRIR-ARCHIVO-TOTFEC.

           OPEN OUTPUT TOTFECHA.

           EVALUATE FS-TOTFECHA
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO TOTFECHA'
                    DISPLAY 'FILE STATUS: ' FS-TOTFECHA
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4600-ABRIR-ARCHIVO-TOTFEC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       6100-LEER-SUCURSA.

           READ SUCURSA.

           EVALUATE FS-SUCURSA
               WHEN '00'
                    ADD 1                      TO WS-SUCURSA-CANT-REG
               WHEN '10'
                    SET WS-SUCURSA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-A-FECHA
               WHEN OTHER
                    SET WS-SUCURSA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-A-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
           END-EVALUATE.
       6100-LEER-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6200-LEER-SUCURSB.

           READ SUCURSB.

           EVALUATE FS-SUCURSB
               WHEN '00'

                    ADD 1                      TO WS-SUCURSB-CANT-REG
               WHEN '10'
                    SET WS-SUCURSB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-B-FECHA
               WHEN OTHER
                    SET WS-SUCURSB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-B-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
           END-EVALUATE.
       6200-LEER-SUCURSB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6300-LEER-SUCURSC.

           READ SUCURSC.

           EVALUATE FS-SUCURSC
               WHEN '00'
                    ADD 1                      TO WS-SUCURSC-CANT-REG
               WHEN '10'
                    SET WS-SUCURSC-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-C-FECHA
               WHEN OTHER
                    SET WS-SUCURSC-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-C-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSC
           END-EVALUATE.
       6300-LEER-SUCURSC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7000-GRABAR-ARCHIVO-SAL.

           WRITE SALIDA-TOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
           END-EVALUATE.

       7000-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7100-GRABAR-ARCHIVO-TOTSAL.

           WRITE REG-TOTALES.

           EVALUATE FS-TOTSALID
               WHEN '00'
                    ADD 1 TO WS-TOTSALID-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE TOTSALI'
                    DISPLAY 'FILE STATUS: ' FS-TOTSALID
           END-EVALUATE.

       7100-GRABAR-ARCHIVO-TOTSAL-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7200-GRABAR-ARCHIVO-TOTFEC.

           MOVE WS-FECHA-ANT     TO TOT-FECHA
           MOVE WS-IMP-FECH      TO TOT-FECHA-IMPORTE

           WRITE REG-TOTAL-FECHA.

           EVALUATE FS-TOTFECHA
               WHEN '00'
                    ADD 1 TO WS-TOTFECHA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE TOTFECHA'
                    DISPLAY 'FILE STATUS: ' FS-TOTFECHA
           END-EVALUATE.

       7200-GRABAR-ARCHIVO-TOTFEC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8000-CERRAR-ARCHIVOS.

           PERFORM 8100-CERRAR-ARCH-SAL
              THRU 8100-CERRAR-ARCH-SAL-EXIT.

           PERFORM 8200-CERRAR-ARCH-TOTSAL
              THRU 8200-CERRAR-ARCH-TOTSAL-EXIT.

           PERFORM 8300-CERRAR-ARCH-SUCURSA
              THRU 8300-CERRAR-ARCH-SUCURSA-EXIT.

           PERFORM 8400-CERRAR-ARCH-SUCURSB
              THRU 8400-CERRAR-ARCH-SUCURSB-EXIT.

           PERFORM 8500-CERRAR-ARCH-SUCURSC
              THRU 8500-CERRAR-ARCH-SUCURSC-EXIT.

           PERFORM 8600-CERRAR-ARCH-TOTFEC
              THRU 8600-CERRAR-ARCH-TOTFEC-EXIT.

       8000-CERRAR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-CERRAR-ARCH-SAL.

           CLOSE VENTATOT.

           IF NOT FS-VENTATOT-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-VENTATOT
           END-IF.

       8100-CERRAR-ARCH-SAL-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-CERRAR-ARCH-TOTSAL.

           CLOSE TOTSALID.

           IF NOT FS-TOTSALID-OK
              DISPLAY 'ERROR EN CLOSE DE TOTSALID: ' FS-TOTSALID
           END-IF.

       8200-CERRAR-ARCH-TOTSAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       8300-CERRAR-ARCH-SUCURSA.

           CLOSE SUCURSA.

           IF NOT FS-SUCURSA-OK
              DISPLAY 'ERROR EN CLOSE DE SUCURSA: ' FS-SUCURSA
           END-IF.

       8300-CERRAR-ARCH-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8400-CERRAR-ARCH-SUCURSB.

           CLOSE SUCURSB.

           IF NOT FS-SUCURSB-OK
              DISPLAY 'ERROR EN CLOSE DE SUCURSB: ' FS-SUCURSB
           END-IF.

       8400-CERRAR-ARCH-SUCURSB-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-CERRAR-ARCH-SUCURSC.

           CLOSE SUCURSC.

           IF NOT FS-SUCURSC-OK
              DISPLAY 'ERROR EN CLOSE DE SUCURSB: ' FS-SUCURSC
           END-IF.

       8500-CERRAR-ARCH-SUCURSC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8600-CERRAR-ARCH-TOTFEC.

           CLOSE TOTFECHA.

           IF NOT FS-TOTFECHA-OK
              DISPLAY 'ERROR EN CLOSE DE TOTFECHA: ' FS-TOTFECHA
           END-IF.

       8600-CERRAR-ARCH-TOTFEC-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9000-TOTALES-ARCHIVO.

           DISPLAY '================================================'.
           DISPLAY 'CANT. REG. LEIDOS SUCURSAL A: ' WS-SUCURSA-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS SUCURSAL B: ' WS-SUCURSB-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS SUCURSAL C: ' WS-SUCURSC-CANT-REG.
           DISPLAY 'CANT. REG. GRABADOS         : ' WS-SALIDA-CANT-REG.
           DISPLAY 'CANT. REG. GRAB. TOTSALID   : 'WS-TOTSALID-CANT-REG.
           DISPLAY 'CANT. REG. GRAB. TOTFECHA   : 'WS-TOTFECHA-CANT-REG.
           DISPLAY '================================================'.
       9000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL13EJ01.
