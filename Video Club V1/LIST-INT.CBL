        IDENTIFICATION DIVISION.
	  PROGRAM-ID. ALTA.

	ENVIRONMENT DIVISION.

	  INPUT-OUTPUT SECTION.

	  FILE-CONTROL.
               SELECT OPTIONAL SOCIOS ASSIGN TO DISK
               "SOCIOS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS COD-SOCIO
               ALTERNATE RECORD KEY IS NOMBRE WITH DUPLICATES
               ALTERNATE RECORD KEY IS DNI
               FILE STATUS IS ESTADO-SOCIOS.


               SELECT OPTIONAL ALQUILER ASSIGN TO DISK
               "ALQUILER.DAT"
	       ORGANIZATION IS INDEXED
	       ACCESS MODE IS DYNAMIC
	       RECORD KEY IS N-SALIDA
	       ALTERNATE RECORD KEY IS CODI-SOCIO
	       ALTERNATE RECORD KEY IS CODI-PELI
	       FILE STATUS IS ESTADO-ALQUILER.




	DATA DIVISION.

	FILE SECTION.
          FD SOCIOS LABEL RECORD STANDARD.
          01 REG-SOCIOS.
            05 COD-SOCIO PIC X(5).
            05 NOMBRE.
              07 APE1 PIC X(12).
              07 APE2 PIC X(12).
              07 NOMB PIC X(12).
            05 DIRECCION PIC X(30).
            05 DNI PIC X(10).
            05 TELEFONO PIC X(10).



          FD ALQUILER LABEL RECORD STANDARD.
	  01 REG-ALQUILER.
	   02 N-SALIDA PIC X(8).
	   02 FECHA-ALQ.
	    03 DIA PIC 99.
	    03 MES PIC 99.
	    03 ANO PIC 9999.
	   02 CODI-SOCIO PIC X(5).
	   02 CODI-PELI PIC X(6).
	   02 PRECI-ALQ PIC 9(4).
	   02 DEVUELTA PIC X.

        WORKING-STORAGE SECTION.
        77 ESTADO-ALQUILER PIC XX.
        77 ESTADO-SOCIOS PIC XX.
        77 FECHA1 PIC X(8).
        77 FECHA2 PIC X(8).
        77 FIN-FICH PIC X.
        77 C PIC 9(2).
        77 ESPERA PIC X.
        PROCEDURE DIVISION.
        INICIO.
               DISPLAY SPACE ERASE.
               OPEN INPUT ALQUILER
               PERFORM MOSTRAR-CAMPOS.
               PERFORM TEST AFTER UNTIL FIN-FICH = 'S'
                 READ ALQUILER NEXT RECORD AT END
                  MOVE 'S' TO FIN-FICH
                 END-READ
                 PERFORM UNTIL C = 20
                  DISPLAY FECHA-ALQ LINE C POSITION 3
                  DISPLAY CODI-PELI LINE C POSITION 12
                  DISPLAY PRECI-ALQ LINE C POSITION 26
                  ADD 1 TO C
                 END-PERFORM
               END-PERFORM.
               ACCEPT ESPERA.
               CLOSE ALQUILER.
               STOP RUN.
        MOSTRAR-CAMPOS.
                       DISPLAY "FECHA" LINE 3 POSITION 3
                       DISPLAY "CODIGO DE LA PELICULA" LINE 3
                       POSITION 12
                       DISPLAY "SOCIO" LINE 3 POSITION 18
                       DISPLAY "PRECIO" LINE 3 POSITION 26
                       ACCEPT ESPERA.