       IDENTIFICATION DIVISION.
        PROGRAM-ID. AL-INDEX.
        AUTHOR. INBELGA.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
            SELECT OPTIONAL SOCIO ASSIGN TO DISK
                                           "SOCIOS.DAT"
            ORGANIZATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS COD-SOCIO
            ALTERNATE RECORD KEY IS NOMBRE WITH DUPLICATES
            ALTERNATE RECORD KEY IS DNI
            FILE STATUS IS ESTADO-SOCIOS.

       DATA DIVISION.
        FILE SECTION.
         FD SOCIO LABEL RECORD IS STANDARD.
          01 REG-SOCIOS.
            05 COD-SOCIO    PIC X(5).
            05 NOMBRE.
              07 APE1       PIC X(12).
              07 APE2       PIC X(12).
              07 NOMB       PIC X(12).
            05 DIRECCION    PIC X(30).
            05 DNI          PIC X(10).
            05 TELEFONO     PIC X(10).
        
        WORKING-STORAGE SECTION.
         77 C PIC 99.
         77 ESTADO-SOCIOS   PIC XX.
         77 RESPUESTA       PIC X.
           88 DATOS-CORREC              VALUE "S" "s".
         77 FICH-GRAB       PIC X.
         77 LINEA           PIC X(80)   VALUE ALL "Í".
         77 LINEA-OSCURA    PIC X(80)   VALUE ALL "°".
         77 FILA            PIC 9(2).
         77 COLUMNA         PIC 9(2).
         77 TECLA           PIC 99.
           88 ESC                       VALUE 27.
        
        SCREEN SECTION.
         01 COLOR-PANTALLA.
           02 BACKGROUND BLUE.
           02 FOREGROUND WHITE.
       
       PROCEDURE DIVISION.
        INICIO.
            DISPLAY SPACES ERASE.
            DISPLAY COLOR-PANTALLA.
            PERFORM ABRIR-FICHERO.
            PERFORM MASCARA.
            PERFORM INIC-CAMP.
            PERFORM TEST AFTER UNTIL ESC
              PERFORM ACEPTAR-CAMPO-PRINCIPAL
              IF NOT ESC
                PERFORM ACEPTAR-LOS-DEMAS-CAMPOS
                DISPLAY "¨Datos Correctos [S/N]?:"
                                              LINE 23 POSITION 29
                MOVE "S" TO RESPUESTA
                ACCEPT RESPUESTA LINE 23 POSITION 53 UPDATE
                DISPLAY " " LINE 23 POSITION 20 ERASE EOL
                IF DATOS-CORREC
                  PERFORM GRABAR-FICHA
                  IF FICH-GRAB = "S"
                    PERFORM INIC-CAMP
                    PERFORM LIMPIAR-CAMPOS
                  END-IF
                END-IF
              END-IF
            END-PERFORM.
            PERFORM CERRAR-FICHERO.
            EXIT PROGRAM.

       ABRIR-FICHERO.
            OPEN EXTEND SOCIO.
            IF ESTADO-SOCIOS NOT = "00"
              DISPLAY "Estado Del Archivo: "
                                LINE 25 POSITION 30 ESTADO-SOCIOS
              ACCEPT RESPUESTA
            END-IF.

       MASCARA.
            DISPLAY " " ERASE.
            DISPLAY LINEA LINE 1 POSITION 1.
            DISPLAY LINEA-OSCURA LINE 2 POSITION 1.
            DISPLAY "ALTAS DE SOCIOS" LINE 2 POSITION 30.
            DISPLAY LINEA LINE 3 POSITION 1.
            DISPLAY "Codigo del socio__:  " LINE 7  POSITION 15.
            DISPLAY "Primer Apellido___:  " LINE 9  POSITION 15.
            DISPLAY "Segundo Apellido__:  " LINE 11 POSITION 15.
            DISPLAY "Nombre____________:  " LINE 13 POSITION 15.
            DISPLAY "Direcci¢n_________:  " LINE 15 POSITION 15.
            DISPLAY "DNI_______________:  " LINE 17 POSITION 15.
            DISPLAY "Telefono__________:  " LINE 19 POSITION 15.
            DISPLAY LINEA LINE 22 POSITION 1.
            DISPLAY LINEA LINE 24 POSITION 1.

       INIC-CAMP.
            MOVE SPACES TO REG-SOCIOS.

       ACEPTAR-CAMPO-PRINCIPAL.
            DISPLAY "Pulse la tecla ESC para finalizar el programa"
                                       LINE 23 POSITION 15.
            ACCEPT COD-SOCIO LINE 7 POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT TECLA FROM ESCAPE KEY
            DISPLAY " " LINE 23 POSITION 15 ERASE EOL.

       ACEPTAR-LOS-DEMAS-CAMPOS.
            ACCEPT APE1  LINE 9  POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT APE2  LINE 11 POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT NOMB  LINE 13 POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT DIRECCION LINE 15 POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT DNI   LINE 17 POSITION 35 TAB NO BEEP UPDATE.
            ACCEPT TELEFONO LINE 19 POSITION 35 TAB NO BEEP UPDATE.

       GRABAR-FICHA.
            MOVE "S" TO FICH-GRAB.
            WRITE REG-SOCIOS INVALID KEY
              DISPLAY "Error Escritura: " LINE 23 POSITION 10
                                                   ESTADO-SOCIOS
              ACCEPT RESPUESTA LINE 23 POSITION 78 PROMPT
              DISPLAY " "  LINE 23 POSITION 10 ERASE EOL
              MOVE "N" TO FICH-GRAB
            END-WRITE.

       CERRAR-FICHERO.
            CLOSE SOCIO.

       LIMPIAR-CAMPOS.
            PERFORM VARYING C FROM 7 BY 2 UNTIL C = 21
             DISPLAY " " LINE C POSITION 35 ERASE EOL
            END-PERFORM.
            
