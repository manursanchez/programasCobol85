        IDENTIFICATION DIVISION.
          PROGRAM-ID. ALTA.

        ENVIRONMENT DIVISION.

          INPUT-OUTPUT SECTION.

          FILE-CONTROL.
               SELECT OPTIONAL PELICULA ASSIGN TO DISK "PELICULA.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS COD-PELI
               ALTERNATE RECORD KEY IS TITULO
               ALTERNATE RECORD KEY IS GENERO WITH DUPLICATES
               ALTERNATE RECORD KEY IS DIRECTOR WITH DUPLICATES
               FILE STATUS IS ESTADO-PELICULA.

        DATA DIVISION.

        FILE SECTION.
          FD PELICULA LABEL RECORD STANDARD.
          01 REG-PELICULA.
           02 COD-PELI PIC X(6).
           02 TITULO PIC X(30).
           02 GENERO PIC X(15).
           02 DIRECTOR PIC X(25).
           02 PRECIO PIC 9(4).
           02 CLASE-PELICULA PIC 9.
           02 DISPONIBLE PIC X.

        WORKING-STORAGE SECTION.
        77 EXISTE PIC X.
        77 ESTADO-PELICULA PIC XX.
        77 RESPU PIC X.
          88 DATOS-CORRECTOS VALUE "s", "S".
        77 TECLA PIC 99.
          88 ESC VALUE 27.
        77 FIN-FICH PIC X.
        77 OPC PIC X.
          88 OPCION VALUE "N", "n".
        77 RES PIC X.
         88 CORRECTA VALUE 'S' 's' 'N' 'n'.
        
        SCREEN SECTION.
          01 COLOR-PORTADA.
            02 BACKGROUND BLUE.

        PROCEDURE DIVISION.
        INICIO.
                DISPLAY SPACE ERASE.
                MOVE 'S' TO OPC.
                MOVE 'S' TO EXISTE
                PERFORM ABRIR-FICHERO.
                PERFORM UNTIL OPCION
                  PERFORM PORTADA
                  MOVE 'S' TO EXISTE
                  ACCEPT COD-PELI LINE 4 POSITION 45
                  READ PELICULA KEY IS COD-PELI INVALID KEY
                   MOVE 'N' TO EXISTE
                  END-READ
                  IF EXISTE = 'N'
                    PERFORM PEDIR-DATOS
                    DISPLAY "GRABAR FICHA (S/N)?" POSITION 30 LINE 22
                    PERFORM UNTIL CORRECTA
                       ACCEPT RES POSITION 50 LINE 22
                    END-PERFORM
                    IF RES = 'S' OR RES = 's'
                       PERFORM GRABAR-FICHA
                    END-IF
                  ELSE
                    DISPLAY 'Ese codigo ya ha sido utilizado'
                    LINE 21 POSITION 21
                  END-IF
                  DISPLAY '¨DESEA DAR DE ALTA OTRA PELICULA?' LINE 22
                  POSITION 20
                  ACCEPT OPC LINE 22 POSITION 54
                END-PERFORM.
                PERFORM CERRAR-FICHERO.
              EXIT PROGRAM.

        ABRIR-FICHERO.
               OPEN I-O PELICULA.

        PORTADA.
                DISPLAY COLOR-PORTADA
                CALL 'S-WINDOW' USING 03 77 02 15
                CANCEL 'S-WINDOW'
                DISPLAY ' ALTA DE PELICULA ' LINE 2 POSITION 30
                REVERSE
                DISPLAY 'Codigo de Pelicula:' LINE 4 POSITION 25
                DISPLAY 'Titulo.....................:'
                LINE 6 POSITION 10
                DISPLAY 'Genero.....................:'
                LINE 8 POSITION 10
                DISPLAY 'Director...................:'
                LINE 10 POSITION 10
                DISPLAY 'Precio.....................:'
                LINE 12 POSITION 10
                DISPLAY 'Clase de Pelicula..........:'
                LINE 14 POSITION 10
                CALL 'S-WINDOW' USING 03 77 20 23
                CANCEL 'S-WINDOW'.
                DISPLAY ' AYUDA ' LINE 20 POSITION 35
                REVERSE.

        PEDIR-DATOS.
                  ACCEPT TITULO LINE 6 POSITION 39 TAB NO BEEP
                  ACCEPT GENERO LINE 8 POSITION 39 TAB NO BEEP
                  ACCEPT DIRECTOR LINE 10 POSITION 39 TAB NO BEEP
                  ACCEPT PRECIO LINE 12 POSITION 39 TAB NO BEEP
                  ACCEPT CLASE-PELICULA LINE 14 POSITION 39 TAB
                  NO BEEP
                  MOVE 'S' TO DISPONIBLE.

        GRABAR-FICHA.
                  WRITE REG-PELICULA INVALID KEY
                    DISPLAY 'Error de escritura en disco,' LINE 21
                    POSITION 15
                    DISPLAY 'ficha no grabada ' LINE 21 POSITION 43
                  END-WRITE.

        CERRAR-FICHERO.
                CLOSE PELICULA.

