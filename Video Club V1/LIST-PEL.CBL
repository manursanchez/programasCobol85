	IDENTIFICATION DIVISION.
	 PROGRAM-ID. LISTADO-PELICULA.
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

	      SELECT IMPRE ASSIGN TO PRINTER 'PRN'
	      FILE STATUS IS ESTADO-IMPRESORA.
	
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
	   
	 FD IMPRE LABEL RECORD STANDARD.
	  01 REG-IMPRE PIC X(80).
	   
	 
	 WORKING-STORAGE SECTION.
	  77 ESTADO-PELICULA PIC XX.
	  77 ESTADO-IMPRESORA PIC XX.
	  77 RES PIC X.
	   88 CORRECTOS VALUE 'S' 's' 'N' 'n'.
	  77 TECLA PIC 99.
	   88 ESC VALUE 27.
	   88 ABA VALUE 53.
	   88 F1 VALUE 01.
	   88 F2 VALUE 02.
	   88 F3 VALUE 03.
	  77 ESPERA PIC X.
	  77 CONT PIC 99.
	  77 FIN-FICHERO PIC X.
	  77 HAY-PEL PIC X.
	  77 ORDEN PIC X.
	  77 SI PIC X.
	  77 PANT PIC 99.
	  
	  01 CABECERA.
	   02 FILLER PIC X(30) VALUE SPACES.
	   02 FILLER PIC X(20) VALUE "LISTADO DE PELICULAS".
	   02 FILLER PIC X(30) VALUE SPACES.

	  01 REGISTRO-TITULO.
	   02 TIT PIC X(30).
	   02 FILLER PIC X VALUE SPACES.
	   02 GEN PIC X(15).
	   02 FILLER PIC X VALUE SPACES.
	   02 DIR PIC X(25).
	 
	 SCREEN SECTION.
	  01 AYUDA.
	   02 'ORDENACION (F1: CODIGO , F2: TITULO)' 
		COL 10 LINE 22.
	   02 ' , F3: IMPRIMIR LISTADO'.
	   02 'FLECHA ABAJO: MAS PELICULAS , ESC: VUELVE MENU PPAL' 
		COL 15 LINE 23.
	  01 PANTALLA.
	   02 FOREGROUND BROWN.
	   02 'CODIGO' COL 15 LINE 4.
	   02 'TITULO' COL 25 LINE 4.
	   02 'GENERO' COL 60 LINE 4.
	    
	  01 AZUL.
	   02 BACKGROUND BLUE.
	   02 FOREGROUND WHITE.
	  01 NEGRO.
	   02 BACKGROUND BLACK.
	   
	PROCEDURE DIVISION.
	 INICIO.
		PERFORM ABRIR-FICHERO.
		DISPLAY NEGRO.
		DISPLAY SPACES ERASE.
		PERFORM PONE-PANTALLA.
		DISPLAY AZUL.
		IF ESTADO-PELICULA NOT EQUAL '00'
			PERFORM NO-ARCHIVO
		ELSE    
		   MOVE 1 TO PANT
		   MOVE 'N' TO FIN-FICHERO
		   MOVE 'N' TO HAY-PEL
		   MOVE 'C' TO ORDEN
		   MOVE 6 TO CONT
		   START PELICULA KEY > COD-PELI
		   READ PELICULA NEXT RECORD
			AT END MOVE 'S' TO FIN-FICHERO
		   END-READ
		   PERFORM UNTIL ESC
		      MOVE 'S' TO HAY-PEL
		      DISPLAY COD-PELI POSITION 15 
			 LINE CONT
		      DISPLAY TITULO 
			 POSITION 25 LINE CONT
		      DISPLAY GENERO POSITION 60 
			 LINE CONT
		      IF FIN-FICHERO = 'N'
			 READ PELICULA NEXT RECORD
			   AT END MOVE 'S' TO FIN-FICHERO
			 END-READ
		      END-IF
		      IF FIN-FICHERO = 'N'
			 ADD 1 TO CONT
		      END-IF
		      IF CONT = 16 OR FIN-FICHERO = 'S'
			PERFORM UNTIL F1 OR F2 OR F3 OR ABA 
				OR ESC    
			   ACCEPT RES POSITION 71 LINE 23 UPDATE
			   ACCEPT TECLA FROM ESCAPE KEY
			END-PERFORM
			MOVE ' ' TO RES
			IF CONT = 16
			  MOVE 6 TO CONT
			  ADD 1 TO PANT
			END-IF
			IF F3
			   OPEN OUTPUT IMPRE
			   IF ESTADO-IMPRESORA = '00'
			      WRITE REG-IMPRE FROM CABECERA
				  BEFORE ADVANCING 2 LINES
			      END-WRITE
			      MOVE LOW-VALUE TO TITULO
			      MOVE 'N' TO FIN-FICHERO 
			      START PELICULA KEY > TITULO
			      READ PELICULA NEXT RECORD 
				   AT END MOVE 'S' TO FIN-FICHERO
			      END-READ
			      PERFORM UNTIL FIN-FICHERO = 'S'
				 MOVE TITULO TO TIT
				 MOVE GENERO TO GEN
				 MOVE DIRECTOR TO DIR
				 WRITE REG-IMPRE FROM REGISTRO-TITULO
				 END-WRITE
				 READ PELICULA NEXT RECORD 
				   AT END MOVE 'S' TO FIN-FICHERO
				 END-READ
			      END-PERFORM
			      MOVE 01 TO TECLA
			   ELSE
			      PERFORM NO-IMPRE
			   END-IF   
			   CLOSE IMPRE
			END-IF
			IF F1 
			   MOVE 6 TO CONT
			   MOVE 'N ' TO FIN-FICHERO 
			   MOVE 1 TO PANT
			   MOVE 'C' TO ORDEN
			   MOVE 0 TO COD-PELI
			   START PELICULA KEY > COD-PELI
			   READ PELICULA NEXT RECORD
			     AT END MOVE 'S' TO FIN-FICHERO
			END-IF
			IF F2 
			   MOVE 6 TO CONT
			   MOVE 'N ' TO FIN-FICHERO 
			   MOVE 1 TO PANT
			   MOVE 'N' TO ORDEN
			   MOVE LOW-VALUE TO TITULO
			   START PELICULA KEY > TITULO
			   READ PELICULA NEXT RECORD
			     AT END MOVE 'S' TO FIN-FICHERO
			END-IF 
			IF ABA AND PANT > 1 AND FIN-FICHERO = 'N'
			  PERFORM PONE-PANTALLA
			END-IF
			IF NOT ESC
			   MOVE 0 TO TECLA
			END-IF
			
		       END-IF
			
		     END-PERFORM
			IF HAY-PEL = 'N'
				PERFORM NO-ARCHIVO
			END-IF
			
		END-IF.
		PERFORM CERRAR-FICHERO
		DISPLAY AZUL.
		IF NOT ESC
		  DISPLAY 'PULSA UNA TECLA PARA CONTINUAR' POSITION 28
			LINE 23
		  ACCEPT ESPERA POSITION 71 LINE 23
		END-IF.
	      EXIT PROGRAM.
	 
	 ABRIR-FICHERO.
		OPEN INPUT PELICULA.

	 CERRAR-FICHERO.
		CLOSE PELICULA.
		
	 NO-ARCHIVO.
		CALL 'S-WINDOW' USING 03 78 21 24
		CANCEL 'S-WINDOW'
		DISPLAY 'NO HAY PELICULAS PARA LISTAR'
			POSITION 29 LINE 22 BLINK.
	 
	 PONE-PANTALLA.
		DISPLAY AZUL
		CALL 'S-WINDOW' USING 03 78 02 18
		CANCEL 'S-WINDOW'
		DISPLAY ' LISTADO DE PELICULAS ' POSITION 32 LINE 2
			REVERSE
		DISPLAY PANTALLA
		DISPLAY AZUL
		CALL 'S-WINDOW' USING 03 78 21 24
		CANCEL 'S-WINDOW'
		DISPLAY AYUDA.
		
	 NO-IMPRE.
		CALL 'S-WINDOW' USING 03 78 21 24
		CANCEL 'S-WINDOW'
		DISPLAY 'ERROR DE IMPRESION' ESTADO-IMPRESORA
			POSITION 29 LINE 22 BLINK.
		
