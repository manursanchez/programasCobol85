	identification division.
	  program-id. alta.
	environment division.
	  input-output section.
	  file-control.
	       select optional pelicula assign to disk
	       "pelicula.dat"
	       organization is indexed
	       access mode is dynamic
	       record key is cod-peli
	       alternate record key is titulo
	       alternate record key is genero with duplicates
	       alternate record key is director with duplicates
	       file status is estado-pelicula.
	data division.
	file section.
	  fd pelicula label record standard.
	  01 reg-pelicula.
	    05 cod-peli pic x(6).
	    05 titulo pic x(30).
	    05 genero pic x(15).
	    05 director pic x(15).
	    05 precio pic 9(5).
	    05 clave-pelicula pic x.
	    05 disponible pic x.
	working-storage section.
	77 estado-pelicula pic xx.
	77 respu pic x.
	  88 datos-correctos value "s", "S".
	77 tecla pic 99.
	  88 esc value 27.
	77 fin-fich pic x.
	77 opc pic x.
	screen section.
	  01 color-portada.
	    02 background blue.

	procedure division.
	inicio.
		display space erase.
		move 's' to opc.
		perform abrir-fichero.
		perform until opc = 'n'
		  display space erase
		  perform portada
		  accept cod-peli line 4 position 45
		  perform pedir-datos
		  perform grabar-ficha
		  display 'Desea dar de alta otra pelicula' line 22
		  position 20
		  perform test after until opc = 's' or 'n'
		    accept opc line 22 position 45
		  end-perform
		end-perform.
		perform cerrar-fichero.
		exit program.

	abrir-fichero.
	       open i-o pelicula.

	portada.
	       display color-portada
	       call 's-window' using 03 77 02 15
	       cancel 's-window'
	       display 'Codigo de peliculaula' line 4 position 25
	       display 'Titulo:' line 6 position 10
	       display 'Genero:' line 8 position 10
	       display 'Director:' line 10 position 10
	       display 'Precio:' line 6 position 55
	       display 'Clave de pelicula:' line 8 position 55
	       display 'Disponible :' line 10 position 55.

	pedir-datos.
		  accept titulo line 6 position 17
		  accept genero line 8 position 17
		  accept director line 10 position 19
		  accept precio line 6 position 63
		  accept clave-pelicula line 8 position 73
		  accept disponible line 10 position 68.

	grabar-ficha.
		  write reg-pelicula invalid key
		   display 'Error de escritura en disco' line 20
		   position 10
		  end-write
		  move spaces to reg-pelicula.

	cerrar-fichero.
		close pelicula.



