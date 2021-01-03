        IDENTIFICATION DIVISION.
         PROGRAM-ID. S-WINDOW.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
         77 N PIC 99.       
         77 X PIC 99.
         77 Y PIC 99.
         77 X11 PIC 99.
         77 Y11 PIC 99.
        
        LINKAGE SECTION.
         77 X1 PIC 99.
         77 X2 PIC 99.
         77 Y1 PIC 99.       
         77 Y2 PIC 99.       
                
        PROCEDURE DIVISION USING X1 X2 Y1 Y2.        
          INICIO.      
                DISPLAY "É" POSITION X1 LINE Y1.
                DISPLAY "»" POSITION X2 LINE Y1.
                DISPLAY "È" POSITION X1 LINE Y2.
                DISPLAY "¼" POSITION X2 LINE Y2.
                COMPUTE X11 = X1 + 1.
                COMPUTE Y = Y2 + 1.
                PERFORM VARYING N FROM X11 BY 1 UNTIL N = X2
                 DISPLAY "Í" POSITION N LINE Y1
                 DISPLAY "Í" POSITION N LINE Y2
                 COMPUTE X = N + 1
                 DISPLAY "°" POSITION N LINE Y
                END-PERFORM.
                COMPUTE Y11 = Y1 + 1.
                COMPUTE X = X2 + 1.
                PERFORM VARYING N FROM Y11  BY 1 UNTIL N = Y2 
                 DISPLAY "º" POSITION X1 LINE N
                 DISPLAY "º" POSITION X2 LINE N
                 DISPLAY "°" POSITION X LINE N
                END-PERFORM.
                DISPLAY "°" POSITION X LINE Y2.
                DISPLAY "°" POSITION X LINE Y.
                DISPLAY "°" POSITION X2 LINE Y.
                PERFORM VARYING X FROM X11 BY 1 UNTIL X = X2
                 PERFORM VARYING Y FROM Y11 BY 1 UNTIL Y = Y2
                  DISPLAY " " POSITION X LINE Y REVERSE
                 END-PERFORM
                END-PERFORM.
            EXIT PROGRAM.


