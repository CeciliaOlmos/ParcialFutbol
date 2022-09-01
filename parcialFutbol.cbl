      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOVEDADES
           ASSIGN TO
           "..\novedades.dat"
           ORGANIZATION is line sequential.
           SELECT JUGADORES
           ASSIGN TO
           "..\jugadores.dat"
           ORGANIZATION is line sequential.
           SELECT EQUIPOS
           ASSIGN TO
           "..\equipo.dat"
           ORGANIZATION is line sequential.
           SELECT ARCH-SORT
           ASSIGN TO "sortwork".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "..\impOrden.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  NOVEDADES.
       01  nov-reg.
           03 nov-equipo pic 99.
           03 nov-cant-jug pic 9(2).
       01  tabla-jugadores.
           03 largo pic 999.
           03 vec-jugad occurs 1 to 100 DEPENDING on largo value zeros.
               05 nov-jugadores pic x(6).
               05 nov-goles pic 9(2).
       FD  JUGADORES.
       01  jug-cab-reg.
           03 jug-cab-tiporeg pic 9.
           03 jug-cab-equ pic 99.
       01  jug-det-reg.
           03 jug-det-tiporeg pic 9.
           03 jug-det-jug pic x(6).
           03 jug-det-goles pic 9(8).

       FD  EQUIPOS.
       01  equ-reg.
           03 equ-cod pic 9(2).
           03 equ-nombre pic x(15).

       SD  ARCH-SORT.
       01  srt-reg.
           03 srt-cod-equipo pic 99.
           03 srt-cod-jugador pic x(6).
           03 srt-cant-goles pic 9(10).

       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  w-flag-jugadores PIC 9 VALUE ZERO.
       01  w-flag-novedades pic 9 value zero.
       01  w-flag-equipo PIC 9 VALUE ZERO.
       01  w-i pic 99.
       01  w-flag-sort pic 9 value zero.
       01  w-cod-equ-ant pic 99.
       01  w-cod-jug-ant pic x(6).
       01  w-acum-jugador pic 9(10).
       01  w-acum-equipo pic 9(10).
       01  w-nombre-ant pic x(15).

      ****** lineas de impresion **********************************************
       01  cabecera1.
           03  lin-titulo.
               05 filler pic x(27) value spaces.
               05 filler pic x(27) value "ASOCIACION FUTBOL ARGENTINO".
               05 filler pic x(26) value spaces.
        01 cabecera2.
           03  lin-subtitulo.
               05 filler pic x(22) value spaces.
               05 filler pic x(36) value "Estadistica de goleadores"-
           "del torneo".
               05 filler pic x(22) value spaces.
       01  cabecera3.
           03  lin-titulo-jug.
               05 filler pic x(7) value spaces.
               05 filler pic x(13) value "NOMBRE-EQUIPO".
               05 filler pic x(8) value spaces.
               05 filler pic x(10) value "COD-EQUIPO".
               05 filler pic x(8) value space.
               05 filler pic x(11) value "COD-JUGADOR".
               05 filler pic x(8) value space.
               05 filler pic x(10) value "CANT-GOLES".
               05 filler pic x(23) value spaces.
       01  cabecera4.
           03  lin-guarda.
               05 filler pic x(80) value all "*".
       01 detalle1.
           03  lin-det-jug.
               05 filler pic x(17) value spaces.
               05 l-equ-nombre pic 9(2).
               05 filler pic x(17) value spaces.
               05 l-equ-cod pic 9(2).
               05 filler pic x(8) value spaces.
               05 l-jug-cod pic x(6).
               05 filler pic x(8) value spaces.
               05 l-cant-goles pic 9(10).
               05 filler pic x(17) value spaces.
       01 detalle2.
           03  lin-det-equipo.
               05 filler pic x(14) value spaces.
               05 filler pic x(10) value "El equipo ".
               05 l-equipo pic 9(2).
               05 filler pic x(8) value spaces.
               05 filler pic x(8) value "realizo ".
               05 l-equ-total pic 9(10).
               05 filler pic x(14) value "goles en total".
               05 filler pic x(14) value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT ARCH-SORT ASCENDING srt-cod-equipo,
                          ASCENDING srt-cod-jugador
           INPUT PROCEDURE IS input-procedure
           OUTPUT PROCEDURE IS output-procedure.
           STOP RUN.

       input-procedure.

           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-LEER-JUGADORES.
           PERFORM 300-LEER-NOVEDADES.
           PERFORM UNTIL w-flag-jugadores is equal 1 AND
                         w-flag-novedades is equal 1
                IF jug-cab-equ < nov-equipo THEN
                    PERFORM 400-PROCESO-POR-MENOR
                IF jug-cab-equ = nov-equipo THEN
                    PERFORM 500-PROCESO-POR-IGUAL
                    PERFORM 300-LEER-NOVEDADES
                END-IF
           END-PERFORM.

           PERFORM 600-FIN-GENERAL.

       output-procedure.

           PERFORM 700-INICIO-OUTPUT-GENERAL.
           PERFORM UNTIL w-flag-sort IS EQUAL 1
               PERFORM 900-INICIO-EQUIPO
               PERFORM UNTIL w-flag-sort IS EQUAL 1
                   OR srt-cod-equipo IS NOT EQUAL w-cod-equ-ant
                   PERFORM 1000-INICIO-JUGADOR
                   PERFORM 1010-BUSCAR-EQUIPO
                   PERFORM UNTIL w-flag-sort IS EQUAL 1
                   OR srt-cod-equipo IS NOT EQUAL w-cod-equ-ant
                   OR srt-cod-jugador IS NOT EQUAL w-cod-jug-ant
                       PERFORM 1100-PROCESO-JUGADOR
                       PERFORM 800-LEER-ARCH-SORT
                       END-PERFORM
                       PERFORM 1200-FIN-JUGADOR
                   END-PERFORM
               PERFORM 1300-FIN-EQUIPO
           END-PERFORM.
           PERFORM 1310-FIN-OUTPUT-GENERAL.

      ******* RUTINAS INPUT-PROCEDURE *****************************************

       100-INICIO-GENERAL.
           OPEN INPUT NOVEDADES.
           OPEN INPUT JUGADORES.
       200-LEER-JUGADORES.
           READ JUGADORES at end move 1 to w-flag-jugadores.
       300-LEER-NOVEDADES.
           READ NOVEDADES at end move 1 to w-flag-novedades.
       400-PROCESO-POR-MENOR.
           move jug-cab-equ to srt-cod-equipo.
           PERFORM 200-LEER-JUGADORES.
           PERFORM UNTIL w-flag-jugadores is equal 1
                       or jug-cab-tiporeg is equal 1
                       move jug-det-jug to srt-cod-jugador
                       move jug-det-goles to srt-cant-goles
                       RELEASE srt-reg
                       PERFORM 200-LEER-JUGADORES
           END-PERFORM.

       500-PROCESO-POR-IGUAL.
           move jug-cab-equ to srt-cod-equipo.
           PERFORM 200-LEER-JUGADORES.
           PERFORM UNTIL w-flag-jugadores is equal 1
                       or jug-cab-tiporeg is equal 1
                       move jug-det-jug to srt-cod-jugador
                       move jug-det-goles to srt-cant-goles
                       RELEASE srt-reg
                       PERFORM 200-LEER-JUGADORES
           END-PERFORM.

           PERFORM VARYING w-i from 1 by 1 until w-i > largo
               MOVE nov-jugadores(w-i) TO srt-cod-jugador
               move nov-goles(w-i) to srt-cant-goles
               RELEASE srt-reg
           END-PERFORM.

       600-FIN-GENERAL.
           CLOSE JUGADORES.
           CLOSE NOVEDADES.

      ******* RUTINAS OUTPUT-PROCEDURE ****************************************
       700-INICIO-OUTPUT-GENERAL.
           PERFORM 720-ABRO-ARCHIVO.
           PERFORM 730-IMPRIMO-ENCABEZADO.
           PERFORM 800-LEER-ARCH-SORT.

       720-ABRO-ARCHIVO.
           OPEN OUTPUT LISTADO.
           OPEN INPUT EQUIPOS.

       730-IMPRIMO-ENCABEZADO.
           WRITE lis-reg FROM cabecera4 AFTER 1.
           WRITE lis-reg FROM cabecera1 AFTER 1.
           WRITE lis-reg FROM cabecera2 AFTER 1.
           WRITE lis-reg FROM cabecera3 AFTER 1.
           WRITE lis-reg FROM cabecera4 AFTER 1.

       800-LEER-ARCH-SORT.
           RETURN ARCH-SORT AT END MOVE 1 TO w-flag-sort.

       900-INICIO-EQUIPO.
           MOVE srt-cod-equipo TO w-cod-equ-ant.
           MOVE ZERO TO w-acum-equipo.


       1000-INICIO-JUGADOR.
           MOVE srt-cod-jugador TO w-cod-jug-ant.
           MOVE ZERO TO w-acum-jugador.
       1010-BUSCAR-EQUIPO.
           PERFORM UNTIL w-flag-equipo is equal 1
                   or srt-cod-equipo=equ-cod
                   PERFORM 1020-LEER-EQUIPOS
           END-PERFORM.
           PERFORM 1030-MOVER-NOM-EQU.
       1020-LEER-EQUIPOS.
           READ EQUIPOS AT END MOVE 1 TO w-flag-equipo.
       1030-MOVER-NOM-EQU.
           MOVE equ-nombre TO l-equ-nombre.

       1100-PROCESO-JUGADOR.
           PERFORM 1110-ARMO-LIN-SOC.
           WRITE lis-reg FROM detalle1 AFTER 1.
           ADD srt-cant-goles TO w-acum-jugador.

       1110-ARMO-LIN-SOC.
           MOVE srt-cod-jugador TO l-jug-cod.

       1200-FIN-JUGADOR.
           ADD w-acum-jugador TO w-acum-equipo.

       1300-FIN-EQUIPO.
           move srt-cod-equipo to l-equipo.
           move w-acum-equipo to l-equ-total.
           write lis-reg from detalle2 AFTER 1.

       1310-FIN-OUTPUT-GENERAL.
           CLOSE LISTADO.
           CLOSE EQUIPOS.

       END PROGRAM YOUR-PROGRAM-NAME.
