       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Temporary vars and file structures
           01 EXPRC.
               05 NUMC PIC S9.
               05 IDC PIC X.
               05 STRC PIC A.
      * Cannot have recursive structures
      * So if statements are limited to basic data types (num, str, id)
               05 IFC.
                   10 TESTC PIC X.
                   10 THENC PIC X.
                   10 ELSEC PIC X.
      * Lambdas only have one arg and simple body
               05 LAMBC.
                   10 ARG PIC X.
                   10 BODY PIC X.
               05 APPC.
                   10 LAMB PIC X.
                   10 PARAM PIC X.


       LOCAL-STORAGE SECTION.
      * Also vars and file structures, but these will be allocated and initialized at start of execution

       PROCEDURE DIVISION.
      * This is where program logic goes
      * (Executable statements in our own defined paragraphs and sections)
       STOP RUN.
