       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Temporary vars and file structures
           01 msg PIC A(12) VALUE "Hello world".
       LOCAL-STORAGE SECTION.
      * Also vars and file structures, but these will be allocated and initialized at start of execution

       PROCEDURE DIVISION.
      * This is where program logic goes
      * (Executable statements in our own defined paragraphs and sections)
           DISPLAY msg.
       STOP RUN.
