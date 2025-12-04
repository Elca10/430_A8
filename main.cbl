       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Temporary vars and file structures
           01 EXPRC.
      * 'N' = NumC, 'I' = IdC, 'S' = StrC, 'F' = IfC, 'L' = LambC, 'A' = AppC
               05 EXPR-TAG PIC X.

               05 NUMC REDEFINES EXPRC.
                   10 EXPR-TAG PIC X.
                   10 NUM-VAL PIC S9.

               05 IDC REDEFINES EXPRC. 
                   10 EXPR-TAG PIC X. 
                   10 ID-VAL PIC X.

               05 STRC REDEFINES EXPRC.
                   10 EXPR-TAG PIC X.
                   10 STR-VAL PIC A.
      * Cannot have recursive structures
      * So if statements are limited to basic data types (num, str, id)
               05 IFC REDEFINES EXPRC.
                   10 EXPR-TAG PIC X.
                   10 TEST-VAL PIC X.
                   10 THEN-VAL PIC X.
                   10 ELSE-VAL PIC X.
      * Lambdas only have one arg and simple body
               05 LAMBC REDEFINES EXPRC.
                   10 EXPR-TAG PIC X.
                   10 ARG-VAL PIC X.
                   10 BODY-VAL PIC X.
               05 APPC REDEFINES EXPRC.
                   10 EXPR-TAG PIC X.
                   10 LAMB-VAL PIC X.
                   10 PARAM-VAL PIC X.
      
      * Primitive values and bindings
           01 BINDING.
               05 SYM PIC X.
               05 VAL.
                   10 VAL-TAG PIC X.

                   10 PRIMV REDEFINES VAL.
                       15 VAL-TAG PIC X.
                       15 PRIMV-VAL PIC X.
                   10 NUMV REDEFINES VAL.
                       15 VAL-TAG PIC X.
                       15 NUMV-VAL PIC S9.
                   10 BOOLV REDEFINES VAL.
                       15 VAL-TAG PIC X.
                       15 BOOLV-VAL PIC X.
                   10 STRV REDEFINES VAL.
                       15 VAL-TAG PIC X.
                       15 STRV-VAL PIC A.
                   10 CLOV REDEFINES VAL.
                       15 VAL-TAG PIC X.
                       15 ARG-VAL PIC X.
                       15 BODY-VAL PIC X.
                       15 ENV-VAL.
                           20 ENVSYM PIC X.
                           20 ENV-VAL-VAL PIC X.

                   

       LOCAL-STORAGE SECTION.
      * Also vars and file structures, but these will be allocated and initialized at start of execution

       PROCEDURE DIVISION.
      * This is where program logic goes
      * (Executable statements in our own defined paragraphs and sections)
           
           EVAL-EXPR.
               IF EXPR-TAG = "N"
                   PERFORM EVAL-NUM
               ELSE IF EXPR-TAG = "I"
                   PERFORM EVAL-ID
               ELSE IF EXPR-TAG = "F"
                   PERFORM EVAL-IF
               ELSE IF EXPR-TAG = "L"
                   PERFORM EVAL-LAMBDA
               ELSE IF EXPR-TAG = "A"
                   PERFORM EVAL-APP
               END-IF.
               .

           EVAL-NUM.
      * logic for evaluating a numeric literal
           .

           EVAL-ID.
      * logic for evaluating an identifier
           .

           EVAL-IF.
      * logic for evaluating an if-expression
           .

           EVAL-LAMBDA.
      * logic for lambdas
           .

           EVAL-APP.
      * logic for application
           .    
       STOP RUN.
