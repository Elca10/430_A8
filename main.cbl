       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Temporary vars and file structures
           01 EXPRC-DEF.
      * 'N' = NumC, 'I' = IdC, 'S' = StrC, 
      * 'F' = IfC, 'L' = LambC, 'A' = AppC
               05 EXPR-TAG PIC X.
               05 EXPRC PIC X(100). *> Base allocation

               05 NUMC REDEFINES EXPRC.
                   10 N-EXPR-TAG PIC X.
                   10 NUM-VAL PIC S9.

               05 IDC REDEFINES EXPRC. 
                   10 I-EXPR-TAG PIC X. 
                   10 ID-VAL PIC X.

               05 STRC REDEFINES EXPRC.
                   10 S-EXPR-TAG PIC X.
                   10 STR-VAL PIC A.
      * Cannot have recursive structures
      * So if statements are limited to basic data types (num, str, id)
               05 IFC REDEFINES EXPRC.
                   10 F-EXPR-TAG PIC X.
                   10 TEST-VAL PIC X.
                   10 THEN-VAL PIC X.
                   10 ELSE-VAL PIC X.
      * Functions can have up to 10 args and only a simple body
               05 LAMBC REDEFINES EXPRC.
                   10 L-EXPR-TAG PIC X.
                   10 L-ARG-COUNT PIC 9.
                   10 L-ARGS OCCURS 10 TIMES.
                       15 L-ARG-TAG PIC X.
                       15 L-ARG-VAL PIC X.
                   10 BODY-VAL PIC X.
               05 APPC REDEFINES EXPRC.
                   10 A-EXPR-TAG PIC X.
                   10 LAMB-VAL PIC X.
                   10 PARAM-COUNT PIC 9.
                   10 PARAMS OCCURS 10 TIMES.
                       15 PARAM-TAG PIC X.
                       15 PARAM-VAL PIC X.
      
      * Primitive values and bindings
           01 BINDING.
               05 SYM PIC X.
               05 VAL-DEF.
      * 'P' = PrimV, 'N' = NumV, 'B' = BoolV, 'S' = StrV, 'C' = ClovV
                   10 VAL-TAG PIC X.
                   10 VAL PIC X(100). *> Base allocation

                   10 PRIMV REDEFINES VAL.
                       15 P-VAL-TAG PIC X.
                       15 PRIMV-VAL PIC X.
                   10 NUMV REDEFINES VAL.
                       15 N-VAL-TAG PIC X.
                       15 NUMV-VAL PIC S9.
                   10 BOOLV REDEFINES VAL.
                       15 B-VAL-TAG PIC X.
                       15 BOOLV-VAL PIC X.
                   10 STRV REDEFINES VAL.
                       15 S-VAL-TAG PIC X.
                       15 STRV-VAL PIC A.
                   10 CLOV REDEFINES VAL.
                       15 C-VAL-TAG PIC X.
                       10 C-ARG-COUNT PIC 9.
                       10 C-ARGS OCCURS 10 TIMES.
                           15 C-ARG-TAG PIC X.
                           15 C-ARG-VAL PIC X.
                       15 BODY-VAL PIC X.
                       15 ENV-VAL.
                           20 ENVSYM PIC X.
                           20 ENV-VAL-VAL PIC X.

                   

       LOCAL-STORAGE SECTION.
      * Also vars and file structures, but these will be allocated 
      * and initialized at start of execution

       PROCEDURE DIVISION.
      * This is where program logic goes
      * (Executable statements in paragraphs and sections)
           EVAL-VAL.
               IF VAL-TAG = "P"
                   PERFORM EVAL-PRIMV
               ELSE IF VAL-TAG = "N"
                   PERFORM EVAL-NUMV
               ELSE IF VAL-TAG = "B"
                   PERFORM EVAL-BOOLV
               ELSE IF VAL-TAG = "S"
                   PERFORM EVAL-STRV
               ELSE IF VAL-TAG = "C"
                   PERFORM EVAL-CLOV
               END-IF.
               .
           
           EVAL-PRIMV.
      * logic for evaluating a primitive value
           .

           EVAL-NUMV.
      * logic for evaluating a numeric value
           .

           EVAL-BOOLV.
      * logic for evaluating a boolean value
           .

           EVAL-STRV.
      * logic for evaluating a string value
           .

           EVAL-CLOV.
      * logic for evaluating a closure
           .


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
