       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
        01 exprc-storage pic x(1000).

      *>redefine reuses same memory
      *>that's a problem. It means if we store a strc and then a lambc,
      *>they occupy the same space and gets overwritten...
        01 exprc redefines exprc-storage.
            05 exprc-element occurs 100 times.
                10 exprc-tag pic a.
        
        01 numc redefines exprc-storage.
            05 numc-element occurs 100 times.
                10 exprc-tag pic a.
                10 numc-val pic 9.
        01 idc redefines exprc-storage.
            05 idc-element occurs 100 times.
                10 exprc-tag pic a.
                10 idc-val pic x.
        01 lamc redefines exprc-storage.
            05 lamc-element occurs 100 times.
                10 exprc-tag pic a.
                10 lamc-param-counts pic 9.
                10 lamc-param-vals occurs 2 times.
                    15 lamc-param-val pic x.
                10 lamc-body pic 99.
        01 appc redefines exprc-storage.
            05 appc-element occurs 100 times.
                10 exprc-tag pic a.
                10 appc-arg-counts pic 9.
                10 appc-arg-vals occurs 2 times.
                    15 appc-arg-val pic 99.
                10 appc-body pic 99.
                

      *> Start at slot 11 since 1-10 will contain top-env bindings
        01 val-idx pic 999 value 11.
        01 val-storage pic x(1000).
        
        01 val redefines val-storage.
            05 val-element occurs 100 times.
                10 val-tag pic a.
        
        01 numv redefines val-storage.
            05 numv-element occurs 100 times.
                10 val-tag pic a.
                10 numv-val pic 9.
      *> x means char, x(n) means string, we want operators to be more
      *>than 1 char long
        01 primv redefines val-storage.
            05 primv-element occurs 100 times.
                10 val-tag pic a.
                10 primv-val pic x(10).
      *> x means char, x(n) means string, we want parameters to be more
      *>than 1 char long
        01 clov redefines val-storage.
            05 clov-element occurs 100 times.
                10 val-tag pic a.
                10 clov-param-counts pic 9.
                10 clov-param-vals occurs 2 times.
                    15 clov-param-val pic x(10).
                10 clov-body pic 99.

        01 boolv redefines val-storage.
            05 boolv-element occurs 100 times.
                10 val-tag pic a.
                10 boolv-val pic x(10).
           
        01 bds-table.
            05 bds occurs 100 times indexed by my-index.
                10 bds-var pic x(10).
                10 bds-val pic 99.
                
    
        01 arg pic 99.
        01 ret pic 99.


        *> Variables for testing
        01 TEST-NAME        PIC X(40).
        01 TEST-COUNT       PIC 99 VALUE 0.
        01 TEST-FAIL-COUNT  PIC 99 VALUE 0.


        PROCEDURE DIVISION RECURSIVE.
        
        main.
      *>set up top-env
            move 'p' to val-tag of val (1).

            move '+' to primv-val (1).
            move '+' to bds-var (1).

           move 'p' to val-tag of val (2).
           move '-' to primv-val (2).
           move '-' to bds-var (2).

           move 'p' to val-tag of val (3).
           move '*' to primv-val (3).
           move '*' to bds-var (3).

           move 'p' to val-tag of val (4).
           move '/' to primv-val (4).
           move '/' to bds-var (4).

           move 'p' to val-tag of val (5).
           move '<=' to primv-val (5).
           move '<=' to bds-var (5).

           move 'b' to val-tag of val (6).
           move "true" to boolv-val (6).
           move "true" to bds-var (6).

           move 'b' to val-tag of val (7).
           move "false" to boolv-val (7).
           move "false" to bds-var (7).

           move 'p' to val-tag of val (8).
           move "strlen" to primv-val (8).
           move "strlen" to bds-var (8).

           move 'p' to val-tag of val (9).
           move "equal?" to primv-val (9).
           move "equal?" to bds-var (9).

           move 'p' to val-tag of val (10).
           move "error" to primv-val (10)
           move "error" to bds-var (10)

            
           *> Run tests for interp
           PERFORM TEST-NUMC-1
           PERFORM TEST-NUMC-7
           PERFORM TEST-IDC-PLUS
           PERFORM TEST-IDC-MINUS
           PERFORM TEST-IDC-MULTIPLY
           PERFORM TEST-IDC-DIVIDE

           *> Print summary
           DISPLAY "Total tests: " TEST-COUNT
           DISPLAY "Failed tests: " TEST-FAIL-COUNT

            
        STOP RUN.

        env-extend-many.
           if params-list = 0
               move env-ptr to new-env
               exit paragraph
           end-if

           move param-list-head(params-list) to this-param
           move argvals-head(args-list)      to this-val
     
           add 1 to env-size
           move env-size to next-slot
           move this-param to bds-var(next-slot)
           move this-val   to bds-val(next-slot)
     
           move param-list-tail(params-list) to params-list
           move argvals-tail(args-list)      to args-list
     
           move env-ptr to saved-env-ptr
           move next-slot to env-ptr
     
           perform env-extend-many
     
           move env-ptr to new-env
           move saved-env-ptr to env-ptr
           exit paragraph.
       

        interp.
            evaluate exprc-tag of exprc (arg)
                when "n"
                    perform interp-numc
                when "i"
                    perform interp-idc
                when "f"
                    perform interp-ifc
                when "s"
                    perform interp-strc
                when "l"
                    perform interp-lambc
                when "a"
                    perform interp-appc
                when other
                    display "SHEQ: interp: unknown exprc tag"
                    move 0 to ret
            end-evaluate.
            exit paragraph.
        
        interp-numc.
            move 'n' to val-tag of val (val-idx).
            move numc-val (arg) to numv-val (val-idx).
            move val-idx to ret.
            add 1 to val-idx.
            exit paragraph.
        
        interp-idc.
            set my-index to 1.
            search bds
                when bds-var (my-index) = idc-val (arg)
                    move 'p' to val-tag of val (val-idx)
                    move idc-val (arg) to primv-val (val-idx)
                    move val-idx to ret
                    add 1 to val-idx
            end-search.
            exit paragraph.


        interp-ifc.
           move ifc-test(arg) to arg
           perform interp
           move ret to test-ret

           evaluate val-tag(test-ret)
               when 'b'
                    if boolv-val(test-ret) = "true"
                        move ifc-then(arg) to arg
                        perform interp
                    else
                        move ifc-else(arg) to arg
                        perform interp
                    end-if
               when other
                    display "SHEQ: interp-ifc: test is not a boolean"
                    move 0 to ret
           end-evaluate
           exit paragraph.


        interp-strc.
            move 's' to val-tag (val-idx)
            move strc-val (arg) to strv-val (val-idx)
            move val-idx to ret
            add 1 to val-idx
            EXIT PARAGRAPH.

        interp-lambc.
            move 'c' to val-tag(val-idx)
            move lambc-ids(arg) to clov-ids(val-idx)
            move lambc-body(arg) to clov-body(val-idx)
            move env-ptr to clov-env(val-idx)
            move val-idx to ret
            add 1 to val-idx
            exit paragraph.

        interp-appc.
      *> Evaluate the function expression
           move appc-fexpr(arg) to arg
           perform interp
           move ret to f-ret

      *> Evaluate all arguments
           move appc-args(arg) to arg-list
           perform interp-args
           move args-ret to arg-vals

      *> Now dispatch based on function type
           evaluate val-tag(f-ret)
               when 'p'
      *> primitive operation
                    move primv-val(f-ret) to op
                    perform eval-prim
      *> eval-prim sets ret

               when 'c'
      *> closure call
                    move clov-body(f-ret) to arg
                    perform env-extend-many
                    perform interp

               when other
                    display "SHEQ: interp-appc: application of non-closure"
                    move 0 to ret

            end-evaluate
            exit paragraph.




      *> Test helpers
        TEST-PASS.
            ADD 1 TO TEST-COUNT
            DISPLAY "PASS: " TEST-NAME
            EXIT PARAGRAPH.

        TEST-FAIL.
            ADD 1 TO TEST-COUNT
            ADD 1 TO TEST-FAIL-COUNT
            DISPLAY "FAIL: " TEST-NAME
            EXIT PARAGRAPH.


        *> Test definitions

        *> Parse NumC
        TEST-NUMC-1.
            MOVE "NumC 1 -> NumV 1" TO TEST-NAME

            *> Build AST at slot 1: {NumC 1}
            MOVE "n" TO exprc-tag OF exprc (1)
            MOVE 1   TO numc-val (1)

            *> Call interp on expr index 1
            MOVE 1 TO arg
            PERFORM interp

            *> Check result: NumV 1
            IF val-tag OF val (ret) = "n"
                AND numv-val (ret) = 1
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.


        TEST-NUMC-7.
            MOVE "NumC 7 -> NumV 7" TO TEST-NAME

            *> Build AST at slot 2: {NumC 7}
            MOVE "n" TO exprc-tag OF exprc (2)
            MOVE 7   TO numc-val (2)

            MOVE 2 TO arg
            PERFORM interp

            *> Check result: NumV 7
            IF val-tag OF val (ret) = "n"
                AND numv-val (ret) = 7
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.



        *> Parse IdC
        TEST-IDC-PLUS.
            MOVE "IdC + -> PrimV +" TO TEST-NAME

            *> Build AST at slot 3: {IdC "+"}
            MOVE "i"  TO exprc-tag OF exprc (3)
            MOVE "+"  TO idc-val (3)

            MOVE 3 TO arg
            PERFORM interp

            *> Expect: PrimV "+"
            IF val-tag OF val (ret) = "p"
                AND primv-val (ret) = "+"
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.
        

        TEST-IDC-MINUS.
            MOVE "IdC - -> PrimV -" TO TEST-NAME

            *> Build AST at slot 4: {IdC "-"}
            MOVE "i"  TO exprc-tag OF exprc (4)
            MOVE "-"  TO idc-val (4)

            MOVE 4 TO arg
            PERFORM interp

            *> Expect: PrimV "-"
            IF val-tag OF val (ret) = "p"
                AND primv-val (ret) = "-"
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.


        TEST-IDC-MULTIPLY.
            MOVE "IdC * -> PrimV *" TO TEST-NAME

            *> Build AST at slot 5: {IdC "*"}
            MOVE "i"  TO exprc-tag OF exprc (5)
            MOVE "*"  TO idc-val (5)

            MOVE 5 TO arg
            PERFORM interp

            *> Expect: PrimV "*"
            IF val-tag OF val (ret) = "p"
                AND primv-val (ret) = "*"
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.


        TEST-IDC-DIVIDE.
            MOVE "IdC / -> PrimV /" TO TEST-NAME

            *> Build AST at slot 6: {IdC "/"}
            MOVE "i"  TO exprc-tag OF exprc (6)
            MOVE "/"  TO idc-val (6)

            MOVE 6 TO arg
            PERFORM interp

            *> Expect: PrimV "/"
            IF val-tag OF val (ret) = "p"
                AND primv-val (ret) = "/"
                    PERFORM TEST-PASS
            ELSE
                PERFORM TEST-FAIL
            END-IF

            EXIT PARAGRAPH.





        *> TODO: Once interp-idc is updated to use BoolV for 'true'/'false'
        *> from bds-val, add test to expect val-tag = 'b' and boolv-val.


        *> TODO: Add test for unknown identifier once error prim is implemented









        

            
            
        
        
       
       

