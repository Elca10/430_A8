       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
        01 exprc-storage pic x(1000).

      *redefine reuses same memory
      *that's a problem. It means if we store a strc and then a lambc,
      *they occupy the same space and gets overwritten...
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
                
                
        01 val-idx pic 999 value 2.
        01 val-storage pic x(1000).
        
        01 val redefines val-storage.
            05 val-element occurs 100 times.
                10 val-tag pic a.
        
        01 numv redefines val-storage.
            05 numv-element occurs 100 times.
                10 val-tag pic a.
                10 numv-val pic 9.
      *> x means char, x(n) means string, we want operators to be more
      *than 1 char long
        01 primv redefines val-storage.
            05 primv-element occurs 100 times.
                10 val-tag pic a.
                10 primv-val pic x(10).
      *> x means char, x(n) means string, we want parameters to be more
      *than 1 char long
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

       LOCAL-STORAGE SECTION.
       PROCEDURE DIVISION.
        
        main.
      *set up top-env
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

            
      *{interp {NumC 1}}
            move 'n' to exprc-tag of exprc (1).
            move 1 to numc-val (1).
            move 1 to arg.
            perform interp
            
            display val-tag of val (ret).
            display numv-val (ret).
            
      *{interp {IdC '+}
            move 'i' to exprc-tag of exprc (2).
            move '+' to idc-val (2).
            move 2 to arg.
            perform interp
            
            display val-tag of val (ret).
            display primv-val (ret).

      *{interp {IdC '-}
            move 'i' to exprc-tag of exprc (3).
            move '-' to idc-val (3).
            move 3 to arg.
            perform interp
            
            display val-tag of val (ret).
            display primv-val (ret).

      *{interp {IdC '*}
            move 'i' to exprc-tag of exprc (4).
            move '*' to idc-val (4).
            move 4 to arg.
            perform interp
            
            display val-tag of val (ret).
            display primv-val (ret).

      *{interp {IdC '/}
            move 'i' to exprc-tag of exprc (5).
            move '*' to idc-val (5).
            move 5 to arg.
            perform interp
            
            display val-tag of val (ret).
            display primv-val (ret).

      * etc....
            
        STOP RUN.
       

        interp.
            evaluate exprc-tag of exprc (arg)
      *>> TODO: Write functions for missing interps
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
            
            
        
        
       
       

