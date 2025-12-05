       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
      *Initialize memory of 1000 bytes
       01 exprc-storage pic x(1000).

      *Redefine memory to 100 exprc's, each exprc has size 10 bytes.

      *Purpose of this 'class' is to access its exprc-tag which will
      *tell us what type of exprc it is.

      *After knowing what type of exprc, you can access its attributes
      *properly. e.g exprc-tag = 'n', can retrieve numc-val.
       01 exprc redefines exprc-storage.
           05 exprc-element occurs 100 times.
      *first byte of every exprc is its tag
      *eg. 'n' for numc, 'i' for idc
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
               
      *Should be new-val-idx, the next free index we can use to
      *initialize a value.
       01 val-idx pic 99 value 2.

       01 val-storage pic x(1000).
       
       01 val redefines val-storage.
           05 val-element occurs 100 times.
               10 val-tag pic a.
       
       01 numv redefines val-storage.
           05 numv-element occurs 100 times.
               10 val-tag pic a.
               10 numv-val pic 9.
       01 primv redefines val-storage.
           05 primv-element occurs 100 times.
               10 val-tag pic a.
               10 primv-val pic x.
       01 clov redefines val-storage.
           05 clov-element occurs 100 times.
               10 val-tag pic a.
               10 clov-param-counts pic 9.
               10 clov-param-vals occurs 2 times.
                   15 clov-param-val pic x.
               10 clov-body pic 99.
          
      *bindings aka top-env
       01 bds-table.
           05 bds occurs 100 times indexed by my-index.
               10 bds-var pic x.
               10 bds-val pic 99.
               
      *just a variable to use for function arguments
       01 arg pic 99.

      *just a variable to use for function return values
       01 ret pic 99.

      *NOTE, will probably need a stack frame to keep track of arg/ret 
      *values.

       LOCAL-STORAGE SECTION.
       PROCEDURE DIVISION.
        
       main.
      *set up top-env
           move 'p' to val-tag of val (1).
           move '+' to primv-val (1).

      *Binding(bds-var='+', bds-val=1).
      *'+' will point to the first element (initialized above) of the 
      *val list.
           move '+' to bds-var (1).
           move 1 to bds-val.
            
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
            
       STOP RUN.
       

      *arg is the index of the exprc we are evaluating
       interp.
           evaluate exprc-tag of exprc (arg)
               when "n"
                   perform interp-numc
               when "i"
                   perform interp-idc
           end-evaluate.
           exit paragraph.
        
       interp-numc.
      *Create a new numv at the next free val index.
           move 'n' to val-tag of val (val-idx).
      *initialize new numv's value to numc's value
           move numc-val (arg) to numv-val (val-idx).
      *return index of new numv
           move val-idx to ret.
      *update the next free val index
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
            
            
        
        
       
       

