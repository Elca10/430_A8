# PROJECT
TODO
- tests
- AST definition
- interpreter

Run command: cobc -x -o out main.cbl ./out


Initially defined AST very simply with no recursion, and only allowing single params and such.
Then discovered "REDEFINE".


# FORMATTING

COLUMNS : CONTENT
--------------------------------------------------------------------
1-6     : Reserved for line nums
7       : Indicator (* for comments, - for continuation, / for form feed)
8-11    : Divisions, sections, paragraphs
12-72   : Statements
73-80   : As needed


# DIVISIONS

### DATA DIVISION: define the vars used in the program
Data defined with the following sections: level number, data name, picture clause, value clause
01            TOTAL-STUDENTS            PIC9(5)            VALUE '125'.
|                    |                    |                    |
|                    |                    |                    |
|                    |                    |                    | 
Level Number     Data Name           Picture Clause       Value Clause

Data names must be defined here in order to use them in the procedure division




# SOURCES
Notes from https://www.tutorialspoint.com/cobol/index.htm