Write a program in C++ that implements the code generation step of our custom compiler of the TINY programming language.

Program input: for this assignment you have the freedom to either take the path of the source code file written in the TINY programming language as an input or hardcode it within your code; You can find an example of an input file in the file called `input.txt`.

Program output: The program should output 3 things

-First: The symbol table of the code basically containing all the named variables in the following format for each variable

[Var=variable name][Mem=initial value of the variable(default is 0, unless the variable was initialized with a different value in an assignment statement][Line=the first line number the variable appears in][Line=the second line number the variable appears in]  and so on for all the other lines the variable is referenced in 

Example of a symbol table:

[Var=x][Mem=1][Line=6][Line=7][Line=20]

[var=y][Mem=0][Line=1]

-Second: The syntax tree; which is basically the same output of the previous assignment but with each non void(Integer or Boolean) node followed by its data type in square brackets.

- All operators are considered of Integer types except for the LessThan and Equal operators; they're considered of Boolean type.
- Numbers and IDs are also considered Integer Types.
- The rest of the node types can be considered Void types, which don't need to be printed(the data type is not the entire node).

Example of an Integer node: [ID][x][Integer]

Example of a Boolean node: [Oper][LessThan][Boolean]

Example of a void node: [Read][x]

Note: In this step you're also expected to perform type checking, so for example if an 'if' is followed by something other than a Boolean node, print to the terminal that there is a problem with that explaining the issue. So something along the lines of "Error, an "if" should be followed by a Boolean".

-Finally: Run a simulation of the compilation of the code as if the program is actually running. The only parts that will interact with the terminal in this step are `Read` and `Write` statements. The rest should be handled internally by your code.

Example of what a `Read x` statement will do:

print this to the terminal "Enter x:" then wait for the user to input that value.

Example of what a `Write 5` statement will do:

print this to the terminal "val: 5" 

Assignment Notes:

-If a certain variable appears in the same line of input code more than once, then the respective line number will be printed in the symbol table for that variable as many times as it appeared, so for example a statement like "X:=X+1" if it appeared let's say in line#7 in the input code, when we output the symbol table and we print that variable node [Line=7] will be printed twice.

-You could implement "[Mem=value]" section in the symbol table to indicate the index of the variable in the table(so if x is the first variable to be declared in the input code, Mem will have a value of 0 and so on), however, if you had already implemented it to be equal to the initial value of the variable both answers will be considered correct.That being said, please note that not printing the initial value of the variable in the symbol table doesn’t mean you should neglect it altogether as you will still need it in the last step when you're traversing the parse tree to keep track of the variables' values.

-A repeat statement in the TINY programming language should mimic the behavior of a do-while loop in c-based programming language.

-A write statement in the TINY programming language should mimic the behavior of a "cout << something << endl;" in c++.
