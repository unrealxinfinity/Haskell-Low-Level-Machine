# Haskell parser, compiler and assembly machine
> This project consists in two parts thats reassemble a parser, compiler and assembly machine in Haskell.

### Project developed by G5-T1:
- João Miguel Vieira Cardoso - up202108732 60%
- HaoChang Fu - up202108730 40%

## Part 1

> In this part we built an assembly machine using Haskell

To begin with, we used custom data types to represent:
- Stack: We used stack as type synonym for a list of StackElement,[StackElement], which is a data type defined to represent literals in order to achieve a mixed list approach with the first element as the topmost element. We declared the following:
    - "TT | FF | Intgr Integer" : **"TT"** represents a boolean True , **"FF"** represents a boolean False and Intgr represents an Integer literal for the stack as StackElements
- State/Storage: For this structure we opted to import Map as to use Hashmap we'd have to install aditional extension. We defined Pair as (a,b) , **"first:: Pair a b -> a"** , **"second::Pair a b -> b"** for State in order to access map key value pair with more ease and used it's library's functions.

For the empty structures, createEmptyStack and createEmptyState, we simply returned a [] and Map.empty respectively
For printing the stack and state:
- **"stack2Str :: Stack -> String"** : It uses list comprehension which maps every stack element using an auxiliar function **"stackElemToString::StackElement -> String"** that turns StackElement into a string and the result is concatenated by a "," using intercalate during the list comprehension construction.
- **"state2Str :: State -> String"** : It uses almost the same approach as stack2Str , but in order to access the map key value pairs we used Pair in the process using **"stateElemToString::Pair String StackElement->String"** that shows "key=value".

For the "run" function , we used pattern matching to match instructions with their respective needed stack elements or state elements and running it recursively till instruction list is empty:
- For arithmetic operations we matched the instructions and the first 2 elements of stack, returning "run" recursively and adding the result of the operation to the top of the stack as head.
- For operations **"Tru"** and **"Fals"** and **"Push n"** we just add **"TT"** or **"FF"** to or **"n"** the top of the stack as head.
- For branch operation we evaluate the if the topmost element is **"TT"** or **"FF"** and concatenate the left or right list of instructions of **"Branch left right"** to the top of the current list of instructions.
- For loop operation, we followed the given example in the specifications and we considered **"Loop condition logic"**. To unfold this and run it recursively we used branch as an auxiliary function: **"run (condition ++ [Branch (logic ++ [Loop condition logic]) [Noop]] ++ code, stack, state)"** - in the recursive call we appended **"conditon"** to the front of the instruction list given by a **"Branch"**, which will return **"logic"** part concatenated with result of another recursive instruction **"Loop condition logic"** for the effect of looping, all of this to the top of the current instruction list, case the top element of stack is **"TT"** for the branch instruction, else the loop will stop and return **"Noop"** to the top of the instruction list.
The **"Noop"** instruction does nothing.
- For storage operations **"Fetch x"** and **"Store x"**:
    - **"Fetch x"** - this instruction uses Map.lookup on the state Map and seeks for the key x to retrieve its value, return a Maybe **"Just value"**. This value is then pushed onto the top of the stack.
    - **"Store x"** - this instruction uses **"x"** as the key and the topmost element of stack as value to store in the State storage. It uses Map.insert which returns a new Map with the inserted key value pair. The return result is then used to update the State.

## Part 2

> This part consists of compiler and parser.

### Compiler

To represent the compiled data, we considered expressions as atomic element and part of statements, being program a list of statements. Many of the data have a tree like structure where they take a left element, right element and node value as the operation.

We divided expressions into 2 types:
- Aritmetic expressions - This is represented by Aexp which can take custom data constructors like **"T | F | Var String | Const Integer | ADDexp Aexp Aexp | SUBexp Aexp Aexp | MULTexp Aexp Aexp"** where **"T, F , Var and Const"** represent the base literal for this recursive data type and each one representing boolean expression,String as variable and Integer constant respectively. In order to form an expression we have the recursive constructors like ADDexp which will take another Aexp that could be recursive data or base literal.
- Boolean expressions - This is represented By Bexp which constructors like **"BexpA Aexp | EQexp Bexp Bexp | BoolEQexp Bexp Bexp | LEQexp Bexp Bexp | ANDexp Bexp Bexp | NEGexp Bexp"** where BexpA represents a base expression that represents a Aexp and the rest are recursive data. 
This way we can define recursive functions to parse through each of the data defined and convert them into instructions.

We also defined different types of statement ,**"Assign String Aexp | While Bexp Program | For Stm Bexp Stm Program | Conditional Bexp Program Program | Print [Bexp] | Function String [String] Program | Return Bexp | VoidFuncCall String [Bexp] | Class String Program"**:
- **"Assign String Aexp"** - This is the assignment statement, which corresponds to "variable:=expression". It takes a string as assigning variable and Aexp as the right side of the operation that can be recursively parsed.
- **"While Bexp Program"** - This represents the loop statement, "while do". It takes a boolean expression in the left side and the List of statements (Program) in the right side.
- **"Conditional Bexp Program Program"** - This is the conditional statement,"if then else". This takes a boolean expression and 2 lists of statements so we can construct then "then" and "else" part, representing if-booleanExpression-then-Program-else-Program.

Extras:
- **"Print [Bexp]"** - Helper data used to pring boolean expression;
- **"Function String [String] Program"** - Data used to simulate function definition;
- **"Return Bexp"** - Data used to simulate return;
- **"VoidFuncCall string [Bexp]"** - Data used to simulate function call with return type void;
- **"Class String Program"** - Data used to simulate class construction.

To achieve compiling, we used pattern matching that would execute each case according to the type of expression or statement.

For the expressions we have 2 different functions:
- **"compA :: Aexp -> Code"** - This will compile every case of arithmetic expression, running compA recursively, being the base case the base literals like **Const,Var,T,F,**, returning the result instruction in a list
- **"compB :: Bexp"** -> Code- The approach is similar to compA but in consideration to boolean expressions. This also uses an auxiliar function **"isBooleanEQ :: Bexp -> Bool"** that ensure that the data we are compiling for comparison operations are of boolean type, for boolean comparison and integer comparison.

- **"compile :: Program -> Code"** - This function takes a list of statements and uses the previously explained code to compile the fields associated with the constructor of each data type. Also runs recursively so it can compile all the statements of a program.
    - For assignment, simply run **"compA"** on the **"aexp"** associated with **"Assign"** data, append it on the front of Store **"var"** instruction (from **"Assign var aexp"** data) and append the **"compile"** result for rest of the program to all of the above.
    - For loop, returns the Loop instruction with **"bexp"** and **"program"** where bexp is calculated through **"compB"** and program calculated through **"compile"**,recursively. Also appends the result instruction list to the resulting Loop instruction, after compiling the rest of the program. 
    - For conditional, the Branch instruction with compiled program1 and program2 is appended to the compiled instruction on boolean expression of "Conditional bexp program1 program2", and the rest of the compiled program is then appended to the results above. This results in a list of instructions reassembling the conditional operation with the rest of the program compiled into instructions.

### Parser

The parser turns code in string into tokens that are processed into data we defined previously for the compilation process.

The first step was to convert code string into tokens
We defined tokens for the given operations defined in TP2 specifications:
- **"EqualTok | PlusTok | TimesTok | IneqTok | EqTok | NotTok | BoolEqTok | AndTok | OpenParTok | CloseParTok | TrueTok | FalseTok | VarTok String | IntTok Integer | IfTok | ThenTok | ElseTok | ColonTok | WhileTok | DoTok | ForTok | PrintTok | FuncTok | RetTok | QuoteTok | RefTok| ClassTok | CommaTok | DotTok"**
    - EqualTok represents the string ":=", for assignment;
    - PlusTok represents the string "+",for arithmetic expressions;
    - TimesTok represents the string "**" for arithmetic expressions;
    - IneqTok represents the string "<=", for integer inequality;
    - EqTok represents the string "==", for integer equality;
    - NotTok represents the string "not", for negation;
    - BoolEqTok represents the string "=", for boolean equality;
    - AndTok represents the string "and" , for boolean and;
    - OpenParTok represents the string "(", for opening a parentesis;
    - CloseParTok represents the string ")", for closing a parentesis;
    - TrueTok represents the string "True", for boolean true;
    - FalseTok represents the string "False", for boolean false;
    - VarTok String represents a lowercase letter;
    - IntTok Integer represents a number;
    - IfTok represents the keyword "if", for conditional;
    - ThenTok represents the keyword "then", for conditional;
    - ElseTok represents the keyword "else", for condtional;
    - ColonTok represents the string ";", to enclose a statement;
    - WhileTok represents the keyword "while", for while loops;
    - DoTok represents the keyword "do", for loops.
    - ForTok represents the keyword "for", for for loops;
    - PrintTok represents the keyword "print" and "(", for print functions instance;
    - FuncTok represents the keyword for "function ", for functions definitions;
    - RetTok represents the keyword for "return", for function return values;
    - QuoteTok represents the string "\"", for use of strings inside a print functions;
    - RefTok represents the string "&", for use of references in calls to functions;
    - ClassTok represent the string "class " for use of class definitions;
    - CommaTok represents the string ",", for seperating function parameters or print statements;
    - DotTok represents the string ".", for making class queries;

To achieve the conversion , we used the function **"lexer"**:
- **"lexer :: String -> [Token]"** - This function uses pattern matching to verify if any of the above stated strings occured in the input string, if so then converts the keywords and special symbols into tokens stated above. Moreover, to ensure multiple digits are taken into consideration, the lexer function separates the numbers of the rest first by using **"break"** on the input string until a character is not digit. Also to distinguish keyword from variables, the mattern matching partakes in this differenciation and uses the same technique used by numbers to ensure multiple characters are considered for the variable,breaking on the character that is not alphabet.

After converting the input string into tokens, we used the function **"buildData"** to convert it into a tree of data structures we defined for the compiler:
- **"buildData :: [Token] -> Pair Program [Token]"** - This function is a higher order funtion that parses through the list of tokens and converts then into data structures we defined for the compiler. The strategy was to use pattern matching during the recursive call of sub functions until a certain result was achieved,for example, when we parse through all the statements and reach the ";", so we extract the result and the return value from the sub fuction. This strategy was used on the sub functions aswell. Most of these sub functions were defined in a call chain that prioritized certain operations defined in TP2 specifications in order to construct the data tree according to those priorities.
    Those sub functions include:
    - **"parseStm :: [Token] -> Maybe (Stm, [Token])"** - This function parses through the list of tokens and returns the data tree for compiler according to the tokens for assignment, conditional, looping code and function calls or definitions. It returns a Maybe of Just (Stm,[Token]) or Nothing in case no match is found (parse errors). This function calls lower order functions that generate subtrees for the expressions data we defined for the compiler according to their priorities, or subtrees for statements, distinguishing them by using functions **"branchFormat :: [Token] -> Pair Program [Token]" and "getBranch :: [Token] -> Int -> Pair Program [Token]"**.
        - **"parseAndOrBoolEqOrNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])"** - This functions parses the tokens related to AND boolean expressions and adds **ANDexp bexp bexp1"** to the tree;
            - **"parseBoolEqOrNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])"** - This functions parses the tokens related to boolean equality expressions and adds **"BoolEQexp bexp bexp1"** to the tree;
                - **"parseNotOrEqOrIneq :: [Token] -> Maybe (Bexp, [Token])"** - This functions parses the tokens related to negation boolean expressions and adds **"NEGexp bexp"** to the tree;
                    - **"parseEqOrIneq :: [Token] -> Maybe (Bexp, [Token])"** - This functions parses the tokens related to integer equality boolean expressions and adds **"EQexp bexp1 bexp2"** to the tree;
                        - **"parseIneq :: [Token] -> Maybe (Bexp, [Token])"** - This functions parses the tokens related to integer inequality boolean expressions and adds **"LEQexp bexp1 bexp2"** to the tree;
                            - **"parseBexpType :: [Token] -> Maybe (Bexp, [Token])"** - This function checks if theres a nested operation enclosed with "()" withing the boolean expressions. And also processes the base element of boolean expressions "BexpA", within each data defined, adding it to the tree.
        - For arithmetic expressions, we used the same similar approach, using high order function: **"parseSumOrSubOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])"**.


We used the function **"parse"** so we could join the job of **"buildData"** and **"lexer"**
- **"parse :: String -> Program"** - Converts the string into tokens and then originates the data tree for compiler.

In the end, we will obtain a tree with all the statements with their respective expressions and priorities defined, Where the topmost element of the tree executes first and the leaves execute the last.

With this tree, we can call **"compile"** to compile it into a instruction list.


## Extra

We also added the functionality to read the input string from a file so we can simulate the coding experience.

The code for these extra features is located in the Extra.hs file and there are some helper functions located in the Parser file as well that would help just for the compilation of the file code.

By running the commands "make clean", followed by "make" on the same directory as the makefile, you will be able to generate an executable, which you can run by "./{Name of executable} {name of file in .txt format}". It is required to create a .txt file first to pass as argument to the compilation of the code. You can also use the command "make run FILE={name of file}" to run it.

In the file we take use of more features including for loops, print functions, user defined functions and classes:


- **For loop** - to use a for loop you can write "for({Assign statement}; {boolean expression}; {Assign statement}) do ({statements to execute inside for loop if boolean expression is true})". The for loop is similar to a while loop except that you can define a new variable at the start and have an automatic counter at the end of each loop, making it more practical;

- **Print Function** - to use the print function simply write "print({print arguments})". The only way to receive feedback from the file is by using the print function. The print arguments can be boolean expressions, in which case will print True or False to the terminal, arithmetic expression, in which case will print a number to the terminal or a static string by using "'" you can write a static string to the terminal and if you include a "\n" inside that string it will print a new line. You can write more then one argument in the print function. To seperate each argument you have to use the string ",". Of course print functions are a statement so you have to end the print with ";";

- **User Defined Functions** - To use user defined functions you first need to define a function by writing the following "function {name of function} ({function arguments})do({function statements})". To call this function you can simply use {function name}({function arguments}) in regular expressions. For now functions will return using the return statement and the function calls will be replaced with that return statement value inside the function. Take note that inside a function it is local so you can't have access to other outside variables except the ones given by arguments. You can also define functions inside functions that will be only available inside the respective function. You can also pass to the function arguments using a '&' before the variable that you are passing to the function. That way you will be able to change values outside of the functions depending on what it was changed inside the function. You can also call void functions that can or not have return statements.

- **Classes** - To use classes you can simply define one using "class {className} ({varaiables, functions or classes})". To call one you first need to instance the class assigning it to a variable calling the class constructor "a := {className}()" and then you can access statements inside the class using the "." query. For example "print(a.func(1, 2));". The feature of classes is not complete yet, there are many errors because it was developed very late and not on time, but the basis is there to be used.

