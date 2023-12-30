# Haskell parser, compiler and assembly machine
> This project consists in two parts thats reassemble a parser, compiler and assembly machine in Haskell.

### Project developed by G5-T1:
- João Miguel Vieira Cardoso - up2021 60%
- HaoChang Fu - up202108730 40%

## Part 1

> In this part we built an assembly machine using Haskell

To begin with, we used custom data types to represent:
- Stack: We used stack as type synonym for a list of StackElement,[StackElement], which is a data type defined to represent literals in order to achieve a mixed list approach with the first element as the topmost element. We declared the following:
    - "TT | FF | Intgr Integer" : TT represents a boolean True , FF represents a boolean False and Intgr represents an Integer literal for the stack as StackElements
- State/Storage: For this structure we opted to import Map as to use Hashmap we'd have to install aditional extension. We defined Pair as (a,b) , first:: Pair a b -> a , second::Pair a b -> b for State in order to access map key value pair with more ease and used it's library's functions.

For the empty structures, createEmptyStack and createEmptyState, we simply returned a [] and Map.empty respectively
For printing the stack and state:
- "stack2Str :: Stack -> String" : It uses list comprehension which maps every stack element using an auxiliar function "stackElemToString::StackElement -> String" that turns StackElement into a string and the result is concatenated by a "," using intercalate during the list comprehension construction.
- "state2Str :: State -> String" : It uses almost the same approach as stack2Str , but in order to access the map key value pairs we used Pair in the process using "stateElemToString::Pair String StackElement->String" that shows "key=value".

For the "run" function , we used pattern matching to match instructions with their respective needed stack elements or state elements and running it recursively till instruction list is empty:
- For arithmetic operations we matched the instructions and the first 2 elements of stack, returning "run" recursively and adding the result of the operation to the top of the stack as head.
- For operations *Tru* and *Fals* and *Push n* we just add *TT* or *FF* to or *n* the top of the stack as head.
- For branch operation we evaluate the if the topmost element is *TT* or *FF* and concatenate the left or right list of instructions of *Branch left right* to the top of the current list of instructions.
- For loop operation, we followed the given example in the specifications and we considered *Loop condition logic*. To unfold this and run it recursively we used branch as an auxiliary function: *run (condition ++ [Branch (logic ++ [Loop condition logic]) [Noop]] ++ code, stack, state)* - in the recursive call we appended *conditon* to the front of the instruction list given by a *Branch*, which will return *logic* part concatenated with result of another recursive instruction *Loop condition logic* for the effect of looping, all of this to the top of the current instruction list, case the top element of stack is *TT* for the branch instruction, else the loop will stop and return *Noop* to the top of the instruction list.
The *Noop* instruction does nothing.
- For storage operations *Fetch x* and *Store x*:
    - *Fetch x* - this instruction uses Map.lookup on the state Map and seeks for the key x to retrieve its value, return a Maybe *Just value*. This value is then pushed onto the top of the stack.
    - *Store x* - this instruction uses *x* as the key and the topmost element of stack as value to store in the State storage. It uses Map.insert which returns a new Map with the inserted key value pair. The return result is then used to update the State.

## Part 2

> This part consists of compiler and parser.

To represent the compiled data, we considered expressions as atomic element and part of statements, being program a list of statements. Many of the data have a tree like structure where they take a left element, right element and node value as the operation.

We divided expressions into 2 types:
- Aritmetic expressions - This is represented by Aexp which can take custom data constructors like *"T | F | Var String | Const Integer | ADDexp Aexp Aexp | SUBexp Aexp Aexp | MULTexp Aexp Aexp"* where T, F , Var and Const represent the base literal for this recursive data type and each one representing boolean expression,String as variable and Integer constant respectively. In order to form an expression we have the recursive constructors like ADDexp which will take another Aexp that could be recursive data or base literal.
- Boolean expressions - This is represented By Bexp which constructors like *"BexpA Aexp | EQexp Bexp Bexp | BoolEQexp Bexp Bexp | LEQexp Bexp Bexp | ANDexp Bexp Bexp | NEGexp Bexp"* where BexpA represents a base expression that represents a Aexp and the rest are recursive data. 
This way we can define recursive functions to parse through each of the data defined and convert them into instructions.

We also defined 4 types of statement ,"Assign String Aexp | Lp Bexp Program | Conditional Bexp Program Program | Print Bexp":
- *Assign String Aexp* - This is the assignment statement, which corresponds to "variable:=expression". It takes a string as assigning variable and Aexp as the right side of the operation that can be recursively parsed.
- *Lp Bexp Program* - This represents the loop statement, "while do". It takes a boolean expression in the left side and the List of statements (Program) in the right side.
- *Conditional Bexp Program Program* - This is the conditional statement,"if then else". This takes a boolean expression and 2 lists of statements so we can construct then "then" and "else" part, representing if-booleanExpression-then-Program-else-Program.
- *Pring Bexp* - Helper data used to pring boolean expression.  **Nao tenho a certeza**

To achieve compiling, we used pattern matching that would execute each case according to the type of expression or statement.

For the expressions we have 2 different functions:
- *compA :: Aexp -> Code* - This will compile every case of arithmetic expression, running compA recursively, being the base case the base literals like *Const,Var,T,F,*, returning the result instruction in a list
- *compB :: Bexp* -> Code- The approach is similar to compA but in consideration to boolean expressions. This also uses an auxiliar function *isBooleanEQ :: Bexp -> Bool* that ensure that the data we are compiling for comparison operations are of boolean type, for boolean comparison and integer comparison.

- *compile :: Program -> Code* - This function takes a list of statements and uses the previously explained code to compile the fields associated with the constructor of each data type. Also runs recursively so it can compile all the statements of a program.
    - For assignment, simply run *compA* on the *aexp* associated with *Assign* data, append it on the front of Store *var* instruction (from *Assign var aexp* data) and append the *compile* result for rest of the program to all of the above.
    - For loop, returns the Loop instruction with *bexp* and *program* where bexp is calculated through *compB* and program calculated through *compile*,recursively. Also appends the result instruction list to the resulting Loop instruction, after compiling the rest of the program. 
    - For conditional, the Branch instruction with compiled program1 and program2 is appended to the compiled instruction on boolean expression of "Conditional bexp program1 program2", and the rest of the compiled program is then appended to the results above. This results in a list of instructions reassembling the conditional operation with the rest of the program compiled into instructions.
    