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
- For loop operation, we followed the given example in the specifications and we considered *Loop condition logic*. To unfold this and run it recursively we used branch as an auxiliary function: *run (condition ++ [Branch (logic ++ [Loop condition logic]) [Noop]] ++ code, stack, state)* - in the recursive call we appended *conditon* to the top of the instruction list given by a *Branch*, which will return *logic* part concatenated with result of another recursive instruction *Loop condition logic* for the effect of looping, all of this to the top of the current instruction list, case the top element of stack is *TT* for the branch instruction, else the loop will stop and return *Noop* to the top of the instruction list.
The *Noop* instruction does nothing.
- For storage operations *Fetch x* and *Store x*:
    - *Fetch x* - this instruction uses Map.lookup on the state Map and seeks for the key x to retrieve its value, return a Maybe *Just value*. This value is then pushed onto the top of the stack.
    - *Store x* - this instruction uses *x* as the key and the topmost element of stack as value to store in the State storage. It uses Map.insert which returns a new Map with the inserted key value pair. The return result is then used to update the State.


