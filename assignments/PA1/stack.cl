(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class List {
   isNil() : Bool { true };

   head()  : String { { abort(); ""; } };

   tail()  : List { { abort(); self; } };

   cons(i  : String) : List {
      (new Cons).init(i, self)
   };

};

class Cons inherits List {
   car : String;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : String { car };

   tail()  : List { cdr };

   init(i : String, rest : List) : List {
      {
         car <- i;
         cdr <- rest;
         self;
      }
   };

};

class Stack {
   list: List;

   init(): SELF_TYPE {
      {
         list <- new List;
         self;
      }
   };

   push(char: String): SELF_TYPE {
      {
         list <- list.cons(char);
         self;
      }
   };
   
   pop(): String {
      let top: String in {
         top <- list.head();
         list <- list.tail();
         top;
      }
   };

   top(): String {
      list.head()
   };

   isEmpty(): Bool {
      list.isNil()
   };

   traverse(): SELF_TYPE {
      let save: List <- list in (
         let io: IO <- new IO in {
            while list.isNil() = false loop {
               io.out_string(list.head().concat(" "));
               list <- list.tail();
            } pool;
            list <- save;
            io.out_string("\n");
            self;
         }
      )
   };
};

class StackCommand {
   exec(stack: Stack): Stack {
      stack
   };
};

class DisplayCommand inherits StackCommand {
   exec(stack: Stack): Stack {
      {
         stack.traverse();
         stack;
      }
   };
};

class SwapCommand inherits StackCommand {
   exec(stack: Stack): Stack {
      let first: String, second: String in {
         stack.pop();
         first <- stack.pop();
         second <- stack.pop();
         stack.push(first);
         stack.push(second);
         stack;
      }
   };
};

class PlusCommand inherits StackCommand {
   exec(stack: Stack): Stack {
      {
         stack.pop();
         let z: A2I <- new A2I in (
            stack.push(z.i2a(z.a2i(stack.pop()) + z.a2i(stack.pop())))
         );
      }
   };
};

class Main inherits IO {

   stack: Stack <- new Stack.init();

   prompt(): String {
      {
         out_string(">");
         in_string();
      }
   };

   newline(): Object {
      out_string("\n")
   };

   main() : Object {
      (let z : A2I <- new A2I, running: Bool <- true in
         while running = true loop (
            let s: String <- prompt() in (
               if s = "x" then running <- false else
               if s = "e" then
                  if stack.isEmpty() = true then "" else
                  if stack.top() = "+" then new PlusCommand.exec(stack) else
                  if stack.top() = "s" then new SwapCommand.exec(stack)
                  else "" fi fi fi else
               if s = "d" then new DisplayCommand.exec(stack)
               else stack.push(s) fi fi fi
            )
         ) pool
      )
   };

};
