(*Variables in F# (Though these are not variables)*)
let myInt=2
let myFloat=3.14
let myString="hello"


(* Lists types F# *)
let oneToFour=[1;2;3;4]
(* :: (cons) operator is used to prepend a element in the start of the list *)
let zeroToFour=0 :: oneToFour
(* @ is a concatenation operator for list *)
let concatoperation= [10;20] @ zeroToFour
printfn "%A" concatoperation

let max a b=if a > b then a else b 
(* We can notice that the type given here is 'a , becuase F# does Automatic Generalization here. As this max function can
    be used for float, int etc, so F# give this task of determining the type to its compiler and its done in runtime *)


(* Functions *)
(* "fun" keyword is used for lambda expressions *)
let squareNum =fun n->n*n
let doubleNum =fun n->2*n

(* Now we can create a list of functions provided the functions are of same signature *)
let listFun=[squareNum;doubleNum]

(* A tuple is a collection that can contain different types of element 
    as below a function and an integer is given as an argument *)
let funWithTuple = ((fun n -> n * n), 10)

(* fst and snd are two special keywork which can be used in tuple just to take a pair of element *)
System.Console.WriteLine((fst funWithTuple)(snd funWithTuple))

(* A good example of function which takes parameters as another function and two integer (x & y)*)
let applyFunction ( h: int -> int -> int) x y = h x y
let mul x y = x * y
let res = applyFunction mul 5 7
printfn "%d" res

//consise format of above program using lambda
let consiseFunction ( h: int -> int -> int) x y = h x y
let finalVal = applyFunction  ( fun x y-> x*y ) 5 7
printfn "Consise Function value %d" finalVal

(* F# provides pipelining function in which a function call can be chained with another call *)
let firstFun n=n+1
let scndFun n=n*6
let pipedVal = 10 |> firstFun |> scndFun
printfn "Piped Val is %d" pipedVal


(* List.Map is used to apply a function to all values of the list*)
let mappedList=List.map (fun n->n*n) oneToFour
printfn "Mapped List is %A" mappedList

(* Higher Order Functions *)
let encapsulate=
    fun func1 func2->
            let toReturn= fun n -> func1(func2 n)
            toReturn

let consiseEncapsulate func1 func2=fun n->func1(func2 n)
            

let computeDoubleMul=encapsulate squareNum doubleNum
printfn "Encapsulated Result is %d" (computeDoubleMul 3)

(* Guess Game showing HIGHER ORDER FUNCTIONS *)
let buildGame desiredNum=
    let game =fun n-> if n=desiredNum then "You Win" else "Better try Next Time"
    game

let gameRef=buildGame 7
printfn "%s" (gameRef 5)
printfn "%s" (gameRef 7)