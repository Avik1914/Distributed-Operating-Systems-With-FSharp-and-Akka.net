open System
let calculate s e =
    let mutable res:double=0.0;
    let mutable tmp:double=0.0;
    let mutable num1:double=0.0;
    let mutable start=1;
    let mutable sum=0;
    

    for i in s .. e do
        sum <- sum+(i*i);
 

    tmp<-double sum
    res<-double sum
    res <- sqrt res
    let num = Convert.ToInt32(res)
    num1<-double num
    if tmp=((num1)*(num1)) then
        printfn "Aniket Dash %d"s
    
let fetchAsync(n,k) =
    async {
        try
            
        with
            | ex -> printfn "%s" (ex.Message);
    }

let main() =
    let n = Console.ReadLine() |> int
    let k = Console.ReadLine() |> int
    let mutable val1=0
    for i in 1 .. n do
       val1<-i+k
       calculate i val1
 
   

       
main()