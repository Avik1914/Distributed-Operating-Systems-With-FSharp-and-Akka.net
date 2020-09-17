open System
let main() =
    let n = Console.ReadLine() |> int
    let k = Console.ReadLine() |> int
 
    let mutable res:double=0.0;
    let mutable tmp:double=0.0;
    let mutable num1:double=0.0;
    let mutable start=1;
    let mutable sum=0;
    

    for i in 1 .. n+k do
        sum <- sum+(i*i);
        tmp<-double sum
        res<-double sum
        res <- sqrt res
        let num = Convert.ToInt32(res)
        num1<-double num
        if i-start+1=k  then
            if tmp=((num1)*(num1)) then
                printfn "Aniket Dash %d"(i-k+1)
            sum <-sum-(start*start)
            start <- start+1
       
main()