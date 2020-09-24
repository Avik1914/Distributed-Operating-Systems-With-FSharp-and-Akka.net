(*
 Team Members :- Aniket Dash (75499549), Avik Khamaru (3699-1467)
 University of Florida

                                            Project 1 
            An interesting problem in arithmetic with deep implications to elliptic curve
            theory is the problem of finding perfect squares that are sums of consecutive
            squares. A classic example is the Pythagorean identity:
            32 + 42 = 52 (1)
            that reveals that the sum of squares of 3, 4 is itself a square. A more interesting
            example is Lucas‘ Square Pyramid:
            12 + 22 + ... + 242 = 702 (2)
            In both of these examples, sums of squares of consecutive integers form the
            square of another integer.
            The goal of this first project is to use F# and the actor model to build a
            good solution to this problem that runs well on multi-core machines.


            Ex1: n = 3, k = 2
                Output: 3

            Ex2: n = 40, k = 24
               Output: 1, 9, 20, 25
*)
#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic

let calculate param =
                     (*
                        Running for the window startNum to endNum where for each 
                        number(from num to num+k) the square sum is calculated with the below formula
                        num * (num + 1) * (2*num + 1) / 6
                     *)

    let startNum, endNum, k = param                
    for i = startNum to endNum do
   
        let y=bigint.Add(bigint(i),bigint.Subtract(bigint(k : int),bigint.One))  // Taking bigint to prevent overflow
        let x=bigint.Subtract(bigint(i),bigint.One)
        let y1=((y) * (y + bigint.One) * (bigint(2) * y + bigint.One)) / bigint(6)
        let x1=((x) * (x + bigint.One) * (bigint(2) * x + bigint.One)) / bigint(6)
       
        let difference = y1 - x1
        if((difference|>float |> sqrt) - (difference|>float |> sqrt |> floor) =0.0) then
            printfn "\n \n%i" i 

let system = ActorSystem.Create("System")

let spawnChilderen param =
    
    let runList = new List<Async<obj>>()
    let n, k, workRange,childCount = param
    
    //Spawing Child Actors
    let childActor = 
        [1 .. childCount]
        |> List.map(fun iter ->   
                        spawn system ("childActor" + string(iter))
                        <| fun mailbox ->
                            let rec loop() = actor {
                                let! msg = mailbox.Receive()
                                match msg with
                                | (_, _, _) as param -> calculate param         // Calling the work unit from the child actor
                                                        mailbox.Sender() <! "ok" //Sending back reference from the actor
                                return! loop() }
                            loop())

    //Dividing the work flow for each actor.
    let mutable cnt=1; //A temp variable which will change its value. 
    for iter in 0 .. childCount - 1 do
        let  startVal=cnt
        let  endVal=min n (cnt+workRange)
        cnt<-cnt+workRange+1
        runList.Add((childActor.Item(iter) <? ((int)startVal, (int)endVal, int(k)))) //Calling child actors and storing child actor reference 

    runList

//Boss Actor creating child actors
let parentThread = 
    spawn system "parentThread"
    <| fun mailbox ->
        let rec loop() = actor {
            let! msg = mailbox.Receive()
            match msg with
            | (_, _, _, _) as param -> let childList = spawnChilderen param
                                       for childRef in childList do
                                            Async.RunSynchronously(childRef, -1) |> ignore
                                       mailbox.Sender() <! "ok"
            return! loop() }
        loop()
//Command line arguments for n and k
let n = int(string (fsi.CommandLineArgs.GetValue 1))
let k = int(string (fsi.CommandLineArgs.GetValue 2))
//no of cores retured by system
let cores=Environment.ProcessorCount|>int
//printfn "NO OF CORES :- %d" cores
let workRange=if n/cores=0 then 1 else n/cores 
Async.RunSynchronously(parentThread <? (n, k, workRange,cores), -1) |> ignore