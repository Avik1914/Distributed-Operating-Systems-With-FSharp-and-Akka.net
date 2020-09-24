//Avik Khamaru()
#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic

let squarePyramid param =

    let windowStart, windowEnd, k = param
    // printfn "%d %d" windowStart windowEnd
    for a = windowStart to windowEnd do
        let y=(bigint(a) + bigint(k : int) - bigint.One)
        let x=(bigint(a) - bigint.One)
        let y1=((y) * (y + bigint.One) * (bigint(2) * y + bigint.One)) / bigint(6)
        let x1=((x) * (x + bigint.One) * (bigint(2) * x + bigint.One)) / bigint(6)
       
        let difference = y1 - x1
        if((difference|>float |> sqrt) - (difference|>float |> sqrt |> floor) =0.0) then
            printfn "\n%i" a 

let system = ActorSystem.Create("System")

let spawnChilderen param =
    
    let runList = new List<Async<obj>>()
    let n, k, workRange,childCount = param
    
    // printfn "%d %d" workRange childCount
    
    let childActor = 
        [1 .. childCount]
        |> List.map(fun iter ->   
                        spawn system ("childActor" + string(iter))
                        <| fun mailbox ->
                            let rec loop() = actor {
                                let! msg = mailbox.Receive()
                                match msg with
                                | (_, _, _) as param -> squarePyramid param
                                                        mailbox.Sender() <! "ok"
                                return! loop() }
                            loop())

    let childRef= childActor
    let mutable cnt=1;
    for iter in 0 .. childCount - 1 do
        let  startVal=cnt
        let  endVal=min n (cnt+workRange)
        cnt<-cnt+workRange+1
        runList.Add((childRef.Item(iter) <? ((int)startVal, (int)endVal, int(k))))

    runList

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

let n = int(string (fsi.CommandLineArgs.GetValue 1))
let k = int(string (fsi.CommandLineArgs.GetValue 2))
let cores=Environment.ProcessorCount|>int
let workRange=if n/cores=0 then 1 else n/cores 
Async.RunSynchronously(parentThread <? (n, k, workRange,cores), -1) |> ignore