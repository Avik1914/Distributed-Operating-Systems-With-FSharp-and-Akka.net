#if INTERACTIVE
# time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 
#endif

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic
open System.Diagnostics
(*Discriminated Union for various Type*)
type Commands = 
    |Initailize of IActorRef[]*IActorRef[]*int
    |StartGossip of String
    |ReportLimit of String
    |StartPushSum of Double
    |PushSum of Double * Double * Double
    |Result of Double * Double
    |ReportTime of int
    |ReportNodes of int

// random class for random number generation
let rnd  = System.Random(1)

// stop watch class for time calculation
let stopwatch = new System.Diagnostics.Stopwatch()


// Generic actor class to watch over the nodes to check convergance and report result
type Listener() =
    inherit Actor()
    let mutable msgCount = 0
    let mutable startTime = 0
    let mutable nodeCount =0
    
    (*Overriden OnReceive function which takes message from mailbox queue*)
    override x.OnReceive(rmsg) = 
        match rmsg :?>Commands with  
        | ReportLimit message ->
            msgCount <- msgCount + 1
            if msgCount = nodeCount then
                printfn "Time for convergence: %d ms" (stopwatch.ElapsedMilliseconds)
                Environment.Exit(0)  
           

        | Result (sum,weight) ->
            printfn "Sum = %f Weight= %f Average=%f" sum weight (sum/weight) 
            printfn "Time for convergence: %d ms" (stopwatch.ElapsedMilliseconds)
            Environment.Exit(0)
        | ReportTime strtTime ->
            startTime<- strtTime
            stopwatch.Start()
            

        | ReportNodes nodecount ->
            nodeCount <-nodecount
        | _->()
 

(*Node which denotes each and every actors in the network*)
type Node(listener: IActorRef, numResend: int, nodeNum: int)=
    inherit Actor()
    let mutable msgCount = 0 
    let mutable  neighbours:IActorRef[]=[||] (*Neighbor Array*)
    let mutable  allNodes:IActorRef[]=[||]
    let mutable idx=0

    //used for push sum
    let mutable sum1= nodeNum |> float
    let mutable weight = 1.0
    let mutable termRound = 1

    
 
    override x.OnReceive(num)=
         match num :?>Commands with 
         | Initailize (aref,allref,i)->
                neighbours<-aref
                allNodes<-allref
                idx<-i

         | StartGossip msg ->
                if(msgCount<10) then
                    msgCount<- msgCount+1
                // printfn "Hearing Actor %s %i" Actor.Context.Self.Path.Name numMsgHeard
                if(msgCount=10) then 
                      listener <! ReportLimit(msg)
                
                if(msgCount <10) then
                        let index= rnd.Next(0,neighbours.Length)
                        neighbours.[index] <! StartGossip(msg)
                else
                    let mutable randomIdx = rnd.Next(0,allNodes.Length)
                    while randomIdx = idx do
                      randomIdx <- rnd.Next(0,allNodes.Length)
                    allNodes.[randomIdx]<!StartGossip(msg)

         | StartPushSum delta -> 
                        let index= rnd.Next(0,neighbours.Length)
                        sum1<- sum1/2.0
                        weight <-weight/2.0
                        neighbours.[index] <! PushSum(sum1,weight,delta)

         | PushSum (s:float,w,delta) -> 
                          let  newsum = sum1+s
                          let newweight = weight + w
                          let cal = sum1/weight - newsum/newweight |> abs
                          if(cal >delta) then
                            termRound<- 0
                            sum1 <- sum1+s
                            weight <- weight + w
                            sum1 <- sum1/2.0
                            weight <- weight/2.0
                            let index= rnd.Next(0,neighbours.Length)
                            neighbours.[index] <! PushSum(sum1,weight,delta)
                           elif (termRound>=3) then
                             listener<! Result(sum1,weight)
                            else
                               sum1<- sum1/2.0
                               weight <- weight/2.0
                               termRound<- termRound+1
                               let index= rnd.Next(0,neighbours.Length)
                               neighbours.[index] <! PushSum(sum1,weight,delta)


         | _-> ()


let mutable numOfNodes = int(string (fsi.CommandLineArgs.GetValue 1))
let topology = string (fsi.CommandLineArgs.GetValue 2)
let protocol= string (fsi.CommandLineArgs.GetValue 3)



let system = ActorSystem.Create("System")

//let Props prop
let mutable actualNumOfNodes=float(numOfNodes)


numOfNodes = if topology="2D" || topology="imp2D" then 
                 int(ceil((actualNumOfNodes ** 0.5) ** 2.0))
             else
                 numOfNodes
          

let listener=system.ActorOf(Props.Create(typeof<Listener>),"listener")
//Matching Topology and starting respective algorithms.
match topology  with 
      | "full"->
          let nodeArray= Array.zeroCreate (numOfNodes+1)
          for i in [0..numOfNodes] do
              nodeArray.[i]<-system.ActorOf(Props.Create(typeof<Node>,listener,10,i+1),"demo"+string(i))
          for i in [0..numOfNodes] do
              nodeArray.[i]<!Initailize(nodeArray,nodeArray,i)
              
          let leader = rnd.Next(0,numOfNodes)
          if protocol="gossip" then
            listener<!ReportNodes(numOfNodes)
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Protocol Gossip"
            nodeArray.[leader]<!StartGossip("Hello")
          else if protocol="push-sum" then
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Push Protocol"
            nodeArray.[leader]<!StartPushSum(10.0 ** -10.0)
      |"line"->
          let nodeArray= Array.zeroCreate (numOfNodes+1)
          let mutable neighbourArray=[||]
          for i in [0..numOfNodes] do
              nodeArray.[i]<-system.ActorOf(Props.Create(typeof<Node>,listener,10,i+1),"demo"+string(i))
          for i in [0..numOfNodes] do
              if i=0 then
                  neighbourArray<-[|nodeArray.[((i+1+numOfNodes)%(numOfNodes+1))]|]
              else if i=numOfNodes then
                  neighbourArray<-[|nodeArray.[((i-1+numOfNodes)%(numOfNodes+1))]|]
              else
                  neighbourArray<-[|nodeArray.[((i-1+numOfNodes)%(numOfNodes+1))];nodeArray.[((i+1+numOfNodes)%(numOfNodes+1))]|]
                 
              nodeArray.[i]<!Initailize(neighbourArray,nodeArray,i)
          let leader = System.Random().Next(0,numOfNodes)
          if protocol="gossip" then
            listener<!ReportNodes(numOfNodes)
            listener<!ReportTime(System.DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Starting Protocol Gossip"
            nodeArray.[leader]<!StartGossip("This is Line Topology")
          else if protocol="push-sum" then
            listener<!ReportTime(System.DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Starting Push Sum Protocol for Line"
            nodeArray.[leader]<!StartPushSum(10.0 ** -10.0)

      |"2D"->
           let gridSize=int(ceil(sqrt actualNumOfNodes))
           let totGrid=gridSize*gridSize
           let nodeArray= Array.zeroCreate (totGrid)
           for i in [0..(gridSize*gridSize-1)] do
              nodeArray.[i]<-system.ActorOf(Props.Create(typeof<Node>,listener,10,i+1),"demo"+string(i))
           
           for i in [0..gridSize-1] do
               for j in [0..gridSize-1] do
                    let mutable neighbours:IActorRef[]=[||]
                    if j+1<gridSize then
                        neighbours<-(Array.append neighbours [|nodeArray.[i*gridSize+j+1]|])
                    if j-1>=0 then
                        neighbours<-Array.append neighbours [|nodeArray.[i*gridSize+j-1]|]
                    if i-1>=0 then
                        neighbours<-Array.append neighbours [|nodeArray.[(i-1)*gridSize+j]|]
                    if i+1<gridSize then
                        neighbours<-(Array.append neighbours [|nodeArray.[(i+1)*gridSize+j]|])
                    nodeArray.[i*gridSize+j]<!Initailize(neighbours,nodeArray,i*gridSize+j)

               
           let leader = rnd.Next(0,totGrid-1)  
           if protocol="gossip" then
            listener<!ReportNodes(totGrid-1)
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Protocol Gossip"
            nodeArray.[leader]<!StartGossip("This is 2D Topology")
           else if protocol="push-sum" then
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Push Sum Protocol for 2D Topology"
            nodeArray.[leader]<!StartPushSum(10.0 ** -10.0)
      |"imp2D"->
           let gridSize=int(ceil(sqrt actualNumOfNodes))
           let totGrid=gridSize*gridSize
           let nodeArray= Array.zeroCreate (totGrid)
           for i in [0..(gridSize*gridSize-1)] do
              nodeArray.[i]<-system.ActorOf(Props.Create(typeof<Node>,listener,10,i+1),"demo"+string(i))
           
           for i in [0..gridSize-1] do
               for j in [0..gridSize-1] do
                    let mutable neighbours:IActorRef[]=[||]
                    if j+1<gridSize then
                        neighbours<-(Array.append neighbours [|nodeArray.[i*gridSize+j+1]|])
                    if j-1>=0 then
                        neighbours<-Array.append neighbours [|nodeArray.[i*gridSize+j-1]|]
                    if i-1>=0 then
                        neighbours<-Array.append neighbours [|nodeArray.[(i-1)*gridSize+j]|]
                    if i+1<gridSize then
                        neighbours<-(Array.append neighbours [|nodeArray.[(i+1)*gridSize+j]|])
                    let impNeighbour= rnd.Next(0,numOfNodes)
                    neighbours<-(Array.append neighbours [|nodeArray.[impNeighbour]|])
                    nodeArray.[i*gridSize+j]<!Initailize(neighbours,nodeArray,i*gridSize+j)
               
           let leader = rnd.Next(0,totGrid-1)  
           if protocol="gossip" then
            listener<!ReportNodes(totGrid-1)
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Protocol Gossip"
            nodeArray.[leader]<!StartGossip("This is 2D Topology")
           else if protocol="push-sum" then
            listener<!ReportTime(System.DateTime.Now.Millisecond)
            printfn "Starting Push Sum Protocol for 2D Topology"
            nodeArray.[leader]<!StartPushSum(10.0 ** -10.0)
      | _-> ()
//Waiting for actors to complete.
System.Console.ReadLine()|>ignore