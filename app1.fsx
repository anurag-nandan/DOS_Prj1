#if INTERACTIVE
#r @"bin\MCD\Debug\netcoreapp3.1\Akka.FSharp.dll"
#r @"bin\MCD\Debug\netcoreapp3.1\Akka.dll"
#r @"bin\MCD\Debug\netcoreapp3.1\FSharp.Core.dll"
#endif

open System
open Akka.Actor
open Akka.FSharp

type ActorMsg =
    | Verify of int64
    | Start

type ActorMsg2 =
    | Initiate_exe of int64
    | Print

let range_N=fsi.CommandLineArgs.[1]|> int64;
let length_k=fsi.CommandLineArgs.[2]|> int64;

let choices = new System.Collections.Generic.Queue<int64>()

for i = 1 to (range_N)|>int do
    choices.Enqueue(i|>int64)
    
let system = ActorSystem.Create("MainActor")

let sub_actor system name=
    let mutable result: int64=0L
    let mutable select: int64=0L
    spawn system name <|fun mailbox ->
                            let rec loop()=
                                actor{
                                    let! message = mailbox.Receive()
                                    match message with
                                    |Initiate_exe start_val->
                                        let M_Actor = system.ActorSelection("akka://MainActor/user/M_Actor")
                                        
                                        select <- start_val
                                        result<-0L
                                        for j=select to ((select+length_k)-1L) do
                                            result <- result + (j*j)

                                        let result2:float = sqrt(result|>float)
                                        if (result2 - floor(result2)) = 0.0 then
                                            M_Actor.Tell(Verify select)
                                        else
                                            M_Actor.Tell(Verify -1L)
                                        
                                    | Print -> 
                                        printf "in child" 

                                    return! loop()

                                }
                            loop()

let mutable last_reached = false
let mutable flag = false
let Master_Actor = spawn system "M_Actor" <| fun mailbox ->
        let Actor =  
            [1..4]
            |> List.map(fun id-> sub_actor mailbox ("Actor"+(string(id))))
        
        let rec loop()=
            actor{
                let! message = mailbox.Receive()
                match message with
                |Start->
                        for i = 0 to 3 do 
                            if choices.Count <> 0 then
                                let q_val:int64 = choices.Dequeue()
                                Actor.[i].Tell(Initiate_exe q_val)
                                if q_val = range_N then
                                    last_reached <- true
                            
                    
                |Verify response -> if (response <> -1L) then printfn "%d" response
                                    if last_reached = true then
                                        flag <- false
                                    else
                                        if choices.Count <> 0 then
                                            let q_val:int64 = choices.Dequeue()
                                            mailbox.Sender().Tell(Initiate_exe q_val)
                                            if q_val = range_N then
                                                last_reached <- true

                
                return! loop()
            }
        loop()

let M_Actor = system.ActorSelection("akka://MainActor/user/M_Actor")
#time
if range_N > 0L then
    if length_k > 0L then
        M_Actor.Tell(Start)

        flag <- true
        while flag do
            if choices.Count=0 then
                flag <- false

#time

