#if INTERACTIVE
//#r @"bin\MCD\Debug\netcoreapp3.1\Akka.DI.Core.dll"
#r @"bin\MCD\Debug\netcoreapp3.1\Akka.FSharp.dll"
#r @"bin\MCD\Debug\netcoreapp3.1\Akka.dll"
//#r @"bin\MCD\Debug\netcoreapp3.1\Akka.TestKit.dll"
#r @"bin\MCD\Debug\netcoreapp3.1\FSharp.Core.dll"
#endif

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp

type ActorMsg =
    | Verify of int64
    | Start

type ActorMsg2 =
    | Initiate_exe
    | Print

let range_N=fsi.CommandLineArgs.[1]|> int64;
let length_k=fsi.CommandLineArgs.[2]|> int64;

//let mutable result = 0
//let mutable result2 = 0.0

// let isperfectsuqare x : bool=
//     let abc:float = sqrt x
//     if (abc - floor(abc)) = 0.0 then
//         true
//     else
//         false

// let sumofsquares n k =
//     result <- 0
//     for i = n to ((n+k)-1) do
//         result <- result + (i*i)
        
//     result2 <- float(result)
//     if (isperfectsuqare result2) = true then
//         printfn "perfect square"
//     else
//         printfn "not perfect"


let choices = new System.Collections.Generic.Queue<int64>()
for i = 1 to (range_N+1L)|>int do
    choices.Enqueue(i|>int64)

let system = ActorSystem.Create("MainActor")

let sub_actor system name=
    let mutable result: int64=0L
    spawn system name <|fun mailbox ->
                            let rec loop()=
                                actor{
                                    let! message = mailbox.Receive()
                                    match message with
                                    |Initiate_exe ->
                                        let M_Actor = system.ActorSelection("akka://MainActor/user/M_Actor")
                                        if choices.Count<>0 then
                                            let select = choices.Dequeue()
                                            result<-0L
                                            for j=select to ((select+length_k)-1L) do
                                                result <- result + (j*j)

                                            let result2:float = sqrt(result|>float)
                                            if (result2 - floor(result2)) = 0.0 then
                                                M_Actor.Tell(Verify select)
                                            else
                                                M_Actor.Tell(Verify -1L)
                                        else
                                            //result <- -1L
                                            M_Actor.Tell(Verify -1L)
                                            return()

                                    | Print -> 
                                        printf "in child" 

                                    return! loop()

                                }
                            loop()



let Master_Actor = spawn system "M_Actor" <| fun mailbox ->
        let Actor1 =  sub_actor mailbox "Actor1"
        let Actor2 =  sub_actor mailbox "Actor2"
        let Actor3 =  sub_actor mailbox "Actor3"
        let Actor4 =  sub_actor mailbox "Actor4"
        let rec loop()=
            actor{
                let! message = mailbox.Receive()
                match message with
                |Start->
                    Actor1.Tell(Initiate_exe)
                    Actor2.Tell(Initiate_exe)
                    Actor3.Tell(Initiate_exe)
                    Actor4.Tell(Initiate_exe)
                    return()
                |Verify response -> 
                        if (response <> -1L) then    
                            printfn "%d" response
                        mailbox.Sender().Tell(Initiate_exe)

                return! loop()
            }
        loop()

let M_Actor = system.ActorSelection("akka://MainActor/user/M_Actor")
#time
M_Actor.Tell(Start)

let mutable flag = true

while flag do
    if choices.Count=0 then
        flag <- false


#time
