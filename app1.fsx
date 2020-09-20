#time
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

let range_N=fsi.CommandLineArgs.[1]|> int64;
let length_k=fsi.CommandLineArgs.[2]|> int64;

let mutable result = 0
let mutable result2 = 0.0

let isperfectsuqare x : bool=
    let abc:float = sqrt x
    if (abc - floor(abc)) = 0.0 then
        true
    else
        false


let sumofsquares n k =
    result <- 0
    for i = n to ((n+k)-1) do
        result <- result + (i*i)
        
    result2 <- float(result)
    if (isperfectsuqare result2) = true then
        printfn "perfect square"
    else
        printfn "not perfect"
            
         


let system = ActorSystem.Create("MainActor")




///////////////////////////////////////////////////////Stolen code - see below/////////////////////////////////
open System
open System.Threading
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
//Inputs
let input1=fsi.CommandLineArgs.[1]|> int64;
let input2=fsi.CommandLineArgs.[2]|> int64;
// let input1=100000L
// let input2=2L

//Queue from 1 to input1
let queue = new System.Collections.Generic.Queue<int64>()
for i=1 to input1|>int do
   queue.Enqueue(i|>int64)

 // Utility Functions to check if the number is a perfect square 
let isequal k1 f=
   (k1=f)
let checkifsquare k1= 
     Math.Sqrt(k1)
     |>int
     |>float
     |>fun x->x*x
     |>isequal k1

// Message DataType for child Actors
type CMessage=
   |Increment 
   |Print
type PMessage=
    |Start
    |Check of int64*int64  

 // Create Actor system using Akka  
let system=ActorSystem.Create("example3")


//A function to make model for child actors
let spawn_printer system name=
  let mutable state: int64=0L
  spawn system name<|
         fun mailbox->
             let rec loop()=
                actor{
                   let! msg=mailbox.Receive()
                   match msg with
                   |Increment ->    
                                    if queue.Count<>0 then
                                      let k=queue.Dequeue()
                                      for i=k to k+input2-1L do
                                        state<-state+i*i

                                      let parent=system.ActorSelection("akka://example3/user/parent")
                                      parent.Tell(Check (state,k))
                                      state<-0L
                                    else 
                                      //printfn "Done"
                                      state<- -1L
                                      return ()
                                      
                                  //printf "Still On"
                   |Print ->printf "From child %i" state
                   
                                //printf "Sent to parent"
                   return! loop() 
                }
             loop()

//Parent Actor declaration  
          
let actor= spawn system "parent" <| fun mailbox->

               let child1=spawn_printer mailbox "child1"
               let child2=spawn_printer mailbox "child2"
               let child3=spawn_printer mailbox "child3"
              //  let child4=spawn_printer mailbox "child4"
              //  let child5=spawn_printer mailbox "child5"
              //  let child6=spawn_printer mailbox "child6"

               let rec loop()=
                   actor{
                   let! msg=mailbox.Receive()
                  // printfn "From Parent %A" msg
                   match msg with
                   |Start->         child1.Tell(Increment) 
                                    child2.Tell(Increment)
                                    child3.Tell(Increment)
                                    // child4.Tell(Increment) 
                                    // child5.Tell(Increment) 
                                    // child6.Tell(Increment) 

                                    return()
                           
                   
                          
                   
                   |Check (n ,k)-> if(checkifsquare (n|>float)) then printfn "%d"  k
                                   mailbox.Sender().Tell(Increment)
                                   
                   return! loop()
                    

                   }
               loop()
          

// Creating Refrence for child 
let child=system.ActorSelection("akka://example3/user/parent/child1")
let child2=system.ActorSelection("akka://example3/user/parent/child2")

//Creating ActorSelection of Parent
let parent=system.ActorSelection("akka://example3/user/parent")
#time

parent.Tell(Start)

let mutable q=true
while q do
 
 if queue.Count=0 
  then q<-false

#time

