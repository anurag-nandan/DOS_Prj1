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

