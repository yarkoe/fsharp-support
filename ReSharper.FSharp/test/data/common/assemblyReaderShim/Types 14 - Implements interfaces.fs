module Module

open System

typeof<Foo> |> ignore

let f = Foo()
let d = f :> IDisposable
let c = f :> IComparable
