module Module

open System

let foo = EventHandler(fun _ _ -> ())

let f = Foo()
f.add_CustomEvent foo
