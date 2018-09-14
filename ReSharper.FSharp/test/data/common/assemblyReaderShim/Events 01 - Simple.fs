module Module

open System

let foo = Action<_>(fun _ -> ())

let f = Foo()
f.add_SimpleEvent foo
