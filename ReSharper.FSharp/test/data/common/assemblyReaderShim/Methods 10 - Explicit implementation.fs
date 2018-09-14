module Module

open System

let f = new Foo()
f.Dispose()

let d = f :> IDisposable
d.Dispose()
