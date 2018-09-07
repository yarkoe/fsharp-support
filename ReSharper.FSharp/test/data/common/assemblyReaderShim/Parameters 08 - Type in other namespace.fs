module Module

Ns2.Foo(Ns1.Bar()) |> ignore

open Ns1
open Ns2

Foo(Bar()) |> ignore