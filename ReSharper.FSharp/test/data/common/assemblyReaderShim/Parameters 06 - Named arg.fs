module Module

Foo(123, param2 = true) |> ignore
Foo(param1 = 123, param2 = true) |> ignore
Foo(param2 = true, param1 = 123) |> ignore
