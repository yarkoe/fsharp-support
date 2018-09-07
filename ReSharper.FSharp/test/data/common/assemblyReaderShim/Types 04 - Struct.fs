module Module

typeof<Foo> |> ignore
typeof<Class> |> ignore

let f = Foo()
System.Nullable<Foo>() |> ignore
System.Nullable<Class>() |> ignore
