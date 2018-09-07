module Module

typeof<Foo1> |> ignore
typeof<Foo2> |> ignore

let f1 = Foo1(fun () -> ())
f1.Invoke()

let f2 = Foo2(fun (s: string) -> ())
f2.Invoke("")

type T1() =
    inherit Foo1(ignore)

type T2() =
    inherit Foo2(ignore)
