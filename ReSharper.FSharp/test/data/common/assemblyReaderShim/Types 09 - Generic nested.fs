module Module

typeof<Foo<_>.Bar<_>> |> ignore
typeof<Foo<int>.Bar<int>> |> ignore
typeof<Foo<int>.Bar<string>> |> ignore
typeof<Foo<_>.Bar<Foo<_>.Bar<_>>> |> ignore