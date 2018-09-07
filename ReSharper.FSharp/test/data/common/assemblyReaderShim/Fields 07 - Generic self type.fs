module Module

let fObj = Foo()
fObj.FooField = Unchecked.defaultof<Foo<obj>> |> ignore

let fString1 = Foo<string>()
let fString2 = Foo<string>()
fString1.FooField = fString2.FooField |> ignore

let fString1 = Foo<string>()
let fInt = Foo<int>()
fString1.FooField = fInt.FooField |> ignore
