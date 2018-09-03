module Module

let (f: int) = Foo.IntField
let (i: Foo) = Foo.FooField

let errorField = Foo.InstanceField
Foo().InstanceField |> ignore