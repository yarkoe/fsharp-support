module Module

let f = Foo()
let (i1: int) = f.Property
f.Property <- 123

let (i2: int) = Foo.StaticProperty
Foo.StaticProperty <- 123
