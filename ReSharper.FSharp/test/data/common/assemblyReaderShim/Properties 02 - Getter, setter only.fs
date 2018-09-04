module Module

let f = Foo()

let (i1: int) = f.ReadProperty
f.ReadProperty <- 123

let (i2: int) = f.WriteProperty
f.WriteProperty <- 123


let (i3: int) = Foo.StaticReadProperty
Foo.StaticReadProperty <- 123

let (i4: int) = Foo.StaticWriteProperty
Foo.StaticWriteProperty <- 123
