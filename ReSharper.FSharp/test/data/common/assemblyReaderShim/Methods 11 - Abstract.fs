module Module

type Bar() =
    inherit Foo()

    override x.AbstractMethod() = ()
    override x.Method() = ()

let b = Bar()
b.AbstractMethod()
b.Method()

let f = Foo()
f.AbstractMethod()
f.Method()