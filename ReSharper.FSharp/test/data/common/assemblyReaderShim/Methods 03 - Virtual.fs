module Module

type Bar() =
    inherit Foo()

    override x.VirtualMethod() = ()
    override x.Method() = ()
