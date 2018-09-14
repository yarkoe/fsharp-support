module Module

open FSharp.NativeInterop

let (b: byte) = NativePtr.read Foo.Field
