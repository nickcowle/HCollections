namespace HCollections.Test

open HCollections
open System
open Xunit

module TestTypeList =

    [<Fact>]
    let ``TypeList.toTypes returns the correct types in the correct order`` () =

        let ts : (int -> string -> bool -> unit) TypeList =
            TypeList.empty
            |> TypeList.cons
            |> TypeList.cons
            |> TypeList.cons

        let expected = [ typeof<int> ; typeof<string> ; typeof<bool> ]

        Assert.Equal<Type list>(expected, ts |> TypeList.toTypes)
