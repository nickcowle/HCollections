namespace HCollections.Test

open HCollections
open TypeEquality
open System
open Xunit

module TestHListT =

    [<Fact>]
    let ``HListT to type list is correct for an empty HListT`` () =
        let testHlist = HListT.empty<int>

        Assert.Equal (TypeList.empty, HListT.toTypeList testHlist)

    [<Fact>]
    let ``HListT to type list is correct for an HListT of length 2`` () =
        let testHlist =
            HListT.empty<int>
            |> HListT.cons "hi" 4
            |> HListT.cons 4.5 10

        let expected =
            TypeList.empty
            |> TypeList.cons<string, _>
            |> TypeList.cons<float, _>

        Assert.Equal<Type list> (expected |> TypeList.toTypes, HListT.toTypeList testHlist |> TypeList.toTypes)

    [<Fact>]
    let ``HListT length is right for length 2`` () =
        let testHlist =
            HListT.empty<int>
            |> HListT.cons "hi" 4
            |> HListT.cons 4.5 10

        Assert.Equal (2, HListT.length testHlist)

    [<Fact>]
    let ``Hlist.tolist returns the correct list`` () =
        let testHList =
            HListT.empty<int>
            |> HListT.cons "hi" 4
            |> HListT.cons 4.5 10
            |> HListT.toList

        Assert.Equal<int list> ([10; 4], testHList)

    [<Fact>]
    let ``HList.toList on an empty HListT returns an empty list`` () =
        let testList = HListT.toList HListT.empty<int>

        Assert.Equal<int list> ([], testList)