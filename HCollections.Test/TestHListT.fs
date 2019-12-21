namespace HCollections.Test

open HCollections
open System
open TypeEquality
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
    let ``HlistT.tolist returns the correct list`` () =
        let testHlist =
            HListT.empty<int>
            |> HListT.cons "hi" 4
            |> HListT.cons 4.5 10
            |> HListT.toList

        Assert.Equal<int list> ([10; 4], testHlist)

    [<Fact>]
    let ``HListT.toList on an empty HListT returns an empty list`` () =
        let testList = HListT.toList HListT.empty<int>

        Assert.Equal<int list> ([], testList)

    [<Fact>]
    let ``HListT.toHList returns the correct HList type`` () =
        let testHlist =
            HListT.empty<int>
            |> HListT.cons "hi" 4
            |> HListT.cons 4.5 10
            |> HListT.toHList

        let expected =
            TypeList.empty
            |> TypeList.cons<string, _>
            |> TypeList.cons<float, _>

        Assert.Equal<Type list> (TypeList.toTypes expected, HList.toTypeList testHlist |> TypeList.toTypes)

    [<Fact>]
    let ``HListT.toHList on an empty HListT then getting the type list returns an empty list`` () =
        let testHlist =
            HListT.empty<int>
            |> HListT.toHList

        Assert.Equal<Type list> ([], HList.toTypeList testHlist |> TypeList.toTypes)

    [<Fact>]
    let ``HListT.split on an empty HListT returns the proof that it's an empty HListT`` () =
        let testHListT =
            HListT.empty<int>
        let result = HListT.split testHListT
        match result with
        | Choice1Of2 t -> Teq.castFrom t ()
        | Choice2Of2 _ -> failwith "The HListT is not empty."

    [<Fact>]
    let ``HListT.split on a non-empty HListT returns the elements at the head of the HListT and the tail`` () =
        let emptyHListT =
            HListT.empty<int>
        let testHListT =
            emptyHListT
            |> HListT.cons "hi" 4
        let result = HListT.split testHListT
        match result with
        | Choice1Of2 _ -> failwith "The HListT is empty."
        | Choice2Of2 c ->
            c.Apply
                { new HListTConsEvaluator<string -> unit,int,int> with
                    member __.Eval (head,elem,tail,t) =
                        let head = Teq.castFrom (Teq.Cong.domainOf t) head
                        let tail = Teq.castFrom (HListT.cong (Teq.Cong.rangeOf t) Teq.refl) tail
                        Assert.Equal<_> (emptyHListT, tail)
                        Assert.Equal (elem, 4)
                        Assert.Equal (head, "hi")
                        5
                } |> ignore