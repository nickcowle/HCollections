namespace HCollections.Test

open HCollections
open TypeEquality
open System
open Xunit

module TestHList =

    [<Fact>]
    let ``HList to type list is correct for an empty HList`` () =
        let testHlist = HList.empty

        Assert.Equal (TypeList.empty, HList.toTypeList testHlist)

    [<Fact>]
    let ``HList to type list is correct for an HList of size 1`` () =
        let testHlist = HList.empty |> HList.cons 300

        Assert.Equal<Type list> (TypeList.empty |> TypeList.cons<int, _> |> TypeList.toTypes, HList.toTypeList testHlist |> TypeList.toTypes)

    [<Fact>]
    let ``HList to type list is correct for an HList of size 4`` () =
        let hlist : (float -> int -> string -> bool -> unit) HList =
            HList.empty
            |> HList.cons false
            |> HList.cons "hi"
            |> HList.cons 300
            |> HList.cons 4.0

        let expected =
            TypeList.empty
            |> TypeList.cons<bool, _>
            |> TypeList.cons<string, _>
            |> TypeList.cons<int, _>
            |> TypeList.cons<float, _>

        Assert.Equal<Type list> (TypeList.toTypes expected, HList.toTypeList hlist |> TypeList.toTypes)

    [<Fact>]
    let ``HList.split on an empty HList returns the proof that it's an empty HList`` () =
        let testHList =
            HList.empty
        let result = HList.split testHList
        match result with
        | Choice1Of2 t -> Teq.castFrom t ()
        | Choice2Of2 _ -> failwith "The HList is not empty."

    [<Fact>]
    let ``HList.split on a non-empty HList returns the elements at the head of the HList and the tail`` () =
        let emptyHList =
            HList.empty
        let testHList =
            emptyHList
            |> HList.cons "hi"
        let result = HList.split testHList
        match result with
        | Choice1Of2 _ -> failwith "The HListT is empty."
        | Choice2Of2 c ->
            c.Apply
                { new HListConsEvaluator<string -> unit,int> with
                    member __.Eval (t,head,tail) =
                        let head = Teq.castFrom (Teq.Cong.domainOf t) head
                        let tail = Teq.castFrom (HList.cong (Teq.Cong.rangeOf t)) tail
                        Assert.Equal<_> (emptyHList, tail)
                        Assert.Equal (head, "hi")
                        5
                } |> ignore