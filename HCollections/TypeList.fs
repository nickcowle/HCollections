namespace HCollections

open System
open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'ts TypeList =
    private
    | Empty of Teq<'ts, unit>
    | Cons of 'ts TypeListConsCrate

and TypeListConsEvaluator<'ts, 'ret> =
    abstract member Eval<'t, 'ts2> : 'ts2 TypeList -> Teq<'ts, 't -> 'ts2> -> 'ret

and 'ts TypeListConsCrate =
    abstract member Apply<'ret> : TypeListConsEvaluator<'ts, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module TypeList =

    let cong (teq : Teq<'ts1, 'ts2>) : Teq<'ts1 TypeList, 'ts2 TypeList> =
        Teq.Cong.believeMe teq

    let empty = Empty Teq.refl

    let cons<'t, 'ts> (types : 'ts TypeList) =
        { new TypeListConsCrate<_> with
            member __.Apply e = e.Eval types Teq.refl<'t -> 'ts>
        }
        |> Cons

    let tail<'t, 'ts> (ts : ('t -> 'ts) TypeList) : 'ts TypeList =
        match ts with
        | Empty _ -> raise Unreachable
        | Cons crate ->
            crate.Apply
                { new TypeListConsEvaluator<_,_> with
                    member __.Eval ts teq =
                        ts |> Teq.castFrom (teq |> Teq.Cong.rangeOf |> cong)
                }

    let split (ts : 'ts TypeList) =
        match ts with
        | Empty teq -> Choice1Of2 teq
        | Cons crate -> Choice2Of2 crate

    let rec toTypes<'ts> (ts : 'ts TypeList) : Type list =
        match ts with
        | Empty _ -> []
        | Cons crate ->
            crate.Apply
                { new TypeListConsEvaluator<_,_> with
                    member __.Eval ts (_ : Teq<_,'a -> _>) =
                        typeof<'a> :: toTypes ts
                }
