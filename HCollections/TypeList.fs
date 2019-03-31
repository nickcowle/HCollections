namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'ts TypeList =
    private
    | Empty of Teq<'ts, unit>
    | Cons of 'ts TypeListConsCrate

and private TypeListConsEvaluator<'ts, 'ret> =
    abstract member Eval<'t, 'ts2> : 'ts2 TypeList -> Teq<'ts, 't -> 'ts2> -> 'ret

and private 'ts TypeListConsCrate =
    abstract member Apply<'ret> : TypeListConsEvaluator<'ts, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module TypeList =

    let empty = Empty Teq.refl

    let cons<'t, 'ts> (types : 'ts TypeList) =
        { new TypeListConsCrate<_> with
            member __.Apply e = e.Eval types Teq.refl<'t -> 'ts>
        }
        |> Cons
