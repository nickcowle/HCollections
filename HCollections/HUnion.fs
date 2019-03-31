namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'ts HUnion =
    private
    | Value of 'ts HUnionValueCrate
    | Extended of 'ts HUnionExtendedCrate

and private HUnionValueEvaluator<'ts, 'ret> =
    abstract member Eval<'t, 'ts2> : 't -> 'ts2 TypeList -> Teq<'ts, 't -> 'ts2> -> 'ret

and private 'ts HUnionValueCrate =
    abstract member Apply<'ret> : HUnionValueEvaluator<'ts, 'ret> -> 'ret

and private HUnionExtendedEvaluator<'ts, 'ret> =
    abstract member Eval<'t, 'ts2> : 'ts2 HUnion -> Teq<'ts, 't -> 'ts2> -> 'ret

and private 'ts HUnionExtendedCrate =
    abstract member Apply : HUnionExtendedEvaluator<'ts, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module HUnion =

    let cong (teq : Teq<'ts1, 'ts2>) : Teq<'ts1 HUnion, 'ts2 HUnion> =
        Teq.Cong.believeMe teq

    let make types value =
        { new HUnionValueCrate<_> with
            member __.Apply e = e.Eval value types Teq.refl
        }
        |> Value

    let extend<'ts, 't> (union : 'ts HUnion) =
        { new HUnionExtendedCrate<_> with
            member __.Apply e = e.Eval union Teq.refl<'t -> 'ts>
        }
        |> Extended

    let split (union : ('t -> 'ts) HUnion) : Choice<'t, 'ts HUnion> =
        match union with
        | Value c ->
            c.Apply
                { new HUnionValueEvaluator<_,_> with
                    member __.Eval v _ teq =
                        v |> Teq.castFrom (Teq.Cong.domainOf teq) |> Choice1Of2
                }
        | Extended c ->
            c.Apply
                { new HUnionExtendedEvaluator<_,_> with
                    member __.Eval union teq =
                        union |> Teq.castFrom (teq |> Teq.Cong.rangeOf |> cong) |> Choice2Of2
                }

    let getSingleton (union : ('t -> unit) HUnion) : 't =
        match union with
        | Value c ->
            c.Apply
                { new HUnionValueEvaluator<_,_> with
                    member __.Eval v _ teq =
                        v |> Teq.castFrom (Teq.Cong.domainOf teq)
                }
        | Extended _ ->
            raise Unreachable
