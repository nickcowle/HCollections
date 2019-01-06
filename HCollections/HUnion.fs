namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'a HUnionTail =
    private
    | Empty of Teq<'a, unit>
    | Extended of 'a HUnionTailExtendedCrate

and private 'a HUnionTailExtendedCrate = abstract member Apply : HUnionTailExtendedCrateEvaluator<'a, 'ret> -> 'ret
and private HUnionTailExtendedCrateEvaluator<'a, 'ret> = abstract member Eval : 'b HUnionTail -> Teq<'a, 'c -> 'b> -> 'ret

[<RequireQualifiedAccess>]
module HUnionTail =

    let empty = Empty Teq.refl

    let extend<'a, 'b> (tail : 'a HUnionTail) =
        { new HUnionTailExtendedCrate<_> with
            member __.Apply e = e.Eval tail Teq.refl<'b -> 'a>
        }
        |> Extended


[<NoComparison>]
[<NoEquality>]
type 'a HUnion =
    private
    | Value of 'a HUnionValueCrate
    | Extended of 'a HUnionExtendedCrate

and private 'a HUnionValueCrate = abstract member Apply : HUnionValueCrateEvaluator<'a, 'ret> -> 'ret
and private HUnionValueCrateEvaluator<'a, 'ret> = abstract member Eval : 'b -> 'c HUnionTail -> Teq<'a, 'b -> 'c> -> 'ret

and private 'a HUnionExtendedCrate = abstract member Apply : HUnionExtendedCrateEvaluator<'a, 'ret> -> 'ret
and private HUnionExtendedCrateEvaluator<'a, 'ret> = abstract member Eval : 'b HUnion -> Teq<'a, 'c -> 'b> -> 'ret

[<RequireQualifiedAccess>]
module HUnion =

    let cong (teq : Teq<'a, 'b>) : Teq<'a HUnion, 'b HUnion> =
        Teq.Cong.believeMe teq

    let make tail value =
        { new HUnionValueCrate<_> with
            member __.Apply e = e.Eval value tail Teq.refl
        }
        |> Value

    let extend<'a, 'b> (union : 'a HUnion) =
        { new HUnionExtendedCrate<_> with
            member __.Apply e = e.Eval union Teq.refl<'b -> 'a>
        }
        |> Extended

    let split (union : ('a -> 'b) HUnion) : Choice<'a, 'b HUnion> =
        match union with
        | Value c ->
            c.Apply
                { new HUnionValueCrateEvaluator<_,_> with
                    member __.Eval v tail teq =
                        v |> Teq.castFrom (Teq.Cong.domainOf teq) |> Choice1Of2
                }
        | Extended c ->
            c.Apply
                { new HUnionExtendedCrateEvaluator<_,_> with
                    member __.Eval union teq =
                        union |> Teq.castFrom (teq |> Teq.Cong.rangeOf |> cong) |> Choice2Of2
                }
