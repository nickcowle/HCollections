namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type HListT<'a, 'b> =
    private
    | Empty of Teq<'a, unit>
    | Cons of HListTConsCrate<'a, 'b>

and private HListTConsCrate<'a, 'b> = abstract member Apply : HListTConsEvaluator<'a, 'b, 'ret> -> 'ret
and private HListTConsEvaluator<'a, 'b, 'ret> = abstract member Eval : 'c -> 'b -> HListT<'d, 'b> -> Teq<'a, 'c -> 'd> -> 'ret

type HListTFolder<'a, 'b> =
    abstract member F : 'a -> 'c -> 'b -> 'a

module HListT =

    let cong (teq1 : Teq<'a, 'b>) (teq2 : Teq<'c, 'd>) : Teq<HListT<'a, 'c>, HListT<'b, 'd>> =
        Teq.Cong.believeMe teq1

    let empty<'a> : HListT<unit, 'a> = HListT.Empty Teq.refl

    let cons (x : 'a) (y : 'b) (xs : HListT<'c, 'b>) =
        HListT.Cons
            { new HListTConsCrate<_, _> with
                member __.Apply e = e.Eval x y xs Teq.refl
            }

    let rec length<'a, 'b> (xs : HListT<'a, 'b>) : int =
        match xs with
        | Empty _ -> 0
        | Cons b ->
            b.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval _ _ xs _ = length xs + 1
                }

    let head (xs : HListT<'a -> 'b, 'c>) : 'a * 'c =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval x y _ teq =
                        let teq = teq |> Teq.Cong.domainOf
                        x |> Teq.castFrom teq, y
                }

    let tail (xs : HListT<'a -> 'b, 'c>) : HListT<'b, 'c> =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval _ _ xs teq =
                        let teq = cong (teq |> Teq.Cong.rangeOf) Teq.refl
                        xs |> Teq.castFrom teq
                }

    let rec fold<'a, 'b, 'c> (folder : HListTFolder<'a, 'b>) (s : 'a) (xs : HListT<'c, 'b>) : 'a =
        match xs with
        | Empty _ -> s
        | Cons c ->
            c.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval x y xs teq = fold folder (folder.F s x y) xs
                }
