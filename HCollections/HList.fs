namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'a HList =
    private
    | Empty of Teq<'a, unit>
    | Cons of 'a HListConsCrate

and private 'a HListConsCrate = abstract member Apply : HListConsEvaluator<'a, 'ret> -> 'ret
and private HListConsEvaluator<'a, 'ret> = abstract member Eval : 'b -> 'c HList -> Teq<'a, 'b -> 'c> -> 'ret

type 'a HListFolder =
    abstract member F : 'a -> 'b -> 'a

module HList =

    let cong (teq : Teq<'a, 'b>) : Teq<'a HList, 'b HList> =
        Teq.Cong.believeMe teq

    let empty = HList.Empty Teq.refl

    let cons (x : 'a) (xs : 'b HList) =
        HList.Cons
            { new HListConsCrate<'a -> 'b> with
                member __.Apply e = e.Eval x xs Teq.refl<'a -> 'b>
            }

    let rec length<'a> (xs : 'a HList) : int =
        match xs with
        | Empty _ -> 0
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval _ xs _ = length xs + 1
                }

    let head<'a, 'b> (xs : ('a -> 'b) HList) : 'a =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval x _ teq =
                        let teq = teq |> Teq.Cong.domainOf
                        x |> Teq.castFrom teq
                }

    let tail<'a, 'b> (xs : ('a -> 'b) HList) : 'b HList =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval _ xs teq =
                        let teq = teq |> Teq.Cong.rangeOf |> cong
                        xs |> Teq.castFrom teq
                }

    let rec fold<'a, 'b> (folder : 'a HListFolder) (s : 'a) (xs : 'b HList) : 'a =
        match xs with
        | Empty _ -> s
        | Cons c ->
            c.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval x xs teq = fold folder (folder.F s x) xs
                }
