namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'ts HList =
    private
    | Empty of Teq<'ts, unit>
    | Cons of 'ts HListConsCrate

and private HListConsEvaluator<'ts, 'ret> =
    abstract member Eval<'t, 'ts2> : 't -> 'ts2 HList -> Teq<'ts, 't -> 'ts2> -> 'ret

and private 'ts HListConsCrate =
    abstract member Apply<'ret> : HListConsEvaluator<'ts, 'ret> -> 'ret

type 'state HListFolder =
    abstract member Folder<'a> : 'state -> 'a -> 'state

module HList =

    let cong (teq : Teq<'ts1, 'ts2>) : Teq<'ts1 HList, 'ts2 HList> =
        Teq.Cong.believeMe teq

    let empty = HList.Empty Teq.refl

    let cons (x : 't) (xs : 'ts HList) =
        HList.Cons
            { new HListConsCrate<_> with
                member __.Apply e = e.Eval x xs Teq.refl
            }

    let rec length<'ts> (xs : 'ts HList) : int =
        match xs with
        | Empty _ -> 0
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval _ xs _ = length xs + 1
                }

    let head (xs : ('t -> 'ts) HList) : 't =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval x _ teq =
                        let teq = teq |> Teq.Cong.domainOf
                        x |> Teq.castFrom teq
                }

    let tail (xs : ('t -> 'ts) HList) : 'ts HList =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons b ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval _ xs teq =
                        let teq = teq |> Teq.Cong.rangeOf |> cong
                        xs |> Teq.castFrom teq
                }

    let rec fold<'state, 'ts> (folder : 'state HListFolder) (seed : 'state) (xs : 'ts HList) : 'state =
        match xs with
        | Empty _ -> seed
        | Cons c ->
            c.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval x xs teq = fold folder (folder.Folder seed x) xs
                }
