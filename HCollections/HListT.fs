namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type HListT<'ts, 'elem> =
    private
    | Empty of Teq<'ts, unit>
    | Cons of HListTConsCrate<'ts, 'elem> * 'ts TypeList

and private HListTConsEvaluator<'ts, 'elem, 'ret> =
    abstract member Eval<'t, 'ts2> : 't -> 'elem -> HListT<'ts2, 'elem> -> Teq<'ts, 't -> 'ts2> -> 'ret

and private HListTConsCrate<'ts, 'elem> =
    abstract member Apply<'ret> : HListTConsEvaluator<'ts, 'elem, 'ret> -> 'ret

type HListTFolder<'state, 'elem> =
    abstract member Folder<'a> : 'state -> 'a -> 'elem -> 'state

module HListT =

    let cong (teq1 : Teq<'ts1, 'ts2>) (_ : Teq<'elem1, 'elem2>) : Teq<HListT<'ts1, 'elem1>, HListT<'ts2, 'elem2>> =
        Teq.Cong.believeMe teq1

    let toTypeList<'ts, 'elem> (list : HListT<'ts, 'elem>) : 'ts TypeList =
        match list with
        | Empty teq ->
            TypeList.empty
            |> Teq.castFrom (TypeList.cong teq)
        | Cons (_, tl) -> tl

    let empty<'elem> : HListT<unit, 'elem> = HListT.Empty Teq.refl

    let length<'ts, 'elem> (xs : HListT<'ts, 'elem>) : int =
        match xs with
        | Empty _ -> 0
        | Cons (_, tl) -> TypeList.length tl

    let cons<'t, 'ts, 'elem> (x : 't) (elem : 'elem) (xs : HListT<'ts, 'elem>) =
        let crate =
            { new HListTConsCrate<_, _> with
                member __.Apply e = e.Eval x elem xs Teq.refl
            }
        let tl =
            xs
            |> toTypeList
            |> TypeList.cons<'t, _>

        HListT.Cons (crate, tl)

    let head (xs : HListT<'t -> 'ts, 'elem>) : 't * 'elem =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons (b, _length) ->
            b.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval x y _ teq =
                        let teq = teq |> Teq.Cong.domainOf
                        x |> Teq.castFrom teq, y
                }

    let tail (xs : HListT<'t -> 'ts, 'elem>) : HListT<'ts, 'elem> =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons (b, _length) ->
            b.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval _ _ xs teq =
                        let teq = cong (teq |> Teq.Cong.rangeOf) Teq.refl
                        xs |> Teq.castFrom teq
                }

    let rec private toHList'<'ts, 'elem, 'k>
        (input : HListT<'ts, 'elem>)
        (cont : 'ts HList -> 'k)
        : 'k
        =
        match input with
        | HListT.Empty t -> HList.empty |> Teq.castFrom (HList.cong t) |> cont
        | HListT.Cons (c,_) ->
            c.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval<'t, 't2> (t : 't) _ (cons : HListT<'t2, 'elem>) teq =
                        toHList'<'t2, 'elem, 'k>
                            cons
                            (fun ts ->
                                HList.cons t ts
                                |> Teq.castFrom (HList.cong teq)
                                |> cont)
                }

    let rec toHList<'ts, 'elem> (input : HListT<'ts, 'elem>) : 'ts HList =
        toHList'<'ts, 'elem, 'ts HList> input id

    let rec private toList'<'ts, 'elem, 'k>
        (current : HListT<'ts, 'elem>)
        (cont : 'elem list -> 'k)
        : 'k
        =
        match current with
        | HListT.Empty _ -> cont []
        | HListT.Cons (c, _) ->
            c.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval<'t, 't2> (_ : 't) v (cons : HListT<'t2, 'elem>) teq =
                        toList'<'t2, 'elem, 'k> cons (fun vs -> v :: vs |> cont)
                }

    let toList<'ts, 'elem> (input : HListT<'ts, 'elem>) : 'elem list =
        toList'<'ts, 'elem, 'elem list> input id

    let rec fold<'state, 'ts, 'elem> (folder : HListTFolder<'state, 'elem>) (seed : 'state) (xs : HListT<'ts, 'elem>) : 'state =
        match xs with
        | Empty _ -> seed
        | Cons (c, _length) ->
            c.Apply
                { new HListTConsEvaluator<_,_,_> with
                    member __.Eval x y xs _ = fold folder (folder.Folder seed x y) xs
                }
