namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'ts HList =
    private
    | Empty of Teq<'ts, unit>
    | Cons of 'ts HListCons * 'ts TypeList

and 'ts HListCons =
    abstract Apply<'ret> : HListConsEvaluator<'ts, 'ret> -> 'ret

and HListConsEvaluator<'ts, 'ret> =
    abstract Eval<'t, 'ts2> : 't * 'ts2 HList * Teq<'ts, 't -> 'ts2> -> 'ret

type 'state HListFolder =
    abstract Folder<'a> : 'state -> 'a -> 'state

module HList =

    let toTypeList<'ts> (xs : 'ts HList) : 'ts TypeList =
        match xs with
        | Empty teq ->
            TypeList.empty |> Teq.castFrom (teq |> TypeList.cong)
        | Cons (_b, tl) -> tl

    let cong (teq : Teq<'ts1, 'ts2>) : Teq<'ts1 HList, 'ts2 HList> =
        Teq.Cong.believeMe teq

    let empty = HList.Empty Teq.refl

    let length<'ts> (xs : 'ts HList) : int =
        match xs with
        | Empty _ -> 0
        | Cons (_b, tl) -> TypeList.length tl

    let cons (x : 't) (xs : 'ts HList) =
        let crate = 
            { new HListCons<_> with
                member __.Apply e = e.Eval (x, xs, Teq.refl)
            }
        let tl = TypeList.cons<'t, 'ts> (toTypeList xs)

        HList.Cons (crate, tl)

    let head (xs : ('t -> 'ts) HList) : 't =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons (b, _length) ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval (x, _, teq) =
                        let teq = teq |> Teq.Cong.domainOf
                        x |> Teq.castFrom teq
                }

    let tail (xs : ('t -> 'ts) HList) : 'ts HList =
        match xs with
        | Empty _ -> raise Unreachable
        | Cons (b, _length) ->
            b.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval (_, xs, teq) =
                        let teq = teq |> Teq.Cong.rangeOf |> cong
                        xs |> Teq.castFrom teq
                }

    let rec fold<'state, 'ts> (folder : 'state HListFolder) (seed : 'state) (xs : 'ts HList) : 'state =
        match xs with
        | Empty _ -> seed
        | Cons (c, _length) ->
            c.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval (x, xs, teq) = fold folder (folder.Folder seed x) xs
                }

    let split (v : 'ts HList) : Choice<Teq<'ts, unit>, 'ts HListCons> =
        match v with
        | Empty ts -> Choice<_,_>.Choice1Of2 ts
        | Cons (c,_) ->
            c.Apply
                { new HListConsEvaluator<_,_> with
                    member __.Eval<'t, 'u> (head, tail, (ts : Teq<'ts, 't -> 'u>)) =
                        { new HListCons<_> with member __.Apply e = e.Eval (head, tail, ts) }
                        |> Choice<_,_>.Choice2Of2
                }