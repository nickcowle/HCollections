namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type HListT<'a, 'b>

type HListTFolder<'a, 'b> =
    abstract member F : 'a -> 'c -> 'b -> 'a

module HListT =

    val cong : Teq<'a, 'b> -> Teq<'c, 'd> -> Teq<HListT<'a, 'c>, HListT<'b, 'd>>

    val empty<'a> : HListT<unit, 'a>

    val cons : 'a -> 'b -> HListT<'c, 'b> -> HListT<'a -> 'c, 'b>

    val length : HListT<'a, 'b> -> int

    val head : HListT<'a -> 'b, 'c> -> 'a * 'c

    val tail : HListT<'a -> 'b, 'c> -> HListT<'b, 'c>

    val fold  : HListTFolder<'a, 'b> -> 'a -> HListT<'c, 'b> -> 'a
