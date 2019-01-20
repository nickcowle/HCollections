namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'a HList

type 'a HListFolder =
    abstract member F : 'a -> 'b -> 'a

module HList =

    val cong : Teq<'a, 'b> -> Teq<'a HList, 'b HList>

    val empty : unit HList

    val cons : 'a -> 'b HList -> ('a -> 'b) HList

    val length : 'a HList -> int

    val head : ('a -> 'b) HList -> 'a

    val tail : ('a -> 'b) HList -> 'b HList

    val fold : 'a HListFolder -> 'a -> 'b HList -> 'a
