namespace HCollections

open TypeEquality

[<NoComparison>]
[<NoEquality>]
type 'a HUnionTail

[<RequireQualifiedAccess>]
module HUnionTail =

    val empty : unit HUnionTail

    val extend<'a, 'b> : 'a HUnionTail -> ('b -> 'a) HUnionTail


[<NoComparison>]
[<NoEquality>]
type 'a HUnion

[<RequireQualifiedAccess>]
module HUnion =

    val cong : Teq<'a, 'b> -> Teq<'a HUnion, 'b HUnion>

    val make : 'a HUnionTail -> 'b -> ('b -> 'a) HUnion

    val extend : 'a HUnion -> ('b -> 'a) HUnion

    val split : ('a -> 'b) HUnion -> Choice<'a, 'b HUnion>
