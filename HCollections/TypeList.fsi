namespace HCollections

/// TypeList is a type-level list of types.
///
/// We represent the list of types using the single type parameter.
/// The empty list is represented with the unit type and
/// types are prepended to the list using the binary operator ->.
/// For example, the list of types [ int ; string ] is represented by:
///     (int -> string -> unit) TypeList
[<NoComparison>]
[<NoEquality>]
type 'ts TypeList

[<RequireQualifiedAccess>]
module TypeList =

    /// The unique empty TypeList
    val empty : unit TypeList

    /// Given an TypeList, prepends a new type
    /// to the list of types being represented.
    val cons<'t, 'ts> : 'ts TypeList -> ('t -> 'ts) TypeList
