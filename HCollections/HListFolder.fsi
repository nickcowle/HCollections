namespace HCollections

[<RequireQualifiedAccess>]
module HListFolder =

    val makeGappedElementFolder : ('s -> 'a option -> 's) -> 's HListFolder

    val makeElementFolder : ('s -> 'a -> 's) -> 's HListFolder
