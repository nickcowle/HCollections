namespace HCollections

[<RequireQualifiedAccess>]
module HListFolder =

    let makeGappedElementFolder (f : 's -> 'a option -> 's) : 's HListFolder =
        { new HListFolder<_> with
            member __.F s (x : 'b) =
                let v =
                    if typeof<'b> = typeof<'a> then
                        x |> unbox |> Some
                    else
                        None
                f s v
        }

    let makeElementFolder (f : 's -> 'a -> 's) : 's HListFolder =
        let g s o = match o with Some a -> f s a | None -> s
        makeGappedElementFolder g
