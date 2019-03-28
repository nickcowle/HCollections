namespace HCollections.Test

open HCollections
open Xunit

module TestHUnion =

    let testUnion = HUnion.make HUnionTail.empty 1234

    [<Fact>]
    let ``Splitting an HUnion that hasn't been extended returns the value of the union`` () =

        match testUnion |> HUnion.split with
        | Choice1Of2 j -> Assert.Equal(1234, j)
        | Choice2Of2 _ -> Assert.True false

    [<Fact>]
    let ``Splitting an HUnion that has been extended returns the inner union`` () =

        let union =
            testUnion
            |> HUnion.extend<_, string>

        match union |> HUnion.split with
        | Choice1Of2 _ -> Assert.True false
        | Choice2Of2 _ -> Assert.True true

    [<Fact>]
    let ``getSingleton returns the correct value`` () =

        let actual = testUnion |> HUnion.getSingleton
        Assert.Equal(1234, actual)
