namespace Nessos.Vagabond.Tests

open NUnit.Framework

[<AutoOpen>]
module Utils =
    let shouldfail (f : unit -> 'T) =
        try let v = f () in raise <| new AssertionException(sprintf "should fail but was '%A'" v)
        with _ -> ()

    let shouldFailwith<'T, 'Exn when 'Exn :> exn> (f : unit -> 'T) =
        try let v = f () in raise <| new AssertionException(sprintf "should fail but was '%A'" v)
        with :? 'Exn -> ()

    /// type safe equality tester
    let shouldEqual (expected : 'T) (input : 'T) = 
        if expected = input then ()
        else
            raise <| new AssertionException(sprintf "expected '%A' but was '%A'." expected input)

    /// type safe equality tester
    let shouldNotEqual (expected : 'T) (input : 'T) = 
        if expected <> input then ()
        else
            raise <| new AssertionException(sprintf "values were equal with '%A'." expected)

    let shouldBe (pred : 'T -> bool) (input : 'T) =
        if pred input then ()
        else
            raise <| new AssertionException(sprintf "value '%A' does not match predicate." input)