module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared

let player1 = User.create () |> User.assignName "1"
let player2 = User.create () |> User.assignName "2"
let player3 = User.create () |> User.assignName "3"

let response1 =
    { HisDescription = "hisDescription1"
      HisName = "hisName1"
      HerDescription = "herDescription1"
      HerName = "herName1"
      WhereTheyMet = "where1"
      WhatHeGaveHer = "object1"
      WhatHeSaidToHer = "heSaid1"
      WhatSheSaidToHim = "sheSaid1"
      TheConsequence = "consequence1"
      WhatTheWorldSaid = "worldSaid1" }
let response2 =
    { HisDescription = "hisDescription2"
      HisName = "hisName2"
      HerDescription = "herDescription2"
      HerName = "herName2"
      WhereTheyMet = "where2"
      WhatHeGaveHer = "object2"
      WhatHeSaidToHer = "heSaid2"
      WhatSheSaidToHim = "sheSaid2"
      TheConsequence = "consequence2"
      WhatTheWorldSaid = "worldSaid2" }
let response3 =
    { HisDescription = "hisDescription3"
      HisName = "hisName3"
      HerDescription = "herDescription3"
      HerName = "herName3"
      WhereTheyMet = "where3"
      WhatHeGaveHer = "object3"
      WhatHeSaidToHer = "heSaid3"
      WhatSheSaidToHim = "sheSaid3"
      TheConsequence = "consequence3"
      WhatTheWorldSaid = "worldSaid3" }

let shared = testList "Shared" [
    testCase "Game moves through states correctly" <| fun _ ->
        let game =
            Game.init ()
            |> Game.start
            // Use different order to join order to test that there's no reliance on same order.
            |> Result.bind (fun g -> Game.updateResponse g [ player1; player2; player3 ] player2 response2)
            |> Result.bind (fun g -> Game.updateResponse g [ player1; player2; player3 ] player3 response3)
            |> function | Ok r -> r | Error e -> failwithf "Bug creating game in test code: %A" e
        let expectedBefore =
            Map.empty
            |> Map.add player2 response2
            |> Map.add player3 response3
            |> WaitingForResponses
        Expect.equal game expectedBefore "Game should be in WaitingForResponses state before last response received."

        let actual = Game.updateResponse game [ player1; player2; player3 ] player1 response1

        let expectedAfter =
            Map.empty
            |> Map.add player1 response1
            |> Map.add player2 response2
            |> Map.add player3 response3
            |> AllResponsesReceived
            |> Ok
        Expect.equal actual expectedAfter "Game should be in AllResponsesReceived state after last response received."

    testCase "Responses are mixed correctly" <| fun _ ->
        let room =
            Room.create (RoomId "room") player1
            |> Room.add player2
            |> Result.bind (Room.add player3)
            |> Result.bind Room.startGame
            |> Result.mapError Response.Error.general
            |> Result.bind (fun r -> Room.updateResponse r player1 response1)
            |> Result.bind (fun r -> Room.updateResponse r player2 response2)
            |> Result.bind (fun r -> Room.updateResponse r player3 response3)
            |> function | Ok r -> r | Error e -> failwithf "Bug creating room in test code: %A" e

        let actual = Room.storyFor room player1

        let expectedFor1 =
            {
                HisDescription = "hisDescription3"
                HisName = "hisName1"
                HerDescription = "herDescription2"
                HerName = "herName3"
                WhereTheyMet = "where1"
                WhatHeGaveHer = "object2"
                WhatHeSaidToHer = "heSaid3"
                WhatSheSaidToHim = "sheSaid1"
                TheConsequence = "consequence2"
                WhatTheWorldSaid = "worldSaid3"
            }
            |> Ok
        Expect.equal actual expectedFor1 "Responses should be mixed up correctly."
]
