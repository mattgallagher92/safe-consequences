namespace Shared

open System

type UserId = UserId of Guid

module UserId =

    let value (UserId guid) = guid

type NamedUser =
    { Id: UserId
      Name: string }

type User =
    | Anonymous of UserId
    | Named of NamedUser

module User =

    let create () = Guid.NewGuid () |> UserId |> Anonymous

    let userId = function | Anonymous uid -> uid | Named user -> user.Id

    let assignName name user =
        { Id = userId user
          Name = name }

    let unassignName user = Anonymous <| userId user

    let equal u1 u2 = userId u1 = userId u2

type RoomId = RoomId of string

module RoomId =

    let value (RoomId id) = id

type Room =
    { Id: RoomId
      Owner: NamedUser
      OtherPlayers: NamedUser list }

module Room =

    // Use List.rev so that this list is shown in order players joined.
    let players room = room.Owner :: List.rev room.OtherPlayers

    let tryGetPlayerByUserId userId room =
        room
        |> players
        |> List.tryFind (fun (u : NamedUser) -> u.Id = userId)

type Response =
    { HisDescription: string
      HisName: string
      HerDescription: string
      HerName: string
      WhereTheyMet: string
      WhatHeGaveHer: string
      WhatHeSaidToHer: string
      WhatSheSaidToHim: string
      TheConsequence: string
      WhatTheWorldSaid: string }

module Response =

    let empty =
        { HisDescription = ""
          HisName = ""
          HerDescription = ""
          HerName = ""
          WhereTheyMet = ""
          WhatHeGaveHer = ""
          WhatHeSaidToHer = ""
          WhatSheSaidToHim = ""
          TheConsequence = ""
          WhatTheWorldSaid = "" }

type Responses = Map<NamedUser, Response>

type GameState = Room * Responses

type Game =
    | WaitingForResponses of GameState

module Game =

    let init room = WaitingForResponses (room, Map.empty)

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IConsequencesApi =
    { createRoom: NamedUser -> Async<Room>
      validateRoomId: string -> Async<RoomId option>
      joinRoom: RoomId * NamedUser -> Async<Result<Room, string>>
      reconnect: string * string -> Async<Result<Room * NamedUser, string>>
      startGame: RoomId -> Async<Result<Game, string>> }
