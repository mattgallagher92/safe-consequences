namespace Shared

open System

module Utils =

    let tee f x = f x; x

module Option =

    let toResult errorIfNone opt =
        match opt with Some x -> Ok x | None -> Error errorIfNone

module Result =

    let bind f result = match result with Ok x -> f x | Error e -> Error e

    let tee f result =
        match result with Ok x -> f x | Error _ -> ()
        result

type ResultBuilder() =
    member __.Bind(x, f) = Result.bind f x
    member __.Return x = Ok x
    member __.ReturnFrom x = x

type UserId = UserId of Guid

module UserId =

    let value (UserId guid) = guid

    let asString = value >> string

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

    type Error =
        { HisDescriptionErrorOpt: string option
          HisNameErrorOpt: string option
          HerDescriptionErrorOpt: string option
          HerNameErrorOpt: string option
          WhereTheyMetErrorOpt: string option
          WhatHeGaveHerErrorOpt: string option
          WhatHeSaidToHerErrorOpt: string option
          WhatSheSaidToHimErrorOpt: string option
          TheConsequenceErrorOpt: string option
          WhatTheWorldSaidErrorOpt: string option
          GeneralErrorOpt: string option }

    module Error =

        let empty =
            { HisDescriptionErrorOpt = None
              HisNameErrorOpt = None
              HerDescriptionErrorOpt = None
              HerNameErrorOpt = None
              WhereTheyMetErrorOpt = None
              WhatHeGaveHerErrorOpt = None
              WhatHeSaidToHerErrorOpt = None
              WhatSheSaidToHimErrorOpt = None
              TheConsequenceErrorOpt = None
              WhatTheWorldSaidErrorOpt = None
              GeneralErrorOpt = None }

        let general errorMsg  = { empty with GeneralErrorOpt = Some errorMsg }

    let validate response =
        let validateField invalidMsg s = if String.IsNullOrWhiteSpace s then Some invalidMsg else None

        let error =
            { Error.empty with
                  HisDescriptionErrorOpt = validateField "Enter a description" response.HisDescription
                  HisNameErrorOpt = validateField "Enter a name" response.HisName
                  HerDescriptionErrorOpt = validateField "Enter a description" response.HerDescription
                  HerNameErrorOpt = validateField "Enter a name" response.HerName
                  WhereTheyMetErrorOpt = validateField "Enter a place" response.WhereTheyMet
                  WhatHeGaveHerErrorOpt = validateField "Enter an object" response.WhatHeGaveHer
                  WhatHeSaidToHerErrorOpt = validateField "Enter a phrase or sentence" response.WhatHeSaidToHer
                  WhatSheSaidToHimErrorOpt = validateField "Enter a phrase or sentence" response.WhatSheSaidToHim
                  TheConsequenceErrorOpt = validateField "Enter a consequence" response.TheConsequence
                  WhatTheWorldSaidErrorOpt = validateField "Enter a phrase or sentence" response.WhatTheWorldSaid
                  GeneralErrorOpt = None }

        if error = Error.empty then Ok response else Error error

type Responses = Map<NamedUser, Response>

type Game =
    | NotStarted
    | WaitingForResponses of Responses
    | AllResponsesReceived of Responses

module Game =

    let init () = NotStarted

    let start g =
        match g with
        | NotStarted -> Ok <| WaitingForResponses Map.empty
        | _ -> Error <| sprintf "The game has already started."

    let updateResponse game allPlayers user response =
        match game with
        | WaitingForResponses responses ->
            let newResponses = Map.add user response responses
            let haveResponsesFromAllPlayers = allPlayers = (Map.toList responses |> List.map fst)
            if haveResponsesFromAllPlayers then AllResponsesReceived newResponses else WaitingForResponses newResponses
            |> Ok
        | _ ->
            Error <| sprintf "The game is no longer accepting responses."

type RoomId = RoomId of string

module RoomId =

    let value (RoomId id) = id

type Room =
    { Id: RoomId
      Owner: NamedUser
      OtherPlayers: NamedUser list
      Game: Game }

module Room =

    let create roomId owner =
        { Id = roomId
          Owner = owner
          OtherPlayers = []
          Game = Game.init () }

    // Use List.rev so that this list is shown in order players joined.
    let players room = room.Owner :: List.rev room.OtherPlayers

    let tryGetPlayerByUserId userId room =
        room
        |> players
        |> List.tryFind (fun (u : NamedUser) -> u.Id = userId)

    let getPlayerByUserId userId room =
        tryGetPlayerByUserId userId room
        |> Option.toResult (sprintf "User %s is not in room %s." (UserId.asString userId) (RoomId.value room.Id))

    let add (user : NamedUser) room =
        let (RoomId rid) = room.Id
        let (UserId uid) = user.Id

        tryGetPlayerByUserId user.Id room
        |> Option.map (fun user -> Error <| sprintf "%s (user %s) is already in room %s" user.Name (string uid) rid)
        |> Option.defaultValue (Ok { room with OtherPlayers = user :: room.OtherPlayers })

    let startGame room =
        Game.start room.Game
        |> Result.map (fun game -> { room with Game = game })

    let updateResponse room user response =
        let update =
            Game.updateResponse room.Game (players room) user
            >> Result.mapError Response.Error.general

        Response.validate response
        |> Result.bind update
        |> Result.map (fun g -> { room with Game = g })

    let playersWhoHaveSubmittedResponses room =
        match room.Game with
        | NotStarted -> []
        | WaitingForResponses rs | AllResponsesReceived rs -> Map.toList rs |> List.map fst

    let playersWhoHaveNotSubmittedResponses room =
        let all = Set.ofList <| players room
        let haveSubmitted = Set.ofList <| playersWhoHaveSubmittedResponses room

        Set.difference all haveSubmitted
        |> List.ofSeq

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IConsequencesApi =
    { createRoom: NamedUser -> Async<Room>
      validateRoomId: string -> Async<RoomId option>
      joinRoom: RoomId * NamedUser -> Async<Result<Room, string>>
      reconnect: string * string -> Async<Result<Room * NamedUser, string>>
      startGame: RoomId -> Async<Result<Room, string>>
      submitResponse: RoomId * UserId * Response -> Async<Result<Room, Response.Error>> }
