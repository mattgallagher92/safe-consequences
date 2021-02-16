module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

type Storage () =

    let rooms = ResizeArray<_>()

    member __.GetRooms () =
        List.ofSeq rooms

    member __.TryGetRoomById roomId =
        __.GetRooms ()
        |> List.tryFind (fun r -> r.Id = roomId)

    member __.AddRoom (room: Room) =
        rooms.Add room

    member __.UpdateRoom roomId room =
        let i = rooms.FindIndex (fun r -> r.Id = roomId)
        rooms.RemoveAt i
        rooms.Add room

let storage = Storage()

module UserId =

    // On server because cannot be compiled by Fable.
    let tryParse (s : string) =
        let g = ref Guid.Empty
        if Guid.TryParse (s, g) then Some (UserId !g) else None

module Room =

    let private rnd = Random ()

    // Can't be in Shared.fs - implementation not supported by Fable.
    let private randomLetter () =
        rnd.Next (0, 26)
        |> (+) <| int 'a'
        // Not supported by Fable.
        |> Char.ConvertFromUtf32

    let private generateUniqueRoomId () =
        let generate () =
            let rl = randomLetter
            rl () + rl () + rl () + rl () |> RoomId

        let alreadyExists roomId =
            storage.TryGetRoomById roomId
            |> Option.map (fun _ -> true)
            |> Option.defaultValue false

        let mutable candidate = generate ()

        while alreadyExists candidate do
            candidate <- generate ()

        candidate

    let create owner =
        let room =
            { Id = generateUniqueRoomId ()
              Owner = owner
              OtherPlayers = []
              Game = Game.init () }

        storage.AddRoom room
        room

    let validateIdString s =
        storage.TryGetRoomById (RoomId s)
        |> Option.map (fun r -> r.Id)

    let private addToRoom room (user : NamedUser) =
        match Room.tryGetPlayerByUserId user.Id room with
        | Some _ ->
            let (RoomId rid) = room.Id
            let (UserId uid) = user.Id
            sprintf "%s (user %s) is already in room %s"
                user.Name (string uid) rid
            |> Error
        | None ->
            Ok { room with OtherPlayers = user :: room.OtherPlayers }

    let join roomId (user : NamedUser) =
        match storage.TryGetRoomById roomId with
        | None ->
            let (RoomId rid) = roomId
            Error <| sprintf "No room with ID %s exists" rid
        | Some room ->
            match addToRoom room user with
            | Error msg ->
                Error msg
            | Ok newRoom ->
                storage.UpdateRoom roomId newRoom
                Ok newRoom

    let reconnect roomIdStr userIdStr =
        let roomId = RoomId roomIdStr

        match UserId.tryParse userIdStr with
        | Some userId ->
            match storage.TryGetRoomById roomId with
            | None ->
                let (RoomId rid) = roomId
                Error <| sprintf "No room with ID %s exists." rid
            | Some room ->
                match Room.tryGetPlayerByUserId userId room with
                | None ->
                    let (RoomId rid) = room.Id
                    let (UserId uid) = userId
                    Error <| sprintf "User %s was never in room %s." (string uid) rid
                | Some user ->
                    Ok (room, user)
        | None ->
            Error <| sprintf "%s is not a valid user ID." userIdStr

    let startGame rid =
        match storage.TryGetRoomById rid with
        | None ->
            Error <| sprintf "Room %s does not exist." (RoomId.value rid)
        | Some room ->
            match Game.start room.Game with
            | Error msg ->
                Error msg
            | Ok game ->
                let newRoom = { room with Game = game }

                storage.UpdateRoom room.Id newRoom

                Ok newRoom

    let submitResponse rid uid response =
        match storage.TryGetRoomById rid with
        | None ->
            let errorMsg = sprintf "Room %s does not exist." (RoomId.value rid)
            Error <| ResponseError.general errorMsg
        | Some room ->
            match Room.tryGetPlayerByUserId uid room with
            | None ->
                let errorMsg = sprintf "User %s is not in room %s." (UserId.asString uid) (RoomId.value rid)
                Error <| ResponseError.general errorMsg
            | Some u ->
                let validate invalidMsg s = if String.IsNullOrWhiteSpace s then Some invalidMsg else None
                let responseError =
                    { ResponseError.empty with
                          HisDescriptionErrorOpt = validate "Enter a description" response.HisDescription
                          HisNameErrorOpt = validate "Enter a name" response.HisName
                          HerDescriptionErrorOpt = validate "Enter a description" response.HerDescription
                          HerNameErrorOpt = validate "Enter a name" response.HerName
                          WhereTheyMetErrorOpt = validate "Enter a place" response.WhereTheyMet
                          WhatHeGaveHerErrorOpt = validate "Enter an object" response.WhatHeGaveHer
                          WhatHeSaidToHerErrorOpt = validate "Enter a phrase or sentence" response.WhatHeSaidToHer
                          WhatSheSaidToHimErrorOpt = validate "Enter a phrase or sentence" response.WhatSheSaidToHim
                          TheConsequenceErrorOpt = validate "Enter a consequence" response.TheConsequence
                          WhatTheWorldSaidErrorOpt = validate "Enter a phrase or sentence" response.WhatTheWorldSaid
                          GeneralErrorOpt = None }

                // TODO: simplify other functions using tee.
                let tee f result = (match result with Ok x -> f x | Error _ -> ()); result

                if responseError <> ResponseError.empty then Error responseError else
                    Game.updateResponse room.Game (Room.players room) u response
                    |> tee (fun g -> storage.UpdateRoom room.Id { room with Game = g })
                    |> Result.mapError ResponseError.general
                    |> Result.map (fun g -> { room with Game = g })

let consequencesApi =
    { createRoom = fun owner -> async { return Room.create owner }
      validateRoomId = fun s -> async { return Room.validateIdString s }
      joinRoom = fun (roomId, user) -> async { return Room.join roomId user }
      reconnect = fun (roomIdStr, userIdStr) -> async { return Room.reconnect roomIdStr userIdStr }
      startGame = fun rid -> async { return Room.startGame rid }
      submitResponse = fun (rid, uid, response) -> async { return Room.submitResponse rid uid response } }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue consequencesApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
