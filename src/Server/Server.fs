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

    let private addToRoom (user : NamedUser) room =
        let (RoomId rid) = room.Id
        let (UserId uid) = user.Id

        Room.tryGetPlayerByUserId user.Id room
        |> Option.map (fun user -> Error <| sprintf "%s (user %s) is already in room %s" user.Name (string uid) rid)
        |> Option.defaultValue (Ok { room with OtherPlayers = user :: room.OtherPlayers })

    let join roomId (user : NamedUser) =
        let (RoomId rid) = roomId

        storage.TryGetRoomById roomId
        |> Option.toResult (sprintf "No room with ID %s exists" rid)
        |> Result.bind (addToRoom user)
        |> Result.tee (storage.UpdateRoom roomId)

    let reconnect roomIdStr userIdStr =
        let roomId = RoomId roomIdStr

        let msgIfInvalidUserId = sprintf "%s is not a valid user ID." userIdStr
        let msgIfInvalidRoomId = let (RoomId rid) = roomId in sprintf "No room with ID %s exists." rid
        let msgIfUserNotInRoom userId = let (RoomId rid) = roomId in let (UserId uid) = userId in sprintf "User %s was never in room %s." (string uid) rid

        UserId.tryParse userIdStr
        |> Option.toResult msgIfInvalidUserId
        |> Result.bind (fun userId ->
            storage.TryGetRoomById roomId
            |> Option.toResult msgIfInvalidRoomId
            |> Result.bind (fun room ->
                Room.tryGetPlayerByUserId userId room
                |> Option.toResult (msgIfUserNotInRoom userId)
                |> Result.map (fun user -> (room, user))))

    let startGame rid =
        let startGameAndUpdateStorage room =
            Game.start room.Game
            |> Result.map (fun game -> { room with Game = game })
            |> Result.tee (fun room -> storage.UpdateRoom room.Id room)

        storage.TryGetRoomById rid
        |> Option.toResult (sprintf "Room %s does not exist." (RoomId.value rid))
        |> Result.bind startGameAndUpdateStorage

    let private validateResponse response =
        let validateField invalidMsg s = if String.IsNullOrWhiteSpace s then Some invalidMsg else None

        let error =
            { ResponseError.empty with
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

        if error = ResponseError.empty then Ok response else Error error

    let submitResponse rid uid response =
        let roomDoesNotExistError = sprintf "Room %s does not exist." (RoomId.value rid) |> ResponseError.general
        let userNotInRoomError =
            sprintf "User %s is not in room %s." (UserId.asString uid) (RoomId.value rid) |> ResponseError.general

        let updateResponseAndUpdateStorage room user response =
            Game.updateResponse room.Game (Room.players room) user response
            |> Result.map (fun g -> { room with Game = g })
            |> Result.mapError ResponseError.general
            |> Result.tee (fun r -> storage.UpdateRoom room.Id r)

        storage.TryGetRoomById rid
        |> Option.toResult roomDoesNotExistError
        |> Result.bind (fun room ->
            Room.tryGetPlayerByUserId uid room
            |> Option.toResult userNotInRoomError
            |> Result.bind (fun user ->
                validateResponse response
                |> Result.bind (updateResponseAndUpdateStorage room user)))

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
