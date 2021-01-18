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
              OtherPlayers = [] }

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

let consequencesApi =
    { createRoom = fun owner -> async { return Room.create owner }
      validateRoomId = fun s -> async { return Room.validateIdString s }
      joinRoom = fun (roomId, user) -> async { return Room.join roomId user } }

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
