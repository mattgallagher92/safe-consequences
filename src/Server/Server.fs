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

    member __.GetRoomById roomId =
        __.TryGetRoomById roomId
        |> Option.toResult (sprintf "Room %s does not exist." (RoomId.value roomId))

    member __.AddRoom (room: Room) =
        rooms.Add room

    member __.UpdateRoom roomId room =
        let i = rooms.FindIndex (fun r -> r.Id = roomId)
        rooms.RemoveAt i
        rooms.Add room

let storage = Storage()

module UserId =

    // On server because cannot be compiled by Fable.
    let parse (s : string) =
        let g = ref Guid.Empty
        if Guid.TryParse (s, g) then Ok (UserId !g) else Error <| sprintf "%s is not a valid user ID." s

module Room =

    let private result = ResultBuilder()

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
        Room.create (generateUniqueRoomId ()) owner
        |> Utils.tee storage.AddRoom

    let validateIdString s =
        storage.TryGetRoomById (RoomId s)
        |> Option.map (fun r -> r.Id)

    let join roomId (user : NamedUser) =
        storage.GetRoomById roomId
        |> Result.bind (Room.add user)
        |> Result.tee (storage.UpdateRoom roomId)

    let reconnect roomIdStr userIdStr =
        let roomId = RoomId roomIdStr

        result {
            let! userId = UserId.parse userIdStr
            let! room = storage.GetRoomById roomId
            let! user = Room.getPlayerByUserId userId room

            return (room, user)
        }

    let startGame rid =
        storage.GetRoomById rid
        |> Result.bind Room.startGame
        |> Result.tee (fun room -> storage.UpdateRoom room.Id room)

    let submitResponse rid uid response =
        result {
            let! room = storage.GetRoomById rid |> Result.mapError Response.Error.general
            let! user = Room.getPlayerByUserId uid room |> Result.mapError Response.Error.general

            let! newRoom = Room.updateResponse room user response

            storage.UpdateRoom room.Id newRoom

            return newRoom
        }

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
