module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

// TODO: periodically purge inactive rooms.
type Storage () =

    let rooms = ResizeArray<_>()

    member __.GetRooms () =
        List.ofSeq rooms

    member __.TryGetRoomById roomId =
        __.GetRooms ()
        |> List.tryFind (fun r -> r.Id = roomId)

    member __.AddRoom (room: Room) =
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
              Owner = owner}

        storage.AddRoom room
        room

    let validateIdString s =
        storage.TryGetRoomById (RoomId s)
        |> Option.map (fun r -> r.Id)

let consequencesApi =
    { createRoom = fun owner -> async { return Room.create owner }
      validateRoomId = fun s -> async { return Room.validateIdString s } }

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
