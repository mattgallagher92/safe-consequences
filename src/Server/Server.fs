module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

type Storage () =
    let todos = ResizeArray<_>()

    member __.GetTodos () =
        List.ofSeq todos

    member __.AddTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok ()
        else Error "Invalid todo"

let storage = Storage()

module Room =

    let private rnd = Random ()

    let private randomLetter () =
        rnd.Next (0, 26)
        |> (+) <| int 'a'
        |> Char.ConvertFromUtf32

    let create owner =
        let rl = randomLetter
        { Id = rl () + rl () + rl () + rl () |> RoomId
          Owner = owner }

let consequencesApi =
    { createRoom = fun owner -> async { return Room.create owner } }

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
