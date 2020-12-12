namespace Shared

open System

type UserId = UserId of Guid

type User =
    { Id: UserId }

module User =

    let create () =
        { Id = UserId <| Guid.NewGuid () }

type RoomId = RoomId of string

type Room =
    { Id: RoomId
      Owner: User }

type Todo =
    { Id : Guid
      Description : string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IConsequencesApi =
    { createRoom: User -> Async<Room> }
