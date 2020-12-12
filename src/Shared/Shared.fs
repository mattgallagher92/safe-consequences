namespace Shared

open System

type UserId = UserId of Guid

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

    let unassignName user =
        Anonymous <| userId user

type RoomId = RoomId of string

module RoomId =

    let value (RoomId id) = id

type Room =
    { Id: RoomId
      Owner: NamedUser }

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
    { createRoom: NamedUser -> Async<Room> }
