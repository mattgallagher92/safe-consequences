module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Msg =
    | StartCreatingRoom
    | SetNameInput of name:string
    | SubmitName of name:string * followUpAction:(NamedUser -> Msg)
    | CreateRoom of user:NamedUser
    | RoomCreated of room:Room
    | StartJoiningRoom
    | SetRoomIdInput of roomId:string
    | SubmitRoomId of roomId:string
    | HandleRoomIdValidation of roomIdOpt:RoomId option
    | JoinRoom of roomId:RoomId * user:NamedUser
    | HandleJoinRoomResult of Result<Room, string>
    | Reconnect of roomIdStr:string * userIdStr:string
    | HandleReconnectResult of Result<Room * NamedUser, string>
    | StartGame of roomId:RoomId
    | HandleStartGameResult of Result<Room, string>

type Page =
    | LandingPage
    | UsernamePage of submitAction: (NamedUser -> Msg)
    | Lobby of Room
    | RoomIdPage

type LobbyQuery =
    { RoomIdStr: string
      UserIdStr: string }

type Route =
    | Query of LobbyQuery
    | Other

    static member FromParams roomIdStrOpt userIdStrOpt =
        match roomIdStrOpt, userIdStrOpt with
        | Some r, Some u -> Query { RoomIdStr = r; UserIdStr = u }
        | _ -> Other

type Model =
    { User: User
      ActivePage: Page
      Route: Route
      NameInput: string
      NameInputErrorOpt: string option
      RoomIdInput: string
      RoomIdInputErrorOpt: string option
      ReconnectErrorOpt: string option
      StartGameErrorOpt: string option }

let consequencesApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IConsequencesApi>

let cmdFor =
    function
    | Some (Query data) -> Cmd.ofMsg <| Reconnect (data.RoomIdStr, data.UserIdStr)
    | Some _ -> Cmd.none
    | None -> Navigation.Navigation.modifyUrl "#"

let init initialRouteOpt : Model * Cmd<Msg> =
    let model =
        { User = User.create ()
          ActivePage = LandingPage
          Route = initialRouteOpt |> Option.defaultValue Other
          NameInput = ""
          NameInputErrorOpt = None
          RoomIdInput = ""
          RoomIdInputErrorOpt = None
          ReconnectErrorOpt = None
          StartGameErrorOpt = None }

    model, cmdFor initialRouteOpt

let urlUpdate routeOpt model =
    let model' =
        match routeOpt with
        | Some route -> { model with Route = route }
        | None -> model

    model', cmdFor routeOpt

let lobbyRouteStr model room =
    let ridStr = RoomId.value room.Id
    let uidStr =
        model.User
        |> User.userId
        |> UserId.value
        |> string

    sprintf "#lobby?roomId=%s&userId=%s" ridStr uidStr

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | StartCreatingRoom ->
        { model with ActivePage = UsernamePage CreateRoom }, Cmd.none

    | SetNameInput value ->
        { model with NameInput = value }, Cmd.none

    | SubmitName (name, followUpAction) ->
        match name with
        | "" ->
            { model with User = User.unassignName model.User
                         NameInputErrorOpt = Some "You must enter a name" },
            Cmd.none
        | s ->
            let namedUser = User.assignName s model.User
            { model with User = Named namedUser },
                Cmd.ofMsg <| followUpAction namedUser

    | CreateRoom user ->
        model, Cmd.OfAsync.perform consequencesApi.createRoom user RoomCreated

    | RoomCreated room ->
        let cmd = Navigation.Navigation.newUrl <| lobbyRouteStr model room
        { model with ActivePage = Lobby room }, cmd

    | StartJoiningRoom ->
        { model with ActivePage = RoomIdPage }, Cmd.none

    | SetRoomIdInput value ->
        { model with RoomIdInput = value }, Cmd.none

    | SubmitRoomId s ->
        model, Cmd.OfAsync.perform consequencesApi.validateRoomId s HandleRoomIdValidation

    | HandleRoomIdValidation roomIdOpt ->
        match roomIdOpt with
        | Some roomId ->
            let action = fun u -> JoinRoom (roomId, u)
            { model with ActivePage = UsernamePage action }, Cmd.none
        | None ->
            let error = sprintf "Room \"%s\" does not exist" model.RoomIdInput
            { model with RoomIdInputErrorOpt = Some error }, Cmd.none

    | JoinRoom (roomId, user) ->
        model, Cmd.OfAsync.perform consequencesApi.joinRoom (roomId, user) HandleJoinRoomResult

    | HandleJoinRoomResult result ->
        match result with
        | Error msg ->
            { model with RoomIdInputErrorOpt = Some msg
                         ActivePage = RoomIdPage },
            Cmd.none
        | Ok room ->
            let cmd = Navigation.Navigation.newUrl <| lobbyRouteStr model room
            { model with ActivePage = Lobby room }, cmd

    | Reconnect (rid, uid) ->
        model, Cmd.OfAsync.perform consequencesApi.reconnect (rid, uid) HandleReconnectResult

    | HandleReconnectResult result ->
        match result with
        | Error msg ->
            let cmd = Navigation.Navigation.modifyUrl "#"
            { model with ActivePage = LandingPage; ReconnectErrorOpt = Some msg; Route = Other }, cmd
        | Ok (room, user) ->
            { model with ActivePage = Lobby room; User = Named user }, Cmd.none

    | StartGame rid ->
        model, Cmd.OfAsync.perform consequencesApi.startGame rid HandleStartGameResult

    | HandleStartGameResult result ->
        match result with
        | Error msg ->
            { model with StartGameErrorOpt = Some msg }, Cmd.none
        | Ok room ->
            model, Cmd.none

open Fable.React
open Fulma

let errorHelpForWithModifiers modifiers msgOpt =
    let msg = Option.defaultValue "" msgOpt
    Help.help (Help.Option.Color IsDanger :: [ Help.Modifiers modifiers ]) [ str msg ]

let errorHelpFor = errorHelpForWithModifiers []

let landingPage (model : Model) (dispatch : Msg -> unit) =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "consequences" ]
            Box.box' [ ] [
                Field.div [ Field.IsGrouped ] [
                    Control.div [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch StartCreatingRoom)
                        ] [
                            str "Create a room"
                        ]
                    ]
                    Control.div [ ] [
                        Button.a [
                            Button.OnClick (fun _ -> dispatch StartJoiningRoom)
                        ] [
                            str "Join a room"
                        ]
                    ]
                ]
                errorHelpFor model.ReconnectErrorOpt
            ]
        ]
    ]

let usernamePage submitAction model (dispatch : Msg -> unit) =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p
                [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str "What's your name?" ]
            Box.box' [ ] [
                Field.div [ ] [
                    Label.label [ ] [ str "Name" ]
                    Control.div [ ] [
                        Input.text [
                            match model.NameInputErrorOpt with
                            | Some _ -> Input.Color IsDanger
                            | None -> ()
                            Input.Value model.NameInput
                            Input.OnChange (fun x -> SetNameInput x.Value |> dispatch)
                        ]
                    ]
                    errorHelpFor model.NameInputErrorOpt
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| SubmitName (model.NameInput, submitAction))
                        ] [
                            str "Submit"
                        ]
                    ]
                ]
            ]
        ]
    ]

let lobby room model dispatch =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p
                [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str <| sprintf "Room %s" (RoomId.value room.Id) ]
            Box.box' [ ] [
                Heading.p [ Heading.Is4 ] [
                    str "Players"
                ]
                Content.content [ ] [
                    ol [ ]
                        (Room.players room |> List.map (fun p -> li [ ] [ str p.Name ]))
                ]
                errorHelpForWithModifiers [ Modifier.Spacing (Spacing.MarginBottom, Spacing.Is3) ] model.StartGameErrorOpt
                if User.equal model.User (Named room.Owner) then
                    Field.div [ ] [
                        Control.p [ ] [
                            Button.a [
                                Button.Color IsPrimary
                                Button.OnClick (fun _ -> dispatch <| StartGame room.Id)
                            ] [
                                str "Start game"
                            ]
                        ]
                    ]
                else ()

            ]
        ]
    ]

let roomIdPage model (dispatch : Msg -> unit) =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p
                [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str "Which room do you want to join?" ]
            Box.box' [ ] [
                Field.div [ ] [
                    Label.label [ ] [ str "Room ID" ]
                    Control.div [ ] [
                        Input.text [
                            match model.RoomIdInputErrorOpt with
                            | Some _ -> Input.Color IsDanger
                            | None -> ()
                            Input.Value model.RoomIdInput
                            Input.OnChange (fun x -> SetRoomIdInput x.Value |> dispatch)
                        ]
                    ]
                    errorHelpFor model.RoomIdInputErrorOpt
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| SubmitRoomId model.RoomIdInput)
                        ] [
                            str "Submit"
                        ]
                    ]
                ]
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    match model.ActivePage with
    | LandingPage -> landingPage model dispatch
    | UsernamePage submitAction -> usernamePage submitAction model dispatch
    | Lobby room -> lobby room model dispatch
    | RoomIdPage -> roomIdPage model dispatch
