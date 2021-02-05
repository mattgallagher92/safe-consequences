module Index

open Elmish
open Fable.Remoting.Client
open Shared

type ResponseMsg =
    | HisDescription of string
    | HisName of string
    | HerDescription of string
    | HerName of string
    | WhereTheyMet of string
    | WhatHeGaveHer of string
    | WhatHeSaidToHer of string
    | WhatSheSaidToHim of string
    | TheConsequence of string
    | WhatTheWorldSaid of string

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
    | HandleStartGameResult of Result<Game, string>
    | ResponseMsg of ResponseMsg

type Page =
    | LandingPage
    | UsernamePage of submitAction: (NamedUser -> Msg)
    | Lobby of Room
    | RoomIdPage
    | ResponsePage of Game

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
      StartGameErrorOpt: string option
      Response: Response }

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
          StartGameErrorOpt = None
          Response = Response.empty }

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

let updateResponse (msg: ResponseMsg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | HisDescription s -> { model with Response = { model.Response with HisDescription = s} }, Cmd.none
    | HisName s -> { model with Response = { model.Response with HisName = s} }, Cmd.none
    | HerDescription s -> { model with Response = { model.Response with HerDescription = s} }, Cmd.none
    | HerName s -> { model with Response = { model.Response with HerName = s} }, Cmd.none
    | WhereTheyMet s -> { model with Response = { model.Response with WhereTheyMet = s} }, Cmd.none
    | WhatHeGaveHer s -> { model with Response = { model.Response with WhatHeGaveHer = s} }, Cmd.none
    | WhatHeSaidToHer s -> { model with Response = { model.Response with WhatHeSaidToHer = s} }, Cmd.none
    | WhatSheSaidToHim s -> { model with Response = { model.Response with WhatSheSaidToHim = s} }, Cmd.none
    | TheConsequence s -> { model with Response = { model.Response with TheConsequence = s} }, Cmd.none
    | WhatTheWorldSaid s -> { model with Response = { model.Response with WhatTheWorldSaid = s} }, Cmd.none

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
        | Error msg -> { model with StartGameErrorOpt = Some msg }, Cmd.none
        | Ok game -> { model with ActivePage = ResponsePage game }, Cmd.none

    | ResponseMsg responseMsg -> updateResponse responseMsg model

open Fable.React
open Fulma

let private errorHelpForWithModifiers modifiers msgOpt =
    let msg = Option.defaultValue "" msgOpt
    Help.help (Help.Option.Color IsDanger :: [ Help.Modifiers modifiers ]) [ str msg ]

let private errorHelpFor = errorHelpForWithModifiers []

let private formField label errorOpt value onChange =
    Field.div [ ] [
        Label.label [ ] [ str label ]
        Control.div [ ] [
            Input.text [
                match errorOpt with
                | Some _ -> Input.Color IsDanger
                | None -> ()
                Input.Value value
                Input.OnChange onChange
            ]
        ]
        errorHelpFor errorOpt
    ]

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
                formField "Name" model.NameInputErrorOpt model.NameInput (fun x -> SetNameInput x.Value |> dispatch)
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
                formField "Room ID" model.RoomIdInputErrorOpt model.RoomIdInput (fun x -> SetRoomIdInput x.Value |> dispatch)
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

let responsePage game model dispatch =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p
                [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str "Enter your responses" ]
            Box.box' [ ] [
                formField "His description" None model.Response.HisDescription
                    (fun x -> dispatch <| (ResponseMsg << HisDescription) x.Value)
                formField "His name" None model.Response.HisName
                    (fun x -> dispatch <| (ResponseMsg << HisName) x.Value)
                formField "Her description" None model.Response.HerDescription
                    (fun x -> dispatch <| (ResponseMsg << HerDescription) x.Value)
                formField "Her name" None model.Response.HerName
                    (fun x -> dispatch <| (ResponseMsg << HerName) x.Value)
                formField "Where they met" None model.Response.WhereTheyMet
                    (fun x -> dispatch <| (ResponseMsg << WhereTheyMet) x.Value)
                formField "What he gave her" None model.Response.WhatHeGaveHer
                    (fun x -> dispatch <| (ResponseMsg << WhatHeGaveHer) x.Value)
                formField "What he said to her" None model.Response.WhatHeSaidToHer
                    (fun x -> dispatch <| (ResponseMsg << WhatHeSaidToHer) x.Value)
                formField "What she said to him" None model.Response.WhatSheSaidToHim
                    (fun x -> dispatch <| (ResponseMsg << WhatSheSaidToHim) x.Value)
                formField "The consequence" None model.Response.TheConsequence
                    (fun x -> dispatch <| (ResponseMsg << TheConsequence) x.Value)
                formField "What the world said" None model.Response.WhatTheWorldSaid
                    (fun x -> dispatch <| (ResponseMsg << WhatTheWorldSaid) x.Value)
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            // TODO: implement
                            Button.OnClick ignore
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
    | ResponsePage game -> responsePage game model dispatch
