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
    | HandleStartGameResult of Result<Room, string>
    | ResponseMsg of ResponseMsg
    | SubmitResponse of RoomId
    | HandleResponseSubmittedResult of Result<Room, Response.Error>
    | ReturnToLobby of Room

type Page =
    | BlankPage
    | LandingPage of reconnectErrorOpt:string option
    | UsernamePage of nameInputErrorOpt:string option * submitAction:(NamedUser -> Msg)
    | Lobby of startGameErrorOpt:string option * Room
    | RoomIdPage of roomIdInputErrorOpt:string option
    | ResponsePage of Response.Error * Room
    | WaitingForOtherPlayersPage of Room
    | StoryPage of Room

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
      RoomIdInput: string
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
    let initialPage =
        match initialRouteOpt with
        | Some (Query _) -> BlankPage
        | _ -> LandingPage None

    let model =
        { User = User.create ()
          ActivePage = initialPage
          Route = initialRouteOpt |> Option.defaultValue Other
          NameInput = ""
          RoomIdInput = ""
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
        |> UserId.asString

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
        { model with ActivePage = UsernamePage (None, CreateRoom) }, Cmd.none

    | SetNameInput value ->
        { model with NameInput = value }, Cmd.none

    | SubmitName (name, followUpAction) ->
        match name with
        | "" ->
            let newPage = UsernamePage <| (Some "You must enter a name", followUpAction)
            { model with User = User.unassignName model.User; ActivePage = newPage }, Cmd.none
        | s ->
            let namedUser = User.assignName s model.User
            { model with User = Named namedUser }, Cmd.ofMsg <| followUpAction namedUser

    | CreateRoom user ->
        model, Cmd.OfAsync.perform consequencesApi.createRoom user RoomCreated

    | RoomCreated room ->
        let cmd = Navigation.Navigation.newUrl <| lobbyRouteStr model room
        { model with ActivePage = Lobby (None, room) }, cmd

    | StartJoiningRoom ->
        { model with ActivePage = RoomIdPage None }, Cmd.none

    | SetRoomIdInput value ->
        { model with RoomIdInput = value }, Cmd.none

    | SubmitRoomId s ->
        model, Cmd.OfAsync.perform consequencesApi.validateRoomId s HandleRoomIdValidation

    | HandleRoomIdValidation roomIdOpt ->
        match roomIdOpt with
        | Some roomId ->
            let action = fun u -> JoinRoom (roomId, u)
            { model with ActivePage = UsernamePage (None, action) }, Cmd.none
        | None ->
            let error = sprintf "Room \"%s\" does not exist" model.RoomIdInput
            { model with ActivePage = RoomIdPage <| Some error }, Cmd.none

    | JoinRoom (roomId, user) ->
        model, Cmd.OfAsync.perform consequencesApi.joinRoom (roomId, user) HandleJoinRoomResult

    | HandleJoinRoomResult result ->
        match result with
        | Error msg ->
            { model with ActivePage = RoomIdPage <| Some msg }, Cmd.none
        | Ok room ->
            let cmd = Navigation.Navigation.newUrl <| lobbyRouteStr model room
            { model with ActivePage = Lobby (None, room) }, cmd

    | Reconnect (rid, uid) ->
        model, Cmd.OfAsync.perform consequencesApi.reconnect (rid, uid) HandleReconnectResult

    | HandleReconnectResult result ->
        match result with
        | Error msg ->
            let cmd = Navigation.Navigation.modifyUrl "#"
            { model with ActivePage = LandingPage <| Some msg; Route = Other }, cmd
        | Ok (room, user) ->
            match room.Game with
            | NotStarted ->
                { model with ActivePage = Lobby (None, room); User = Named user }, Cmd.none
            | WaitingForResponses responses ->
                if Map.containsKey user responses
                then { model with ActivePage = WaitingForOtherPlayersPage room; User = Named user }, Cmd.none
                else { model with ActivePage = ResponsePage (Response.Error.empty, room); User = Named user }, Cmd.none
            | AllResponsesReceived _ ->
                { model with ActivePage = StoryPage room; User = Named user}, Cmd.none


    | StartGame rid ->
        model, Cmd.OfAsync.perform consequencesApi.startGame rid HandleStartGameResult

    | HandleStartGameResult result ->
        match result with
        | Error msg ->
            let room =
                match model.ActivePage with
                | Lobby (_, r) -> r
                | _ -> failwith "Assumption violated: HandleStartGameResult is only handled in Lobby."
            { model with ActivePage = Lobby <| (Some msg, room) }, Cmd.none
        | Ok room ->
            { model with ActivePage = ResponsePage (Response.Error.empty, room) }, Cmd.none

    | ResponseMsg responseMsg -> updateResponse responseMsg model

    | SubmitResponse rid ->
        let uid = User.userId model.User

        model,
        Cmd.OfAsync.perform consequencesApi.submitResponse (rid, uid, model.Response) HandleResponseSubmittedResult

    | HandleResponseSubmittedResult result ->
        match result with
        | Error responseError ->
            let room =
                match model.ActivePage with
                | ResponsePage (_, r) -> r
                | _ -> failwith "Assumption violated: HandleResponseSubmittedResult is only handled on ResponsePage."
            { model with ActivePage = ResponsePage <| (responseError, room) }, Cmd.none
        | Ok room ->
            match room.Game with
            | NotStarted -> failwith "Bug: response handled okay for inactive game."
            | WaitingForResponses _ -> { model with ActivePage = WaitingForOtherPlayersPage room } , Cmd.none
            | AllResponsesReceived _ -> { model with ActivePage = StoryPage room } , Cmd.none

    | ReturnToLobby room -> { model with ActivePage = Lobby (None, room) }, Cmd.none

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

let private primaryButton onClick label =
    Control.div [ ] [ Button.button [ Button.Color IsPrimary; Button.OnClick onClick ] [ str label ] ]

let private secondaryButton onClick label =
    Control.div [ ] [ Button.button [ Button.OnClick onClick ] [ str label ] ]

let private page headingText content =
    Container.container [ ] [
        Column.column [
            Column.Width (Screen.All, Column.Is6)
            Column.Offset (Screen.All, Column.Is3)
        ] [
            Heading.p
                [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str headingText ]
            Box.box' [ ] content
        ]
    ]

let landingPage reconnectErrorOpt (model : Model) (dispatch : Msg -> unit) =
    [
        Field.div [ Field.IsGrouped ] [
            primaryButton (fun _ -> dispatch StartCreatingRoom) "Create a room"
            secondaryButton (fun _ -> dispatch StartJoiningRoom) "Join a room"
        ]
        errorHelpFor reconnectErrorOpt
    ]
    |> page "Consequences"

let usernamePage nameInputErrorOpt submitAction model (dispatch : Msg -> unit) =
    [
        formField "Name" nameInputErrorOpt model.NameInput (fun x -> SetNameInput x.Value |> dispatch)
        Field.div [ ] [ primaryButton (fun _ -> dispatch <| SubmitName (model.NameInput, submitAction)) "Submit" ]
    ]
    |> page "What's your name?"

let lobby startGameErrorOpt room model dispatch =
    [
        Heading.p [ Heading.Is4 ] [ str "Players" ]

        Content.content [ ] [ ol [ ] (Room.players room |> List.map (fun p -> li [ ] [ str p.Name ])) ]

        errorHelpForWithModifiers [ Modifier.Spacing (Spacing.MarginBottom, Spacing.Is3) ] startGameErrorOpt

        if User.equal model.User (Named room.Owner)
        then Field.div [ ] [ primaryButton (fun _ -> dispatch <| StartGame room.Id) "Start game" ]
        else ()
    ]
    |> page (sprintf "Room %s" (RoomId.value room.Id))

let roomIdPage roomIdInputErrorOpt model (dispatch : Msg -> unit) =
    [
        formField "Room ID" roomIdInputErrorOpt model.RoomIdInput (fun x -> SetRoomIdInput x.Value |> dispatch)
        Field.div [ ] [ primaryButton (fun _ -> dispatch <| SubmitRoomId model.RoomIdInput) "Submit" ]
    ]
    |> page "Which room do you want to join?"

open Shared.Response

let responsePage responseError room model dispatch =
    [
        formField "His description" responseError.HisDescriptionErrorOpt model.Response.HisDescription
            (fun x -> dispatch <| (ResponseMsg << HisDescription) x.Value)
        formField "His name" responseError.HisNameErrorOpt model.Response.HisName
            (fun x -> dispatch <| (ResponseMsg << HisName) x.Value)
        formField "Her description" responseError.HerDescriptionErrorOpt model.Response.HerDescription
            (fun x -> dispatch <| (ResponseMsg << HerDescription) x.Value)
        formField "Her name" responseError.HerNameErrorOpt model.Response.HerName
            (fun x -> dispatch <| (ResponseMsg << HerName) x.Value)
        formField "Where they met" responseError.WhereTheyMetErrorOpt model.Response.WhereTheyMet
            (fun x -> dispatch <| (ResponseMsg << WhereTheyMet) x.Value)
        formField "What he gave her" responseError.WhatHeGaveHerErrorOpt model.Response.WhatHeGaveHer
            (fun x -> dispatch <| (ResponseMsg << WhatHeGaveHer) x.Value)
        formField "What he said to her" responseError.WhatHeSaidToHerErrorOpt model.Response.WhatHeSaidToHer
            (fun x -> dispatch <| (ResponseMsg << WhatHeSaidToHer) x.Value)
        formField "What she said to him" responseError.WhatSheSaidToHimErrorOpt model.Response.WhatSheSaidToHim
            (fun x -> dispatch <| (ResponseMsg << WhatSheSaidToHim) x.Value)
        formField "The consequence" responseError.TheConsequenceErrorOpt model.Response.TheConsequence
            (fun x -> dispatch <| (ResponseMsg << TheConsequence) x.Value)
        formField "What the world said" responseError.WhatTheWorldSaidErrorOpt model.Response.WhatTheWorldSaid
            (fun x -> dispatch <| (ResponseMsg << WhatTheWorldSaid) x.Value)
        Field.div [ ] [ primaryButton (fun _ -> dispatch <| SubmitResponse room.Id) "Submit" ]
    ]
    |> page "Enter your responses"

let waitingForOtherPlayersPage room model dispatch =
    [
        Heading.p [ Heading.Is4 ] [ str "Waiting for" ]
        Content.content [ ] [
            ul [ ] (Room.playersWhoHaveNotSubmittedResponses room |> List.map (fun p -> li [ ] [ str p.Name ]))
        ]
        Heading.p [ Heading.Is4 ] [ str "Submitted responses" ]
        Content.content [ ] [
            ul [ ] (Room.playersWhoHaveSubmittedResponses room |> List.map (fun p -> li [ ] [ str p.Name ]))
        ]
    ]
    |> page "Waiting for other players"

let storyPage room model dispatch =
    let r =
        match model.User with
        | Named u -> Room.storyFor room u
        | Anonymous _ -> Error "Cannot get story: your user details are invalid."
        |> Result.unsafeExtractOkContent

    let inP s = s |> str |> List.singleton |> p []

    [
        Content.content [ ] [
            sprintf "The %s" r.HisDescription |> inP
            sprintf "%s" r.HisName |> inP
            sprintf "met the %s" r.HerDescription |> inP
            sprintf "%s" r.HerName |> inP
            sprintf "at %s." r.WhereTheyMet |> inP
            sprintf "He gave her %s." r.WhatHeGaveHer |> inP
            sprintf "He said \"%s\"." r.WhatHeSaidToHer |> inP
            sprintf "She said \"%s\"." r.WhatSheSaidToHim |> inP
            sprintf "The consequence was %s." r.TheConsequence |> inP
            sprintf "The world said \"%s\"." r.WhatTheWorldSaid |> inP
        ]
        primaryButton (fun _ -> dispatch <| ReturnToLobby room) "Return to the lobby"
    ]
    |> page "The story for you to read out"

let view (model : Model) (dispatch : Msg -> unit) =
    match model.ActivePage with
    | BlankPage -> div [] []
    | LandingPage reconnectErrorOpt -> landingPage reconnectErrorOpt model dispatch
    | UsernamePage (nameInputErrorOpt, submitAction) -> usernamePage nameInputErrorOpt submitAction model dispatch
    | Lobby (startGameErrorOpt, room) -> lobby startGameErrorOpt room model dispatch
    | RoomIdPage roomIdInputErrorOpt -> roomIdPage roomIdInputErrorOpt model dispatch
    | ResponsePage (responseError, room) -> responsePage responseError room model dispatch
    | WaitingForOtherPlayersPage room -> waitingForOtherPlayersPage room model dispatch
    | StoryPage room -> storyPage room model dispatch
