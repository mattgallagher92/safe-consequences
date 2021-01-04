module Index

open Elmish
open Fable.Remoting.Client
open Shared
type Page =
    | LandingPage
    | UsernamePage of errorMessageOpt:string option
    | Lobby of Room
    | RoomIdPage of errorMessageOpt:string option

module Page =

    let SetError msg =
        function
        | LandingPage -> LandingPage
        | UsernamePage _ -> UsernamePage <| Some msg
        | Lobby room -> Lobby room
        | RoomIdPage _ -> RoomIdPage <| Some msg
type Model =
    { User: User
      ActivePage: Page
      NameInput: string
      RoomIdInput: string }

type Msg =
    | StartCreatingRoom
    | SetNameInput of name:string
    | SubmitName of name:string * namedUserToMsg:(NamedUser -> Msg)
    | CreateRoom of user:NamedUser
    | RoomCreated of room:Room
    | StartJoiningRoom
    | SetRoomIdInput of roomId:string
    | JoinRoom of roomId:string

let consequencesApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IConsequencesApi>

let init () : Model * Cmd<Msg> =
    let model =
        { User = User.create ()
          ActivePage = LandingPage
          NameInput = ""
          RoomIdInput = "" }
    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | StartCreatingRoom ->
        { model with ActivePage = UsernamePage None }, Cmd.none
    | SetNameInput value ->
        { model with NameInput = value }, Cmd.none
    | SubmitName (name, msgForNamedUser) ->
        let u =
            match name with
            | "" -> User.unassignName model.User
            | s -> Named <| User.assignName s model.User
        let newModel = { model with User = u }

        match newModel.User with
        | Named user ->
            let cmd = Cmd.ofMsg <| msgForNamedUser user
            newModel, cmd
        | Anonymous _ ->
            { newModel with ActivePage = Page.SetError "You must enter a name" newModel.ActivePage }, Cmd.none
    | CreateRoom user ->
        let cmd = Cmd.OfAsync.perform consequencesApi.createRoom user RoomCreated
        model, cmd
    | RoomCreated room ->
        { model with ActivePage = Lobby room }, Cmd.none
    | StartJoiningRoom ->
        { model with ActivePage = RoomIdPage None }, Cmd.none
    | SetRoomIdInput value ->
        { model with RoomIdInput = value }, Cmd.none
    | JoinRoom roomdId -> failwith "TODO"

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
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
            ]
        ]
    ]

let usernamePage usernameInput errorMessageOpt (dispatch : Msg -> unit) =
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
                            match errorMessageOpt with
                            | Some _ -> Input.Color IsDanger
                            | None -> ()
                            Input.Value usernameInput
                            Input.OnChange (fun x -> SetNameInput x.Value |> dispatch)
                        ]
                    ]
                    match errorMessageOpt with
                    | Some msg -> Help.help [ Help.Option.Color IsDanger ] [ str msg ]
                    | None -> ()
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| SubmitName (usernameInput, CreateRoom))
                        ] [
                            str "Submit"
                        ]
                    ]
                ]
            ]
        ]
    ]

let lobby (room : Room) (dispatch : Msg -> unit) =
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
                    ol [ ] [
                        li [ ] [ str <| room.Owner.Name ]
                    ]
                ]
            ]
        ]
    ]

let roomIdPage roomIdInput errorMessageOpt (dispatch : Msg -> unit) =
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
                    Label.label [ ] [ str "RoomId" ]
                    Control.div [ ] [
                        Input.text [
                            match errorMessageOpt with
                            | Some _ -> Input.Color IsDanger
                            | None -> ()
                            Input.Value roomIdInput
                            Input.OnChange (fun x -> SetRoomIdInput x.Value |> dispatch)
                        ]
                    ]
                    match errorMessageOpt with
                    | Some msg -> Help.help [ Help.Option.Color IsDanger ] [ str msg ]
                    | None -> ()
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| JoinRoom roomIdInput)
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
    | UsernamePage errorMessageOpt -> usernamePage model.NameInput errorMessageOpt dispatch
    | Lobby room -> lobby room dispatch
    | RoomIdPage errorMessageOpt -> roomIdPage model.RoomIdInput errorMessageOpt dispatch
