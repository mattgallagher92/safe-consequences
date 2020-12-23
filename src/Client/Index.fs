module Index

open Elmish
open Fable.Remoting.Client
open Shared

type UsernamePageData =
    { Username: string
      ErrorMessageOpt: string option }

      static member Init () =
          { Username = ""
            ErrorMessageOpt = None }

type Page =
    | LandingPage
    | UsernamePage of UsernamePageData
    | Lobby of Room

module Page =

    let SetError msg =
        function
        | LandingPage -> LandingPage
        | UsernamePage data -> UsernamePage { data with ErrorMessageOpt = Some msg }
        | Lobby room -> Lobby room


type Model =
    { User: User
      ActivePage: Page }

type Msg =
    | StartCreatingRoom
    | SetNameInput of name:string
    | SubmitNameInput of name:string * namedUserToMsg:(NamedUser -> Msg)
    | CreateRoom of user:NamedUser
    | RoomCreated of room:Room

let consequencesApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IConsequencesApi>

let init (): Model * Cmd<Msg> =
    let model =
        { User = User.create ()
          ActivePage = LandingPage }
    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | StartCreatingRoom ->
        { model with ActivePage = UsernamePage <| UsernamePageData.Init () }, Cmd.none
    | SetNameInput value ->
        let p =
            match model.ActivePage with
            | LandingPage -> LandingPage
            | UsernamePage data -> UsernamePage { data with Username = value }
            | Lobby room -> Lobby room
        { model with ActivePage = p }, Cmd.none
    | SubmitNameInput (name, msgForNamedUser) ->
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
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch StartCreatingRoom)
                        ] [
                            str "Create a room"
                        ]
                    ]
                ]
            ]
        ]
    ]

let usernamePage data (dispatch : Msg -> unit) =
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
                            match data.ErrorMessageOpt with
                            | Some _ -> Input.Color IsDanger
                            | None -> ()
                            Input.Value data.Username
                            Input.OnChange (fun x -> SetNameInput x.Value |> dispatch)
                        ]
                    ]
                    match data.ErrorMessageOpt with
                    | Some msg -> Help.help [ Help.Option.Color IsDanger ] [ str msg ]
                    | None -> ()
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| SubmitNameInput (data.Username, CreateRoom))
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

let view (model : Model) (dispatch : Msg -> unit) =
    match model.ActivePage with
    | LandingPage -> landingPage model dispatch
    | UsernamePage data -> usernamePage data dispatch
    | Lobby room -> lobby room dispatch
