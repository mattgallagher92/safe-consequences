module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Page =
    | LandingPage
    | UsernamePage
    | Lobby of Room

type Model =
    { User: User
      ActivePage: Page }

type Msg =
    | StartCreatingRoom
    | SetNameInput of string
    | CreateRoom
    | RoomCreated of Room

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
        { model with ActivePage = UsernamePage }, Cmd.none
    | SetNameInput value ->
        let user =
            match value with
            | "" -> User.unassignName model.User
            | _ -> Named <| User.assignName value model.User
        { model with User = user }, Cmd.none
    | CreateRoom ->
        match model.User with
        | Named user ->
            let cmd = Cmd.OfAsync.perform consequencesApi.createRoom user RoomCreated
            model, cmd
        | Anonymous user -> failwith "TODO"
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

let usernamePage (model : Model) (dispatch : Msg -> unit) =
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
                            Input.Value
                              ( match model.User with
                                | Anonymous uid -> ""
                                | Named user -> user.Name )
                            Input.OnChange (fun x -> SetNameInput x.Value |> dispatch)
                        ]
                    ]
                ]
                Field.div [ ] [
                    Control.p [ ] [
                        Button.a [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch CreateRoom)
                        ] [
                            str "Submit"
                        ]
                    ]
                ]
            ]
        ]
    ]

let lobby (room : Room) (model : Model) (dispatch : Msg -> unit) =
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
    | UsernamePage -> usernamePage model dispatch
    | Lobby room -> lobby room model dispatch
