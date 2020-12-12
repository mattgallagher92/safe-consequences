module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Page =
    | LandingPage
    | Lobby of Room

type Model =
    { User: User
      ActivePage: Page }

type Msg =
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
    | CreateRoom ->
        let cmd = Cmd.OfAsync.perform consequencesApi.createRoom model.User RoomCreated
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

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Field.div [ Field.IsGrouped ] [
            Control.p [ ] [
                Button.a [
                    Button.Color IsPrimary
                    Button.OnClick (fun _ -> dispatch CreateRoom)
                ] [
                    str "Create a room"
                ]
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    match model.ActivePage with
    | LandingPage ->
        Hero.hero [
            Hero.Color IsPrimary
            Hero.IsFullHeight
            Hero.Props [
                Style [
                    Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                    BackgroundSize "cover"
                ]
            ]
        ] [
            Hero.head [ ] [
                Navbar.navbar [ ] [
                    Container.container [ ] [ navBrand ]
                ]
            ]

            Hero.body [ ] [
                Container.container [ ] [
                    Column.column [
                        Column.Width (Screen.All, Column.Is6)
                        Column.Offset (Screen.All, Column.Is3)
                    ] [
                        Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "consequences" ]
                        containerBox model dispatch
                    ]
                ]
            ]
        ]
    | Lobby room -> str <| string room
