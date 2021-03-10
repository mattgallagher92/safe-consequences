open Farmer
open Farmer.Builders

let app = webApp {
    name "safe-consequences"
    zip_deploy @"..\..\deploy"
}

let deployment = arm {
    location Location.NorthEurope
    add_resource app
}

deployment
|> Deploy.execute "safe-consequences-group" Deploy.NoParameters
|> ignore
