import Html exposing (..)
import Html.Events exposing (..)

main = Html.beginnerProgram {model = model, view = view, update = update}

type alias User = {
    firstName : String
    , lastName : String
}
user: User
user = {firstName = "Kacper", lastName = "Moskala"}

fullname: User -> String
fullname user = user.firstName ++ " " ++ user.lastName

type Msg = 
    ShowForm | SaveNote

type Mode = 
    ShowAddNoteForm | ShowButton

type alias Todo = {
    id : Int
    , content : String
}

type alias Model = {
    user : User
    , todos : List Todo
    , mode : Mode
}
model: Model
model = {
    user = user
    , todos = [
        Todo 1 "write app"
        , Todo 2 "write app again" ]
    , mode = ShowButton }

view: Model -> Html Msg
view model = 
    Html.div [] [
        h1 [] [text (fullname model.user)]
        , ul [] (List.map (\ todo -> Html.li [] [text todo.content]) model.todos)
        , handleForm model.mode
    ]

handleForm model =
    case model of
        ShowButton -> button [ onClick ShowForm ] [text "New note" ]
        ShowAddNoteForm -> form [] [
            input [] []
            , button [onClick SaveNote] [text "Add"]
        ]

update: Msg -> Model -> Model
update msg model
    = case msg of
        ShowForm -> {model | mode = ShowAddNoteForm}
        SaveNote -> {model | mode = ShowButton}