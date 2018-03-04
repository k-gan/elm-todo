import Html
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

view: Model -> Html.Html Msg
view model = 
    Html.div [] [
        Html.h1 [] [Html.text (fullname model.user)]
        , Html.ul [] (List.map (\ todo -> Html.li [] [Html.text todo.content]) model.todos)
        , handleForm model.mode
    ]

handleForm model =
    case model of
        ShowButton -> Html.button [ onClick ShowForm ] [Html.text "New note" ]
        ShowAddNoteForm -> Html.form [] [
            Html.input [] []
            , Html.button [onClick SaveNote] [Html.text "Add"]
        ]

update: Msg -> Model -> Model
update msg model
    = case msg of
        ShowForm -> {model | mode = ShowAddNoteForm}
        SaveNote -> {model | mode = ShowButton}