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
    ShowForm | SaveNote | NoteChanged String

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
    , newNote : Maybe String
}
model: Model
model = {
    user = user
    , todos = [
        Todo 1 "write app"
        , Todo 2 "write app again" ]
    , mode = ShowButton
    , newNote = Nothing }

view: Model -> Html.Html Msg
view model = 
    Html.div [] [
        Html.h1 [] [Html.text (fullname model.user)]
        , Html.ul [] (List.map (\ todo -> createTodoLi todo) model.todos)
        , handleForm model
    ]

update: Msg -> Model -> Model
update msg model
    = case msg of
        ShowForm -> {model | mode = ShowAddNoteForm}
        SaveNote -> {model | mode = ShowButton, newNote = Nothing, todos = insertNewTodo model.newNote model.todos}
        NoteChanged note -> {model | newNote = Just note}

handleForm: Model -> Html.Html Msg
handleForm model =
    case model.mode of
        ShowButton -> Html.button [ onClick ShowForm ] [Html.text "New note" ]
        ShowAddNoteForm -> Html.div [] [
            Html.input [ onInput NoteChanged ] [ ]
            , Html.button [onClick SaveNote] [Html.text "Add"]
        ]

createTodoLi: Todo -> Html.Html Msg
createTodoLi todo =
    Html.li [] [Html.text (toString todo.id ++ ". " ++ todo.content)]

insertNewTodo: Maybe String -> List Todo -> List Todo
insertNewTodo content todos =
    case content of
        Nothing -> todos
        Just text ->
            Todo (getHighestNumberOrZero (getTodoIds todos) + 1) text 
            |> List.singleton 
            |> List.append todos

getHighestNumberOrZero: List Int -> Int
getHighestNumberOrZero numbers =
    List.maximum numbers |> Maybe.withDefault 0

getTodoIds: List Todo -> List Int
getTodoIds todos =
    List.map getTodoId todos

getTodoId: Todo -> Int
getTodoId {id} =
    id