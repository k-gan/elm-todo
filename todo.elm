import Html
import Html.Attributes
import Html.Events exposing (..)
import Json.Encode
import Json.Decode

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
    ShowForm  | SaveNote | NoteChanged String | SaveToFile | LoadFromFile | EncodedTodoListChanged String

type Mode = 
    ShowAddNoteForm | ShowButton

type alias TodosList = {
    todos: List (Todo)
}

type alias Todo = {
    id : Int
    , content : String
}

type alias Model = {
    user : User
    , todos : List Todo
    , mode : Mode
    , newNote : Maybe String
    , encodedTodos : Maybe String
}
model: Model
model = {
    user = user
    , todos = [
        Todo 1 "write app"
        , Todo 2 "write app again" ]
    , mode = ShowButton
    , newNote = Nothing
    , encodedTodos = Nothing }

view: Model -> Html.Html Msg
view model = 
    Html.div [] [
        Html.h1 [] [Html.text (fullname model.user)]
        , Html.ul [] (List.map (\ todo -> createTodoLi todo) model.todos)
        , handleForm model
        , Html.br [] []
        , Html.br [] []
        , Html.button [ onClick SaveToFile ] [Html.text "Save to file"]
        , Html.br [] []
        , Html.br [] []
        , Html.textarea 
            [Html.Attributes.style [("width", "400px"), ("height", "150px")],
                onInput EncodedTodoListChanged ] 
            [ Maybe.withDefault "" model.encodedTodos |> Html.text]
        , Html.br [] []
        , Html.button [ onClick LoadFromFile ] [Html.text "Load notes"]
    ]

update: Msg -> Model -> Model
update msg model
    = case msg of
        ShowForm -> {model | mode = ShowAddNoteForm}
        SaveNote -> {model | mode = ShowButton, newNote = Nothing, todos = insertNewTodo model.newNote model.todos}
        NoteChanged note -> {model | newNote = Just note}
        SaveToFile -> { model | encodedTodos = encodeTodos model.todos |> encodeTodosToString |> Just }
        LoadFromFile -> {model | todos = Maybe.withDefault "" model.encodedTodos |> decodeTodosFromString }
        EncodedTodoListChanged encodedTodoList -> { model | encodedTodos = Maybe.Just encodedTodoList }

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

encodeTodosToString: Json.Encode.Value -> String
encodeTodosToString encodedTodos =
    Json.Encode.object [("todos", encodedTodos)]
    |> Json.Encode.encode 4

encodeTodos: List Todo -> Json.Encode.Value
encodeTodos todos =
    List.map (\t -> encodeTodo t) todos
    |> Json.Encode.list

encodeTodo: Todo -> Json.Encode.Value
encodeTodo todo = 
    Json.Encode.object [
        ("id", Json.Encode.int todo.id)
        , ("content", Json.Encode.string todo.content)
    ]

decodeTodosFromString: String -> List Todo
decodeTodosFromString todosString = 
    -- logTodo2 (logTodo todosString)
    (decodeTodoList todosString).todos

userDecoder: Json.Decode.Decoder (List Todo)
userDecoder =
    Json.Decode.list todoDecoder

todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.map2 Todo (Json.Decode.field "id" Json.Decode.int) (Json.Decode.field "content" Json.Decode.string)

decodeTodoList : String -> TodosList
decodeTodoList todoString = 
    -- TodoList Json.Decode.field "todos" userDecoder
    TodosList (Result.withDefault ([Todo 0 ""]) (Json.Decode.decodeString (Json.Decode.field "todos" userDecoder) todoString ))