import Html
import Html.Attributes
import Html.Events exposing (..)
import TodoList exposing (..)
import TodoListJson exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Button as BootstrapButton
import Bootstrap.Grid as Grid


-- main = Html.beginnerProgram {model = model, view = view, update = update}
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
init: (Model, Cmd Msg)
init = (model, Cmd.none)

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
        Todo 1 "write app" Pending
        , Todo 2 "write app again" Done ]
    , mode = ShowButton
    , newNote = Nothing
    , encodedTodos = Nothing }

view: Model -> Html.Html Msg
view model = 
    Html.div [] [
        CDN.stylesheet
        , Html.h1 [] [Html.text (fullname model.user)]
        , Html.ul [] (List.map (\ todo -> createTodoLi todo) model.todos)
        , handleForm model
        , Html.br [] []
        , Html.br [] []
        , BootstrapButton.button [ BootstrapButton.success, BootstrapButton.onClick SaveToFile ] [Html.text "Save to file"]
        , Html.br [] []
        , Html.br [] []
        , Html.div [ Html.Attributes.style [("display", "block" )] ] [
            Html.textarea 
                [Html.Attributes.style [("width", "400px"), ("height", "150px")],
                    onInput EncodedTodoListChanged ] 
                [ Maybe.withDefault "" model.encodedTodos |> Html.text]
            , Html.br [] []
            , BootstrapButton.button [ BootstrapButton.primary, BootstrapButton.onClick LoadFromFile ] [Html.text "Load notes"]
        ]
    ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model
    = case msg of
        ShowForm -> ({model | mode = ShowAddNoteForm}, Cmd.none)
        SaveNote -> (
            {model | mode = ShowButton, newNote = Nothing, todos = insertNewTodo model.newNote model.todos}
            , Cmd.none)
        NoteChanged note -> ({model | newNote = Just note}, Cmd.none)
        SaveToFile -> (
            { model | encodedTodos = encodeTodos model.todos |> encodeTodosToString |> Just }
            , Cmd.none)
        LoadFromFile -> (
            {model | todos = Maybe.withDefault "" model.encodedTodos |> decodeTodosFromString }
            , Cmd.none)
        EncodedTodoListChanged encodedTodoList -> 
            ({ model | encodedTodos = Maybe.Just encodedTodoList }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


handleForm: Model -> Html.Html Msg
handleForm model =
    case model.mode of
        ShowButton -> Html.button [ onClick ShowForm ] [Html.text "New note" ]
        ShowAddNoteForm -> Html.div [] [
            Html.input [ onInput NoteChanged ] [ ]
            , BootstrapButton.button [ BootstrapButton.primary, BootstrapButton.onClick SaveNote] [Html.text "Add"]
        ]

createTodoLi: Todo -> Html.Html Msg
createTodoLi todo =
    Html.li
        [Html.Attributes.style [("text-decoration", case todo.status of 
            Done -> "line-through" 
            _ -> "none")
        ]]
        [Html.text (toString todo.id ++ ". " ++ todo.content)]
