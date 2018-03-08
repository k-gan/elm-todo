module TodoList exposing (..)

type alias TodosList = {
    todos: List (Todo)
}

type alias Todo = {
    id : Int
    , content : String
}