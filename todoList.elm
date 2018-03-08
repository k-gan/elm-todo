module TodoList exposing (..)

type alias TodosList = {
    todos: List (Todo)
}

type alias Todo = {
    id : Int
    , content : String
}

getTodoIds: List Todo -> List Int
getTodoIds todos =
    List.map getTodoId todos

getTodoId: Todo -> Int
getTodoId {id} =
    id