module TodoList exposing (..)

import Date exposing (Date)

type alias TodosList = {
    todos: List (Todo)
}

type alias Todo = {
    id : Int
    , content : String
    , status : TaskStatus
}

type TaskStatus = Pending | Done

getTodoIds: List Todo -> List Int
getTodoIds todos =
    List.map getTodoId todos

getTodoId: Todo -> Int
getTodoId {id} =
    id

insertNewTodo: Maybe String -> List Todo -> List Todo
insertNewTodo content todos =
    case content of
        Nothing -> todos
        Just text ->
            Todo ((getHighestNumberOrZero (getTodoIds todos))  + 1) text Pending
            |> List.singleton 
            |> List.append todos

getHighestNumberOrZero: List Int -> Int
getHighestNumberOrZero numbers =
    List.maximum numbers |> Maybe.withDefault 0