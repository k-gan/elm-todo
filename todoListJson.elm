module TodoListJson exposing (..)

import Json.Encode
import Json.Decode
import TodoList exposing (..)

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
    (decodeTodoList todosString).todos

userDecoder: Json.Decode.Decoder (List Todo)
userDecoder =
    Json.Decode.list todoDecoder

todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.map2 Todo (Json.Decode.field "id" Json.Decode.int) (Json.Decode.field "content" Json.Decode.string)

decodeTodoList : String -> TodosList
decodeTodoList todoString = 
    TodosList (Result.withDefault ([Todo 0 ""]) (Json.Decode.decodeString (Json.Decode.field "todos" userDecoder) todoString ))