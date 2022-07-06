module Table exposing (Table, empty, filter, fromDict, get, insert, isEmpty, map, member, remove, size, toList, update)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Uuid exposing (Uuid)


type Table a
    = Table (Dict String a)



-- BUILD


empty : Table a
empty =
    Table Dict.empty


insert : Uuid -> a -> Table a -> Table a
insert id value (Table table) =
    Dict.insert (Uuid.toString id) value table
        |> Table


update : Uuid -> (Maybe a -> Maybe a) -> Table a -> Table a
update id alter (Table table) =
    Dict.update (Uuid.toString id) alter table
        |> Table


remove : Uuid -> Table a -> Table a
remove id (Table table) =
    Dict.remove (Uuid.toString id) table
        |> Table



-- CONVERT


fromDict : Dict String a -> Table a
fromDict dict =
    Table dict



-- QUERY


member : Uuid -> Table a -> Bool
member id (Table table) =
    Dict.member (Uuid.toString id) table


get : Uuid -> Table a -> Maybe a
get id (Table table) =
    Dict.get (Uuid.toString id) table


size : Table a -> Int
size (Table table) =
    Dict.size table


isEmpty : Table a -> Bool
isEmpty (Table table) =
    Dict.isEmpty table



-- LISTS


toList : Table a -> List ( Uuid, a )
toList (Table table) =
    table
        |> Dict.toList
        |> List.filterMap
            (\( idString, value ) ->
                idString
                    |> Uuid.fromString
                    |> Maybe.map (\id -> ( id, value ))
            )



-- TRANSFORM


map : (Uuid -> a -> b) -> Table a -> Table b
map func (Table table) =
    Dict.filterMap
        (\idString item ->
            Uuid.fromString idString
                |> Maybe.map (\id -> func id item)
        )
        table
        |> Table


filter : (Uuid -> a -> Bool) -> Table a -> Table a
filter isGood (Table table) =
    Dict.filter
        (\idString value ->
            Uuid.fromString idString
                |> Maybe.map (\id -> isGood id value)
                |> Maybe.withDefault False
        )
        table
        |> Table
