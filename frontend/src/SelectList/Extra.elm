module SelectList.Extra exposing (selectElement)

import Maybe.Extra
import SelectList exposing (SelectList)


selectElement : a -> SelectList a -> Maybe (SelectList a)
selectElement a list =
    if a == SelectList.selected list then
        Just list

    else
        SelectList.selectBeforeIf ((==) a) list
            |> Maybe.Extra.orElse (SelectList.selectAfterIf ((==) a) list)
