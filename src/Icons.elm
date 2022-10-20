module Icons exposing (edit, done, history, undo, swap, gridView)

import Html exposing (Html)
import Html.Attributes as Attributes

type alias IconMaker msg = List (Html.Attribute msg) -> Html msg

iconMaker : String -> IconMaker msg
iconMaker iconName =
  \attributes ->
    Html.span (Attributes.class "material-icons" :: attributes) [ Html.text iconName]

edit : IconMaker msg
edit = iconMaker "edit"

done : IconMaker msg
done = iconMaker "done"

history : IconMaker msg
history = iconMaker "history"

undo : IconMaker msg
undo = iconMaker "undo"

swap : IconMaker msg
swap = iconMaker "swap_vert"

gridView : IconMaker msg
gridView = iconMaker "grid_view"