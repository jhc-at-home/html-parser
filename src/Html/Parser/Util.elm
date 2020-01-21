module Html.Parser.Util exposing (toVirtualDom)

{-| Utility functions that may help you digging into the contents.


# Virtual DOM

@docs toVirtualDom

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Parser exposing (Node(..))
import VirtualDom


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List (Node a) -> (a -> String) -> List (Html msg)
toVirtualDom nodes repr =
    List.map (toVirtualDomEach repr) nodes


toVirtualDomEach : (a -> String) -> Node a -> Html msg
toVirtualDomEach repr node =
    case node of
        Element name attrs children ->
            Html.node name (List.map toAttribute attrs) (toVirtualDom children repr)

        Leaf s ->
            text (repr s)

        Comment _ ->
            text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    attribute name value
