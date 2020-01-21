module Html.Parser exposing
    ( run, runComplex, Node(..), Attribute
    , node, nodeToString
    )

{-| Parse HTML 5 in Elm.
See <https://www.w3.org/TR/html5/syntax.html>

@docs run, Node, Attribute


# Internals

If you are building a parser of your own using [`elm/parser`][elm-parser] and
you need to parse HTML... This section is for you!

[elm-parser]: https://package.elm-lang.org/packages/elm/parser/latest

@docs node, nodeToString

-}

import Dict
import Hex
import Html.Parser.NamedCharacterReferences as NamedCharacterReferences
import Parser exposing ((|.), (|=), Parser)


{-| Run the parser!

    run "<div><p>Hello, world!</p></div>"
    -- => Ok [ Element "div" [] [ Element "p" [] [ Leaf "Hello, world!" ] ] ]

-}
run : String -> Result (List Parser.DeadEnd) (List (Node String))
run input =
    runComplex identity (++) "" input

{-| Run the parser!

    runComplex "<div><p>5</p></div>" (String.toInt >> withDefault 0) (+) 0
    -- => Ok [ Element "div" [] [ Element "p" [] [ Leaf 5 ] ] ]

-}
runComplex : (String -> a)
           -> (a -> a -> a)
           -> a
           -> String
           -> Result (List Parser.DeadEnd) (List (Node a))
runComplex convert combine empty input =
    if String.isEmpty input then
        Ok []

    else
        Parser.run (oneOrMore "node" (node convert combine empty)) input


-- Node


{-| An HTML node. It can either be:

  - Text
  - Element (with its **tag name**, **attributes** and **children**)
  - Comment

-}
type Node a
    = Leaf a
    | Element String (List Attribute) (List (Node a))
    | Comment String


{-| An HTML attribute. For instance:

    ( "href", "https://elm-lang.org" )

-}
type alias Attribute =
    ( String, String )


{-| Parse an HTML node.

You can use this in your own parser to add support for HTML 5.

-}
node : (String -> a) -> (a -> a -> a) -> a -> Parser (Node a)
node convert combine empty =
    Parser.oneOf
        [ leaf convert combine empty
        , comment
        , element convert combine empty
        ]


{-| Turn a parser node back into its HTML string.

For instance:

    Element "a"
        [ ( "href", "https://elm-lang.org" ) ]
        [ Text "Elm" ]
        |> nodeToString

Produces `<a href="https://elm-lang.org">Elm</a>`.

-}
nodeToString : (a -> String) -> Node a -> String
nodeToString toStr node_ =
    let
        attributeToString ( attr, value ) =
            attr ++ "=\"" ++ value ++ "\""
    in
    case node_ of
        Leaf leaf_ ->
            toStr leaf_

        Element name attributes children ->
            let
                maybeAttributes =
                    case attributes of
                        [] ->
                            ""

                        _ ->
                            " " ++ String.join " " (List.map attributeToString attributes)
            in
            if isVoidElement name then
                String.concat
                    [ "<"
                    , name
                    , maybeAttributes
                    , ">"
                    ]

            else
                String.concat
                    [ "<"
                    , name
                    , maybeAttributes
                    , ">"
                    , String.join "" (List.map (nodeToString toStr) children)
                    , "</"
                    , name
                    , ">"
                    ]

        Comment comment_ ->
            "<!-- " ++ comment_ ++ " -->"



-- Text


leaf : (String -> a) -> (a -> a -> a) -> a -> Parser (Node a)
leaf convert combine empty =
    Parser.oneOf
        [ Parser.map convert (Parser.getChompedString (chompOneOrMore (\c -> c /= '<' && c /= '&')))
        , Parser.map convert characterReference
        ]
        |> oneOrMore "leaf element"
        |> Parser.map (Leaf << List.foldr combine empty)


characterReference : Parser String
characterReference =
    Parser.succeed identity
        |. Parser.chompIf ((==) '&')
        |= Parser.oneOf
            [ Parser.backtrackable namedCharacterReference
                |. chompSemicolon
            , Parser.backtrackable numericCharacterReference
                |. chompSemicolon
            , Parser.succeed "&"
            ]


namedCharacterReference : Parser String
namedCharacterReference =
    Parser.getChompedString (chompOneOrMore Char.isAlpha)
        |> Parser.map
            (\reference ->
                Dict.get reference NamedCharacterReferences.dict
                    |> Maybe.withDefault ("&" ++ reference ++ ";")
            )


numericCharacterReference : Parser String
numericCharacterReference =
    let
        codepoint =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.chompIf (\c -> c == 'x' || c == 'X')
                    |= hexadecimal
                , Parser.succeed identity
                    |. Parser.chompWhile ((==) '0')
                    |= Parser.int
                ]
    in
    Parser.succeed identity
        |. Parser.chompIf ((==) '#')
        |= Parser.map (Char.fromCode >> String.fromChar) codepoint



-- Element


element : (String -> a) -> (a -> a -> a) -> a -> Parser (Node a)
element convert combine init =
    Parser.succeed Tuple.pair
        |. Parser.chompIf ((==) '<')
        |= tagName
        |. Parser.chompWhile isSpaceCharacter
        |= tagAttributes
        |> Parser.andThen
            (\( name, attributes ) ->
                if isVoidElement name then
                    Parser.succeed (Element name attributes [])
                        |. Parser.oneOf
                            [ Parser.chompIf ((==) '/')
                            , Parser.succeed ()
                            ]
                        |. Parser.chompIf ((==) '>')

                else
                    Parser.succeed (Element name attributes)
                        |. Parser.chompIf ((==) '>')
                        |= many (Parser.backtrackable (node convert combine init))
                        |. closingTag name
            )


tagName : Parser String
tagName =
    Parser.getChompedString
        (Parser.chompIf Char.isAlphaNum
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '-')
        )
        |> Parser.map String.toLower


tagAttributes : Parser (List Attribute)
tagAttributes =
    many tagAttribute


tagAttribute : Parser Attribute
tagAttribute =
    Parser.succeed Tuple.pair
        |= tagAttributeName
        |. Parser.chompWhile isSpaceCharacter
        |= tagAttributeValue
        |. Parser.chompWhile isSpaceCharacter


tagAttributeName : Parser String
tagAttributeName =
    Parser.getChompedString (chompOneOrMore isTagAttributeCharacter)
        |> Parser.map String.toLower


tagAttributeValue : Parser String
tagAttributeValue =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.chompIf ((==) '=')
            |. Parser.chompWhile isSpaceCharacter
            |= Parser.oneOf
                [ tagAttributeUnquotedValue
                , tagAttributeQuotedValue '"'
                , tagAttributeQuotedValue '\''
                ]
        , Parser.succeed ""
        ]


tagAttributeUnquotedValue : Parser String
tagAttributeUnquotedValue =
    let
        isUnquotedValueChar c =
            not (isSpaceCharacter c) && c /= '"' && c /= '\'' && c /= '=' && c /= '<' && c /= '>' && c /= '`' && c /= '&'
    in
    Parser.oneOf
        [ chompOneOrMore isUnquotedValueChar
            |> Parser.getChompedString
        , characterReference
        ]
        |> oneOrMore "attribute value"
        |> Parser.map (String.join "")


tagAttributeQuotedValue : Char -> Parser String
tagAttributeQuotedValue quote =
    let
        isQuotedValueChar c =
            c /= quote && c /= '&'
    in
    Parser.succeed identity
        |. Parser.chompIf ((==) quote)
        |= (Parser.oneOf
                [ Parser.getChompedString (chompOneOrMore isQuotedValueChar)
                , characterReference
                ]
                |> many
                |> Parser.map (String.join "")
           )
        |. Parser.chompIf ((==) quote)


closingTag : String -> Parser ()
closingTag name =
    let
        chompName =
            chompOneOrMore (\c -> not (isSpaceCharacter c) && c /= '>')
                |> Parser.getChompedString
                |> Parser.andThen
                    (\closingName ->
                        if String.toLower closingName == name then
                            Parser.succeed ()

                        else
                            Parser.problem ("closing tag does not match opening tag: " ++ name)
                    )
    in
    Parser.chompIf ((==) '<')
        |. Parser.chompIf ((==) '/')
        |. chompName
        |. Parser.chompWhile isSpaceCharacter
        |. Parser.chompIf ((==) '>')



-- Comment


comment : Parser (Node a)
comment =
    Parser.succeed Comment
        |. Parser.token "<!"
        |. Parser.token "--"
        |= Parser.getChompedString (Parser.chompUntil "-->")
        |. Parser.token "-->"



-- Void elements


isVoidElement : String -> Bool
isVoidElement name =
    List.member name voidElements


voidElements : List String
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]



-- Character validators


isTagAttributeCharacter : Char -> Bool
isTagAttributeCharacter c =
    not (isSpaceCharacter c) && c /= '"' && c /= '\'' && c /= '>' && c /= '/' && c /= '='


isSpaceCharacter : Char -> Bool
isSpaceCharacter c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' || c == '\u{000C}' || c == '\u{00A0}'



-- Chomp


chompSemicolon : Parser ()
chompSemicolon =
    Parser.chompIf ((==) ';')


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore fn =
    Parser.chompIf fn
        |. Parser.chompWhile fn



-- Types


hexadecimal : Parser Int
hexadecimal =
    chompOneOrMore Char.isHexDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\hex ->
                case Hex.fromString (String.toLower hex) of
                    Ok value ->
                        Parser.succeed value

                    Err error ->
                        Parser.problem error
            )



-- Loops


many : Parser a -> Parser (List a)
many parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , Parser.succeed (Parser.Done (List.reverse list))
                ]
        )


oneOrMore : String -> Parser a -> Parser (List a)
oneOrMore type_ parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , if List.isEmpty list then
                    Parser.problem ("expecting at least one " ++ type_)

                  else
                    Parser.succeed (Parser.Done (List.reverse list))
                ]
        )
