module Main exposing (suite)

import Expect exposing (Expectation)
import Html.Parser exposing (Node(..))
import Test exposing (Test, describe, test)
import Json.Decode as Decode exposing (Decoder)


testParseAll : String -> List (Node String) -> (() -> Expectation)
testParseAll s astList =
    \_ ->
        Expect.equal (Ok astList) (Html.Parser.run s)


testParse : String -> Node String -> (() -> Expectation)
testParse s ast =
    testParseAll s [ ast ]


testParseComplex : (String -> a)
                 -> (a -> a -> a)
                 -> a
                 -> String
                 -> Node a
                 -> (() -> Expectation)
testParseComplex convert combine empty s ast =
    \_ ->
        Expect.equal (Ok [ ast ]) (Html.Parser.runComplex convert combine empty s)

testError : String -> (() -> Expectation)
testError s =
    \_ ->
        let
            failed =
                case Html.Parser.run s of
                    Ok _ ->
                        False

                    Err _ ->
                        True
        in
        Expect.true s failed


textNodeTests : Test
textNodeTests =
    describe "TextNode"
        [ test "empty" (testParseAll "" [])
        , test "space" (testParse " " (Leaf " "))
        , test "basic1" (testParse "1" (Leaf "1"))
        , test "basic2" (testParse "a" (Leaf "a"))
        , test "basic3" (testParse "1a" (Leaf "1a"))
        , test "basic4" (testParse "^" (Leaf "^"))
        , test "decode1" (testParse "&" (Leaf "&"))
        , test "decode2" (testParse "&amp;" (Leaf "&"))
        , test "decode3" (testParse "&lt;" (Leaf "<"))
        , test "decode4" (testParse "&gt;" (Leaf ">"))
        , test "decode6" (testParse "&apos;" (Leaf "'"))
        , test "decode7" (testParse "&#38;" (Leaf "&"))
        , test "decode8" (testParse "&#x26;" (Leaf "&"))
        , test "decode9" (testParse "&#x3E;" (Leaf ">"))
        , test "decodeA" (testParse "&#383;" (Leaf "ſ"))
        , test "decodeB" (testParse "&nbsp;" (Leaf "\u{00A0}"))
        , test "decodeC" (testParse "&nbsp;&nbsp;" (Leaf "\u{00A0}\u{00A0}"))
        , test "decodeD" (testParse "a&nbsp;b" (Leaf "a\u{00A0}b"))
        , test "decodeE" (testParse "a&nbsp;&nbsp;b" (Leaf "a\u{00A0}\u{00A0}b"))
        , test "decodeF" (testParse """<img alt="&lt;">""" (Element "img" [ ( "alt", "<" ) ] []))
        , test "decodeG" (testParse "&#0038;" (Leaf "&"))
        ]

type alias ExampleTestType =
    { name : String
    , age : Int
    }

complexObjTests : Test
complexObjTests =
    let
        complex : String -> Result Decode.Error ExampleTestType
        complex = Decode.decodeString (Decode.map2
                                            ExampleTestType
                                            (Decode.field "name" Decode.string)
                                            (Decode.field "age" Decode.int))
        convert : String -> ExampleTestType
        convert str =
            case complex str of
                Ok (val) -> val
                Err _ -> ExampleTestType "" 0

        combine : ExampleTestType -> ExampleTestType -> ExampleTestType
        combine a b =
            { a | name = a.name ++ b.name,
                  age = a.age + b.age }

        empty : ExampleTestType
        empty = { name = "", age = 0 }

        complexTest : String -> Node ExampleTestType -> (() -> Expectation)
        complexTest = testParseComplex convert combine empty

    in
    describe "ComplexNode"
        [ test "leaf" (complexTest "{\"name\": \"aname\", \"age\": 5}"
                           (Leaf {name = "aname", age = 5}))
        , test "basic" (complexTest "<a>{\"name\": \"aname\", \"age\": 1}</a>"
                            (Element "a" [] [ Leaf {name = "aname", age = 1} ]))
        , test "failure" (complexTest "<a> </a>"
                              (Element "a" [] [ Leaf empty ]))
        ]


nodeTests : Test
nodeTests =
    describe "Node"
        [ test "basic1" (testParse "<a></a>" (Element "a" [] []))
        , test "basic2" (testParse "<a></a >" (Element "a" [] []))
        , test "basic3" (testParse "<A></A >" (Element "a" [] []))
        , test "basic4" (testParseAll " <a></a> " [ Leaf " ", Element "a" [] [], Leaf " " ])
        , test "basic5" (testParseAll "a<a></a>b" [ Leaf "a", Element "a" [] [], Leaf "b" ])
        , test "basic6" (testParse "<A></A>" (Element "a" [] []))
        , test "basic7" (testParse "<a>a</a>" (Element "a" [] [ Leaf "a" ]))
        , test "basic8" (testParse "<a> a </a>" (Element "a" [] [ Leaf " a " ]))
        , test "basic10" (testParse "<br>" (Element "br" [] []))
        , test "basic11" (testParse "<a><a></a></a>" (Element "a" [] [ Element "a" [] [] ]))
        , test "basic12" (testParse "<a> <a> </a> </a>" (Element "a" [] [ Leaf " ", Element "a" [] [ Leaf " " ], Leaf " " ]))
        , test "basic13" (testParse "<a> <br> </a>" (Element "a" [] [ Leaf " ", Element "br" [] [], Leaf " " ]))
        , test "basic14" (testParse "<a><a></a><a></a></a>" (Element "a" [] [ Element "a" [] [], Element "a" [] [] ]))
        , test "basic15" (testParse "<a><a><a></a></a></a>" (Element "a" [] [ Element "a" [] [ Element "a" [] [] ] ]))
        , test "basic16" (testParse "<a><a></a><b></b></a>" (Element "a" [] [ Element "a" [] [], Element "b" [] [] ]))
        , test "basic17" (testParse "<h1></h1>" (Element "h1" [] []))
        , test "start-only-tag1" (testParse "<br>" (Element "br" [] []))
        , test "start-only-tag2" (testParse "<BR>" (Element "br" [] []))
        , test "start-only-tag3" (testParse "<br >" (Element "br" [] []))
        , test "start-only-tag4" (testParse "<BR >" (Element "br" [] []))
        , test "start-only-tag5" (testParse "<a> <br> </a>" (Element "a" [] [ Leaf " ", Element "br" [] [], Leaf " " ]))
        , test "start-only-tag6" (testParse "<a><br><br></a>" (Element "a" [] [ Element "br" [] [], Element "br" [] [] ]))
        , test "start-only-tag7" (testParse "<a><br><img><hr><meta></a>" (Element "a" [] [ Element "br" [] [], Element "img" [] [], Element "hr" [] [], Element "meta" [] [] ]))
        , test "start-only-tag8" (testParse "<a>foo<br>bar</a>" (Element "a" [] [ Leaf "foo", Element "br" [] [], Leaf "bar" ]))
        , test "self-closing-tag1" (testParse "<br/>" (Element "br" [] []))
        , test "self-closing-tag2" (testParse "<br />" (Element "br" [] []))
        , test "self-closing-tag3" (testParse "<link href=\"something\" rel=\"something else\"/>" (Element "link" [ ( "href", "something" ), ( "rel", "something else" ) ] []))
        , test "web-component-tag" (testParse "<a-web-component></a-web-component>" (Element "a-web-component" [] []))
        ]


nodeToStringTests : Test
nodeToStringTests =
    describe "nodeToString"
        [ test "simple link" <|
            \_ ->
                Element "a" [ ( "href", "https://elm-lang.org" ) ] [ Leaf "Elm" ]
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "<a href=\"https://elm-lang.org\">Elm</a>"
        , test "container" <|
            \_ ->
                Element "div"
                    []
                    [ Element "p" [] [ Leaf "Hello," ]
                    , Element "p" [] [ Leaf "World!" ]
                    ]
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "<div><p>Hello,</p><p>World!</p></div>"
        , test "multiple attributes" <|
            \_ ->
                Element "a"
                    [ ( "href", "https://elm-lang.org" )
                    , ( "alt", "Elm website" )
                    ]
                    [ Leaf "Elm" ]
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "<a href=\"https://elm-lang.org\" alt=\"Elm website\">Elm</a>"
        , test "void element" <|
            \_ ->
                Element "br" [] [ Element "a" [] [ Leaf "should be ignored" ] ]
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "<br>"
        , test "comment" <|
            \_ ->
                Comment "This is a comment"
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "<!-- This is a comment -->"
        , test "text" <|
            \_ ->
                Leaf "Hello, world!"
                    |> Html.Parser.nodeToString identity
                    |> Expect.equal "Hello, world!"
        , test "number" <|
            \_ ->
                Leaf 5
                    |> Html.Parser.nodeToString String.fromFloat
                    |> Expect.equal "5"
        ]


scriptTests : Test
scriptTests =
    describe "Script"
        [ test "script1" (testParse """<script></script>""" (Element "script" [] []))
        , test "script2" (testParse """<SCRIPT></SCRIPT>""" (Element "script" [] []))
        , test "script3" (testParse """<script src="script.js">foo</script>""" (Element "script" [ ( "src", "script.js" ) ] [ Leaf "foo" ]))
        , test "script4" (testParse """<script>var a = 0 < 1; b = 1 > 0;</script>""" (Element "script" [] [ Leaf "var a = 0 < 1; b = 1 > 0;" ]))
        , test "script5" (testParse """<script><!----></script>""" (Element "script" [] [ Comment "" ]))
        , test "script6" (testParse """<script>a<!--</script><script>-->b</script>""" (Element "script" [] [ Leaf "a", Comment "</script><script>", Leaf "b" ]))
        , test "style" (testParse """<style>a<!--</style><style>-->b</style>""" (Element "style" [] [ Leaf "a", Comment "</style><style>", Leaf "b" ]))
        ]


commentTests : Test
commentTests =
    describe "Comment"
        [ test "basic1" (testParse """<!---->""" (Comment ""))
        , test "basic2" (testParse """<!--<div></div>-->""" (Comment "<div></div>"))
        , test "basic3" (testParse """<div><!--</div>--></div>""" (Element "div" [] [ Comment "</div>" ]))
        , test "basic4" (testParse """<!--<!---->""" (Comment "<!--"))
        , test "basic5" (testParse """<!--foo\t\u{000D}
        -->""" (Comment "foo\t\u{000D}\n        "))
        ]


attributeTests : Test
attributeTests =
    describe "Attribute"
        [ test "basic1" (testParse """<a href="example.com"></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic2" (testParse """<a href='example.com'></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic3" (testParse """<a href=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic4" (testParse """<a HREF=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic5" (testParse """<a href=bare></a>""" (Element "a" [ ( "href", "bare" ) ] []))
        , test "basic6" (testParse """<a href="example.com?a=b&amp;c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic7" (testParse """<a href="example.com?a=b&c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic8" (testParse """<input max=100 min = 10.5>""" (Element "input" [ ( "max", "100" ), ( "min", "10.5" ) ] []))
        , test "basic9" (testParse """<input disabled>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic10" (testParse """<input DISABLED>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic11" (testParse """<meta http-equiv=Content-Type>""" (Element "meta" [ ( "http-equiv", "Content-Type" ) ] []))
        , test "basic12" (testParse """<input data-foo2="a">""" (Element "input" [ ( "data-foo2", "a" ) ] []))
        , test "basic13" (testParse """<html xmlns:v="urn:schemas-microsoft-com:vml"></html>""" (Element "html" [ ( "xmlns:v", "urn:schemas-microsoft-com:vml" ) ] []))
        , test "basic14" (testParse """<link rel=stylesheet
        href="">""" (Element "link" [ ( "rel", "stylesheet" ), ( "href", "" ) ] []))

        -- Invalid attribute names shouldn't be parsed: https://github.com/elm/html/issues/46
        , test "invalid character" (testParse """<p\u{00A0} ></p>""" (Element "p" [] []))
        ]


errorTests : Test
errorTests =
    describe "Errors"
        [ test "invalid closing tag" (testError "<a><br></p>")
        , test "invalid tag name" (testError "<-></->")
        ]


suite : Test
suite =
    describe "HtmlParser"
        [ textNodeTests
        , complexObjTests
        , nodeTests
        , nodeToStringTests
        , commentTests
        , attributeTests
        , errorTests

        --, scriptTests
        ]
