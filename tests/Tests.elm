module Tests exposing (..)

import Expect
import Test exposing (..)


type alias Location =
    ( Int, Int )


type alias WorldInput =
    List String


type alias World =
    List (List WorldCell)


type WorldCell
    = EmptyCell
    | DotCell
    | WallCell


type Heading
    = East
    | South
    | West
    | North


worldCellToString : WorldCell -> String
worldCellToString cell =
    case cell of
        EmptyCell ->
            " "

        DotCell ->
            "."

        WallCell ->
            "x"


parseWorldRow : String -> List WorldCell
parseWorldRow row =
    row
        |> String.split ""
        |> List.map parseWorldCell


parseWorld : WorldInput -> World
parseWorld rows =
    rows
        |> List.map parseWorldRow


parseWorldCell : String -> WorldCell
parseWorldCell input =
    case input of
        "." ->
            DotCell

        "x" ->
            WallCell

        _ ->
            EmptyCell


getFromList : Int -> a -> List a -> a
getFromList index default list =
    List.drop (index % List.length list) list
        |> List.head
        |> Maybe.withDefault default


getWorldCell : Location -> World -> WorldCell
getWorldCell ( x, y ) world =
    getFromList y [] world
        |> getFromList x EmptyCell


locationDifference heading =
    case heading of
        East ->
            ( 1, 0 )

        South ->
            ( 0, 1 )

        West ->
            ( -1, 0 )

        North ->
            ( 0, -1 )


getAheadCell : Location -> Heading -> World -> WorldCell
getAheadCell ( x, y ) heading world =
    let
        ( dx, dy ) =
            locationDifference heading
    in
    getWorldCell ( x + dx, y + dy ) world


updatePacManLocation : World -> ( Location, Heading ) -> Location
updatePacManLocation world ( ( x, y ) as location, heading ) =
    let
        worldWidth =
            world
                |> List.head
                |> Maybe.map List.length
                |> Maybe.withDefault 1

        worldHeight =
            List.length world

        ( dx, dy ) =
            locationDifference heading

        cellAhead =
            getAheadCell location heading world
    in
    case cellAhead of
        WallCell ->
            ( x, y )

        _ ->
            ( (x + dx) % worldWidth, (y + dy) % worldHeight )


gameToStringList : ( Location, Heading ) -> World -> WorldInput
gameToStringList ( ( x, y ), heading ) world =
    let
        pacman =
            case heading of
                East ->
                    "<"

                _ ->
                    "V"
    in
    world
        |> List.indexedMap
            (\j cells ->
                cells
                    |> List.indexedMap
                        (\i cell ->
                            if i == x && j == y then
                                pacman
                            else
                                worldCellToString cell
                        )
                    |> String.join ""
            )


all : Test
all =
    let
        someWorld3x3 =
            parseWorld
                [ " .x"
                , ".x."
                , "..."
                ]

        emptyWorld3x3 =
            parseWorld
                [ "   "
                , "   "
                , "   "
                ]
    in
    describe "Pac-Man"
        [ describe "worldCellToString"
            [ testWorldCell EmptyCell " "
            , testWorldCell DotCell "."
            , testWorldCell WallCell "x"
            ]
        , describe "parseWorld"
            [ testParseWorld "empty 1x1" [ " " ] [ [ EmptyCell ] ]
            , testParseWorld "dot 1x1" [ "." ] [ [ DotCell ] ]
            , testParseWorld "wall 1x1" [ "x" ] [ [ WallCell ] ]
            , testParseWorld "empty 2x1"
                [ "  " ]
                [ [ EmptyCell, EmptyCell ] ]
            , testParseWorld "empty 1x2"
                [ " ", " " ]
                [ [ EmptyCell ], [ EmptyCell ] ]
            , testParseWorld "some 1x3"
                [ ".", "x", " " ]
                [ [ DotCell ], [ WallCell ], [ EmptyCell ] ]
            ]
        , describe "parseWorldCell"
            [ testParseWorldCell " " EmptyCell
            , testParseWorldCell "." DotCell
            , testParseWorldCell "x" WallCell
            ]
        , describe "getWorldCell"
            [ testGetWorldCell
                "(0, 0) in someWorld3x3"
                ( 0, 0 )
                someWorld3x3
                EmptyCell
            , testGetWorldCell
                "(1, 0) in someWorld3x3"
                ( 1, 0 )
                someWorld3x3
                DotCell
            , testGetWorldCell
                "(2, 0) in someWorld3x3"
                ( 2, 0 )
                someWorld3x3
                WallCell
            , testGetWorldCell
                "(0, 1) in someWorld3x3"
                ( 0, 1 )
                someWorld3x3
                DotCell
            , testGetWorldCell
                "(4, 0) in someWorld3x3"
                ( 4, 0 )
                someWorld3x3
                DotCell
            , testGetWorldCell
                "(-1, 0) in someWorld3x3"
                ( -1, 0 )
                someWorld3x3
                WallCell
            , testGetWorldCell
                "(0, 4) in someWorld3x3"
                ( 0, 4 )
                someWorld3x3
                DotCell
            ]
        , describe "getAheadCell" <|
            testGetAheadCells
                "someWorld3x3"
                someWorld3x3
                [ ( ( 0, 0 ), East, DotCell )
                , ( ( 1, 0 ), East, WallCell )
                , ( ( 2, 0 ), East, EmptyCell )
                , ( ( 0, 0 ), West, WallCell )
                , ( ( 0, 0 ), South, DotCell )
                , ( ( 0, 0 ), North, DotCell )
                ]
        , describe "updatePacMan"
            [ test "heading east from (0, 0) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 0, 0 ), East )
                        |> Expect.equal ( 1, 0 )
            , test "heading east from (1, 0) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 1, 0 ), East )
                        |> Expect.equal ( 2, 0 )
            , test "heading east from (2, 0) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 2, 0 ), East )
                        |> Expect.equal ( 0, 0 )
            , test "heading west from (0, 0) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 0, 0 ), West )
                        |> Expect.equal ( 2, 0 )
            , test "heading south from (0, 0) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 0, 0 ), South )
                        |> Expect.equal ( 0, 1 )
            , test "heading south from (0, 2) in an empty world" <|
                \_ ->
                    updatePacManLocation emptyWorld3x3 ( ( 0, 2 ), South )
                        |> Expect.equal ( 0, 0 )
            , test "heading east from (1, 0) in some world" <|
                \_ ->
                    updatePacManLocation someWorld3x3 ( ( 1, 0 ), East )
                        |> Expect.equal ( 1, 0 )
            ]
        , describe "gameToString"
            [ test "someWorld with Pac-Man at (0, 0) heading north" <|
                \_ ->
                    gameToStringList ( ( 0, 0 ), North ) someWorld3x3
                        |> Expect.equal
                            [ "V.x"
                            , ".x."
                            , "..."
                            ]
            , test "someWorld with Pac-Man at (0, 0) heading east" <|
                \_ ->
                    gameToStringList ( ( 0, 0 ), East ) someWorld3x3
                        |> Expect.equal
                            [ "<.x"
                            , ".x."
                            , "..."
                            ]
            , test "someWorld with Pac-Man at (0, 1) heading north" <|
                \_ ->
                    gameToStringList ( ( 0, 1 ), North ) someWorld3x3
                        |> Expect.equal
                            [ " .x"
                            , "Vx."
                            , "..."
                            ]
            ]
        ]


testGetAheadCells worldName world testCases =
    let
        x =
            \( location, heading, expected ) ->
                test (toString heading ++ " from " ++ toString location ++ " in " ++ worldName) <|
                    \_ ->
                        world
                            |> getAheadCell location heading
                            |> Expect.equal expected
    in
    List.map x testCases


testGetWorldCell name location world expected =
    test name <|
        \_ ->
            getWorldCell location world
                |> Expect.equal expected


testParseWorldCell : String -> WorldCell -> Test
testParseWorldCell input expected =
    test ("input: " ++ input) <|
        \_ ->
            input
                |> parseWorldCell
                |> Expect.equal expected


testParseWorld : String -> WorldInput -> World -> Test
testParseWorld name input expected =
    test name <|
        \_ ->
            input
                |> parseWorld
                |> Expect.equal expected


testWorldCell : WorldCell -> String -> Test
testWorldCell cell string =
    test (toString cell) <|
        \_ ->
            cell
                |> worldCellToString
                |> Expect.equal string
