module Tests exposing (..)

import Expect
import Test exposing (..)


all : Test
all =
    describe "Pac-Man"
        [ describe "example"
            [ test "test" <|
                \_ ->
                    Expect.equal 3 (1 + 2)
            ]
        ]
