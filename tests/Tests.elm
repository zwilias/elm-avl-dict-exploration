module Tests exposing (..)

import Test exposing (..)
import Dict.AVLTest


all : Test
all =
    describe "All tests"
        [ Dict.AVLTest.tests
        ]
