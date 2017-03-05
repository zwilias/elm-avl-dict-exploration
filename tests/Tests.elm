module Tests exposing (..)

import Test exposing (..)
import Tree.AVLTest
import Dict.AVLTest


all : Test
all =
    describe "All tests"
        [ Tree.AVLTest.all
        , Dict.AVLTest.tests
        ]
