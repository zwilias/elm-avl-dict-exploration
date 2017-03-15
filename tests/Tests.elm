module Tests exposing (..)

import Test exposing (..)
import Dict.AVLTest
import Dict.BalanceTest
import Doc.Tests


all : Test
all =
    describe "All tests"
        [ Dict.AVLTest.tests
        , Dict.BalanceTest.tests
        , Doc.Tests.all
        ]
