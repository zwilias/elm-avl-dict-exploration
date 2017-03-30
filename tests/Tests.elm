module Tests exposing (..)

import Test exposing (..)
import Dict.AVLTest
import Dict.BalanceTest
import Doc.Tests
import Dict.Quick


all : Test
all =
    describe "All tests"
        [ Dict.AVLTest.tests
        , Dict.BalanceTest.tests
        , Doc.Tests.all
        , Dict.Quick.tests
        ]
