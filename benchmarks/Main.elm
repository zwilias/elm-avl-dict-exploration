module Main exposing (main)

import Benchmark exposing (Benchmark, withRuntime)
import Html exposing (Html)
import Benchmark.Reporting
    exposing
        ( Report(..)
        , Status(..)
        , encoder
        , fromBenchmark
        )
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Http
import Process
import Debug
import Html.Attributes as A
import Html.Events as E
import Random.Deletion
import Random.Insertion
import Serial.Deletion
import Serial.Insertion
import Serial.Retrieval
import Dict exposing (Dict)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Progress as Progress
import Bootstrap.Card as Card
import Html.Lazy exposing (lazy, lazy2)
import Random.Pcg as Random exposing (step, initialSeed)
import Time
import Shuffle exposing (shuffle)


type RunPhase
    = Prepare
    | InProgress
    | Finished


type alias Model =
    { currentBenchmark : Maybe BenchmarkDescriptor
    , browserInfo : BrowserInfo
    , toRun : List BenchmarkDescriptor
    , runPhase : RunPhase
    , failedRequests : List Submission
    }


type alias BenchmarkDescriptor =
    { name : String
    , size : Int
    , benchmark : Benchmark
    }


breakForRender : Task x a -> Task x a
breakForRender task =
    Task.andThen (\_ -> task) (Process.sleep 50)


next : BenchmarkDescriptor -> Maybe (Cmd Msg)
next descriptor =
    Benchmark.step descriptor.benchmark
        |> Maybe.map breakForRender
        |> Maybe.map
            (Task.perform
                (\bench -> Update { descriptor | benchmark = bench })
            )


applyList : List (a -> b) -> List a -> List b
applyList fList argList =
    List.map (\f -> List.map f argList) fList
        |> List.concat


allBenches : List BenchmarkDescriptor
allBenches =
    let
        descriptorFactory :
            ( String, Int -> Benchmark )
            -> Int
            -> BenchmarkDescriptor
        descriptorFactory ( name, factory ) size =
            { name = name
            , size = size
            , benchmark = factory size
            }

        benchFactories : Dict String (Int -> Benchmark)
        benchFactories =
            Dict.fromList
                [ ( "serial insertion", Serial.Insertion.suiteOfSize )
                , ( "serial deletion", Serial.Deletion.suiteOfSize )
                , ( "serial retrieval", Serial.Retrieval.suiteOfSize )
                , ( "random deletion", Random.Deletion.suiteOfSize )
                , ( "random insertion", Random.Insertion.suiteOfSize )
                ]

        sizes : List Int
        sizes =
            [ 1, 10, 100, 1000, 10000 ]
    in
        applyList
            (Dict.toList benchFactories |> List.map descriptorFactory)
            (sizes)
            |> List.map
                (\descriptor ->
                    { descriptor
                        | benchmark =
                            descriptor.benchmark
                                |> withRuntime (1 * Time.second)
                    }
                )


type alias Flags =
    { browserInfo : SafeBrowserInfo
    , seed : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.seed
    in
        { currentBenchmark = Nothing
        , browserInfo =
            flags.browserInfo
                |> fromSafeBrowserInfo
        , toRun =
            allBenches
                |> shuffle
                |> flip Random.step initialSeed
                |> Tuple.first
        , runPhase = Prepare
        , failedRequests = []
        }
            ! []


type alias Submission =
    { description : String
    , tries : Int
    , request : Http.Request String
    , result : Result Http.Error String
    }


type Msg
    = Update BenchmarkDescriptor
    | SubmissionResult Submission
    | ReSubmissionResult Submission
    | Start
    | Retry Submission
    | UpdateBrowserInfo UpdateBrowserInfo


type UpdateBrowserInfo
    = UpdateBrowser String
    | UpdateVersion String
    | UpdateOS String
    | UpdateOSVersion String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Retry submission ->
            { model
                | failedRequests =
                    List.filter ((/=) submission) model.failedRequests
            }
                ! [ retry submission ]

        Update descriptor ->
            case next descriptor of
                Just cmd ->
                    { model
                        | currentBenchmark = Just descriptor
                    }
                        ! [ cmd ]

                Nothing ->
                    { model
                        | currentBenchmark = Nothing
                    }
                        ! [ submitBench model.browserInfo descriptor ]

        ReSubmissionResult ({ result } as submission) ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "failed" err
                    in
                        { model
                            | failedRequests =
                                submission :: model.failedRequests
                        }
                            ! []

                Ok _ ->
                    model ! []

        SubmissionResult ({ result } as submission) ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "failed" err
                    in
                        update Start
                            ({ model
                                | failedRequests =
                                    submission :: model.failedRequests
                             }
                            )

                Ok _ ->
                    update Start model

        Start ->
            case model.toRun of
                [] ->
                    { model | runPhase = Finished } ! []

                head :: tail ->
                    let
                        newModel =
                            { model
                                | currentBenchmark = Just head
                                , toRun = tail
                                , runPhase = InProgress
                            }
                    in
                        update (Update head) newModel

        UpdateBrowserInfo info ->
            { model
                | browserInfo = updateBrowserInfo info model.browserInfo
            }
                ! []


updateBrowserInfo : UpdateBrowserInfo -> BrowserInfo -> BrowserInfo
updateBrowserInfo msg browserInfo =
    case msg of
        UpdateBrowser browser ->
            { browserInfo | name = browser }

        UpdateVersion version ->
            { browserInfo | version = version }

        UpdateOS os ->
            { browserInfo | os = os }

        UpdateOSVersion osVersion ->
            { browserInfo | osVersion = osVersion }


retry : Submission -> Cmd Msg
retry submission =
    Http.send
        (ReSubmissionResult
            << Submission
                submission.description
                (submission.tries + 1)
                submission.request
        )
        submission.request


submitBench : BrowserInfo -> BenchmarkDescriptor -> Cmd Msg
submitBench browserInfo descriptor =
    let
        body =
            encodeSubmission browserInfo descriptor
                |> Http.jsonBody
    in
        Http.post
            "https://elm-bench.herokuapp.com/"
            body
            Decode.string
            |> (\request ->
                    Http.send
                        (SubmissionResult
                            << Submission
                                (descriptorToString descriptor)
                                0
                                request
                        )
                        request
               )


encodeBrowserInfo : BrowserInfo -> Encode.Value
encodeBrowserInfo browserInfo =
    Encode.object
        [ ( "browser", Encode.string browserInfo.name )
        , ( "version", Encode.string browserInfo.version )
        , ( "os", Encode.string browserInfo.os )
        , ( "osVersion", Encode.string browserInfo.osVersion )
        ]


encodeSubmission : BrowserInfo -> BenchmarkDescriptor -> Encode.Value
encodeSubmission browserInfo descriptor =
    Encode.object
        [ ( "browserInfo", encodeBrowserInfo browserInfo )
        , ( "type", Encode.string descriptor.name )
        , ( "size", Encode.int descriptor.size )
        , ( "benchmark", encoder <| fromBenchmark descriptor.benchmark )
        ]


view : Model -> Html Msg
view model =
    Grid.container
        [ A.class "main" ]
        [ Grid.row []
            [ Grid.col []
                [ Html.h1 [] [ Html.text "Dict vs Dict.AVL benchmarks" ] ]
            ]
        , Grid.row []
            [ Grid.col []
                [ lazy2
                    renderBrowserForm
                    model.runPhase
                    model.browserInfo
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ lazy2 renderFailed model.runPhase model.failedRequests ]
            ]
        , Grid.row []
            [ Grid.col [] [ lazy renderCurrent model.currentBenchmark ] ]
        , Grid.row []
            [ Grid.col [] [ lazy renderNext model.toRun ] ]
        ]


renderFailed : RunPhase -> List Submission -> Html Msg
renderFailed phase submissionList =
    let
        description =
            case phase of
                Finished ->
                    "Alright, done running benchmarks. You can now attempt to retry submission..."

                _ ->
                    "I failed to submit some benchmarks. Let's wait for the other benchmarks to finish, and then you can retry them once you have access to the internet again."

        button submission =
            case phase of
                Finished ->
                    [ Button.linkButton
                        [ Button.small
                        , Button.warning
                        , Button.attrs <|
                            [ E.onClick <| Retry submission ]
                        ]
                        [ Html.text "Retry" ]
                    ]

                _ ->
                    []
    in
        case submissionList of
            [] ->
                Html.div [] []

            _ ->
                Card.config [ Card.warning ]
                    |> Card.block []
                        [ Card.text []
                            [ Html.text description
                            , Html.ul [] <|
                                List.map
                                    (\submission ->
                                        Html.li [] <|
                                            (Html.text submission.description)
                                                :: button submission
                                    )
                                    submissionList
                            ]
                        ]
                    |> Card.view


renderBrowserForm : RunPhase -> BrowserInfo -> Html Msg
renderBrowserForm phase browserInfo =
    case phase of
        Prepare ->
            let
                myForm =
                    Grid.container []
                        [ Form.form []
                            [ Form.row []
                                [ Form.col []
                                    [ Html.text "Please adjust settings as required, and hit the button to start the benchmark. Note that the benchmarks might run for quite a while..." ]
                                ]
                            , Form.row []
                                [ Form.colLabel
                                    [ Col.sm1
                                    , Col.attrs [ A.for "browser" ]
                                    ]
                                    [ Html.text "Browser" ]
                                , Form.col [ Col.sm5 ]
                                    [ Input.text
                                        [ Input.value browserInfo.name
                                        , Input.id "browser"
                                        , Input.onInput <|
                                            UpdateBrowserInfo
                                                << UpdateBrowser
                                        ]
                                    ]
                                , Form.colLabel
                                    [ Col.sm1
                                    , Col.attrs [ A.for "version" ]
                                    ]
                                    [ Html.text "Version" ]
                                , Form.col [ Col.sm5 ]
                                    [ Input.text
                                        [ Input.value browserInfo.version
                                        , Input.id "version"
                                        , Input.onInput <|
                                            UpdateBrowserInfo
                                                << UpdateVersion
                                        ]
                                    ]
                                ]
                            , Form.row []
                                [ Form.colLabel
                                    [ Col.sm1
                                    , Col.attrs [ A.for "os" ]
                                    ]
                                    [ Html.text "OS" ]
                                , Form.col [ Col.sm5 ]
                                    [ Input.text
                                        [ Input.value browserInfo.os
                                        , Input.id "os"
                                        , Input.onInput <|
                                            UpdateBrowserInfo
                                                << UpdateOS
                                        ]
                                    ]
                                , Form.colLabel
                                    [ Col.sm1
                                    , Col.attrs [ A.for "osversion" ]
                                    ]
                                    [ Html.text "Version" ]
                                , Form.col [ Col.sm5 ]
                                    [ Input.text
                                        [ Input.value browserInfo.osVersion
                                        , Input.id "osversion"
                                        , Input.onInput <|
                                            UpdateBrowserInfo
                                                << UpdateOSVersion
                                        ]
                                    ]
                                ]
                            , Form.row
                                [ Row.centerMd ]
                                [ Form.col [ Col.sm2 ]
                                    [ Button.linkButton
                                        [ Button.primary
                                        , Button.attrs
                                            [ E.onClick Start
                                            , A.href "#"
                                            ]
                                        ]
                                        [ Html.text "Start benching" ]
                                    ]
                                ]
                            , Form.row
                                []
                                [ Form.col []
                                    [ Html.text "I'm only asking for this information so that I know how the dict is performing on any given platform. If I note a serious regression on any given platform, that'll help me pin down the problem, and possibly optimize it away." ]
                                ]
                            ]
                        ]
            in
                Card.config []
                    |> Card.block []
                        [ Card.text [] [ myForm ] ]
                    |> Card.view

        _ ->
            Card.config [ Card.info ]
                |> Card.block []
                    [ Card.text [] [ Html.text "Alright, gotcha. Running the benchmarks now!" ] ]
                |> Card.view


renderCurrent : Maybe BenchmarkDescriptor -> Html Msg
renderCurrent benchmarkDescriptorMaybe =
    case benchmarkDescriptorMaybe of
        Nothing ->
            Card.config [ Card.warning ]
                |> Card.block []
                    [ Card.text [] [ Html.text "No benchmark currently running..." ] ]
                |> Card.view

        Just descriptor ->
            Card.config [ Card.outlineSuccess ]
                |> Card.block []
                    [ Card.text []
                        [ Html.text "Current benchmark: "
                        , renderDescriptor descriptor
                        , Html.br [] []
                        , renderRunningBenchmark descriptor.benchmark
                        ]
                    ]
                |> Card.view


renderNext : List BenchmarkDescriptor -> Html Msg
renderNext benchmarkDescriptorList =
    Card.config []
        |> Card.block []
            [ Card.text []
                [ case benchmarkDescriptorList of
                    [] ->
                        Html.text "No more benchmarks queued!"

                    _ ->
                        Html.div []
                            [ Html.text "Alright, so I still need to run some benchmarks. They're shuffled, so that - even if people \"accidently\" close their tab and stop mid-benchmark, I still get a fair sample, over time."
                            , Html.ul
                                [ A.style [ ( "column-count", "3" ) ] ]
                              <|
                                (List.map
                                    (\descriptor ->
                                        Html.li []
                                            [ renderDescriptor descriptor ]
                                    )
                                    benchmarkDescriptorList
                                )
                            ]
                ]
            ]
        |> Card.view


descriptorToString : BenchmarkDescriptor -> String
descriptorToString benchmarkDescriptor =
    benchmarkDescriptor.name
        ++ " of size "
        ++ (toString benchmarkDescriptor.size)


renderDescriptor : BenchmarkDescriptor -> Html Msg
renderDescriptor benchmarkDescriptor =
    Html.text <|
        descriptorToString benchmarkDescriptor


type BenchState
    = Sizing
    | Running Int Int
    | Done Int


state : Report -> BenchState
state report =
    case report of
        Benchmark.Reporting.Benchmark _ status ->
            case status of
                Pending time sampleSize samples ->
                    Running (round <| List.sum samples) (round time)

                ToSize _ ->
                    Sizing

                Failure _ ->
                    Done 0

                Success stats ->
                    List.sum stats.samples
                        |> round
                        |> Done

        Benchmark.Reporting.Group _ benches ->
            List.map state benches
                |> List.foldl (combineState) (Done 0)

        Benchmark.Reporting.Compare _ left right ->
            combineState (state left) (state right)


combineState : BenchState -> BenchState -> BenchState
combineState left right =
    case ( left, right ) of
        ( _, Sizing ) ->
            Sizing

        ( Sizing, _ ) ->
            Sizing

        ( Running lspent ltotal, Running rspent rtotal ) ->
            Running (lspent + rspent) (ltotal + rtotal)

        ( Running spent ltotal, Done total ) ->
            Running (spent + total) (ltotal + total)

        ( Done ltotal, Running spent total ) ->
            Running (spent + total) (ltotal + total)

        ( Done l, Done r ) ->
            Done <| l + r


renderRunningBenchmark : Benchmark -> Html Msg
renderRunningBenchmark benchmark =
    case state <| fromBenchmark benchmark of
        Sizing ->
            Html.text "Figuring out how many iterations I should do..."

        Running spent total ->
            Progress.progress [ Progress.value ((spent * 100 // total)) ]

        Done _ ->
            Html.text "Submitting benchmark..."


type alias BrowserInfo =
    { name : String
    , version : String
    , os : String
    , osVersion : String
    }


type alias SafeBrowserInfo =
    { name : Maybe String
    , version : Maybe String
    , os : Maybe String
    , osVersion : Maybe String
    }


fromSafeBrowserInfo : SafeBrowserInfo -> BrowserInfo
fromSafeBrowserInfo safeBrowserInfo =
    BrowserInfo
        (Maybe.withDefault "" safeBrowserInfo.name)
        (Maybe.withDefault "" safeBrowserInfo.version)
        (Maybe.withDefault "" safeBrowserInfo.os)
        (Maybe.withDefault "" safeBrowserInfo.osVersion)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
