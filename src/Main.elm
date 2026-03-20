module Main exposing (main, lzwCompress, lzwDecompress)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String.UTF8 as UTF8


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String }


init : Model
init =
    { input = "ABABABAB" }



-- UPDATE


type Msg
    = InputChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged s ->
            { model | input = s }



-- LZW COMPRESSION
-- Returns (encoded codes, final dictionary)


lzwCompress : List Int -> ( List Int, Dict Int (List Int) )
lzwCompress bytes =
    -- Initialize dictionary with single-byte entries 1..256
    let
        initDict : Dict Int (List Int)
        initDict =
            List.foldl
                (\b d -> Dict.insert (b + 1) [ b ] d)
                Dict.empty
                (List.range 0 255)

        -- Build reverse lookup: List Int -> code
        initRevDict : Dict (List Int) Int
        initRevDict =
            List.foldl
                (\b d -> Dict.insert [ b ] (b + 1) d)
                Dict.empty
                (List.range 0 255)
    in
    case bytes of
        [] ->
            ( [], initDict )

        first :: rest ->
            let
                result =
                    List.foldl
                        (\byte state ->
                            let
                                newW =
                                    state.w ++ [ byte ]
                            in
                            case Dict.get newW state.revDict of
                                Just _ ->
                                    { state | w = newW }

                                Nothing ->
                                    let
                                        newCode =
                                            state.nextCode

                                        newDict =
                                            Dict.insert newCode newW state.dict

                                        newRevDict =
                                            Dict.insert newW newCode state.revDict

                                        outputCode =
                                            case Dict.get state.w state.revDict of
                                                Just c ->
                                                    c

                                                Nothing ->
                                                    0
                                    in
                                    { state
                                        | w = [ byte ]
                                        , dict = newDict
                                        , revDict = newRevDict
                                        , nextCode = newCode + 1
                                        , output = state.output ++ [ outputCode ]
                                    }
                        )
                        { w = [ first ]
                        , dict = initDict
                        , revDict = initRevDict
                        , nextCode = 257
                        , output = []
                        }
                        rest

                finalOutput =
                    case Dict.get result.w result.revDict of
                        Just c ->
                            result.output ++ [ c ]

                        Nothing ->
                            result.output
            in
            ( finalOutput, result.dict )



-- LZW DECOMPRESSION
-- Inverts lzwCompress: given a list of 9-bit codes, returns the original bytes.
-- Returns Nothing only if the code stream is invalid (should never happen for
-- output produced by lzwCompress).


lzwDecompress : List Int -> Maybe (List Int)
lzwDecompress codes =
    let
        initDict : Dict Int (List Int)
        initDict =
            List.foldl
                (\b d -> Dict.insert (b + 1) [ b ] d)
                Dict.empty
                (List.range 0 255)
    in
    case codes of
        [] ->
            Just []

        first :: rest ->
            case Dict.get first initDict of
                Nothing ->
                    Nothing

                Just firstEntry ->
                    let
                        result =
                            List.foldl
                                (\code state ->
                                    case state.err of
                                        Just _ ->
                                            state

                                        Nothing ->
                                            let
                                                maybeEntry =
                                                    case Dict.get code state.dict of
                                                        Just e ->
                                                            Just e

                                                        Nothing ->
                                                            -- Special case: code equals the next code to be
                                                            -- added. Entry is prev ++ [prev[0]].
                                                            if code == state.nextCode then
                                                                case state.prev of
                                                                    [] ->
                                                                        Nothing

                                                                    p :: _ ->
                                                                        Just (state.prev ++ [ p ])

                                                            else
                                                                Nothing
                                            in
                                            case maybeEntry of
                                                Nothing ->
                                                    { state | err = Just "invalid code" }

                                                Just entry ->
                                                    let
                                                        newEntry =
                                                            state.prev ++ List.take 1 entry

                                                        newDict =
                                                            Dict.insert state.nextCode newEntry state.dict
                                                    in
                                                    { state
                                                        | prev = entry
                                                        , dict = newDict
                                                        , nextCode = state.nextCode + 1
                                                        , output = state.output ++ entry
                                                        , err = Nothing
                                                    }
                                )
                                { prev = firstEntry
                                , dict = initDict
                                , nextCode = 257
                                , output = firstEntry
                                , err = Nothing
                                }
                                rest
                    in
                    case result.err of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just result.output



-- VIEW HELPERS


intToBytes : String -> List Int
intToBytes s =
    UTF8.toBytes s


bytesToBitCount : List Int -> Int
bytesToBitCount bytes =
    List.length bytes * 8


codesToBitCount : List Int -> Int
codesToBitCount codes =
    List.length codes * 9


formatList : List Int -> String
formatList lst =
    String.join ", " (List.map String.fromInt lst)


-- Format a list of bytes as a string, showing printable ASCII chars
formatBytesNice : List Int -> String
formatBytesNice bytes =
    String.join ", " (List.map String.fromInt bytes)


-- Decode a list of bytes back to a string (best effort, show ? for invalid)
bytesToDisplayString : List Int -> String
bytesToDisplayString bytes =
    case UTF8.toString bytes of
        Ok s ->
            "\"" ++ s ++ "\""

        Err _ ->
            "(invalid UTF-8)"


-- Show the dictionary entry value nicely
dictValueDisplay : List Int -> String
dictValueDisplay bytes =
    let
        raw =
            String.join " " (List.map (\b -> String.fromInt b) bytes)

        decoded =
            case UTF8.toString bytes of
                Ok s ->
                    if String.isEmpty s then
                        ""
                    else
                        " → \"" ++ s ++ "\""

                Err _ ->
                    " → (non-UTF-8)"
    in
    "[" ++ raw ++ "]" ++ decoded



-- VIEW


view : Model -> Html Msg
view model =
    let
        bytes =
            intToBytes model.input

        ( codes, dict ) =
            lzwCompress bytes

        inputBitCount =
            bytesToBitCount bytes

        outputBitCount =
            codesToBitCount codes

        -- Only show the entries above 256 (the learned ones)
        learnedEntries =
            Dict.toList dict
                |> List.filter (\( k, _ ) -> k > 256)
                |> List.sortBy (\( k, _ ) -> -k)

        compressionRatio =
            if inputBitCount == 0 then
                "N/A"
            else
                let
                    ratio =
                        toFloat outputBitCount / toFloat inputBitCount * 100
                in
                String.fromInt (round ratio) ++ "%"
    in
    div [ class "container" ]
        [ header []
            [ h1 [] [ text "LZW Compression Demo" ]
            , p [ class "subtitle" ]
                [ text "8-bit input → 9-bit codes · Unicode supported" ]
            ]
        , section [ class "input-section" ]
            [ label [ for "text-input" ] [ text "Input Text" ]
            , textarea
                [ id "text-input"
                , value model.input
                , onInput InputChanged
                , placeholder "Type something to compress…"
                , rows 3
                ]
                []
            ]
        , if String.isEmpty model.input then
            div [ class "empty-state" ] [ text "Enter some text above to see LZW compression in action." ]

          else
            div [ class "results" ]
                [ statsBar inputBitCount outputBitCount compressionRatio
                , section [ class "result-section" ]
                    [ sectionHeader "1" "Raw Bytes (UTF-8)" (String.fromInt inputBitCount ++ " bits")
                    , div [ class "code-block" ]
                        [ text (formatBytesNice bytes) ]
                    ]
                , section [ class "result-section" ]
                    [ sectionHeader "2" "LZW-Encoded Codes (9-bit)" (String.fromInt outputBitCount ++ " bits")
                    , div [ class "code-block highlight" ]
                        [ text (formatList codes) ]
                    ]
                , section [ class "result-section" ]
                    [ sectionHeader "3" "Learned Dictionary" (String.fromInt (List.length learnedEntries) ++ " entries")
                    , if List.isEmpty learnedEntries then
                        div [ class "empty-dict" ] [ text "No new entries — input too short to build patterns." ]
                      else
                        div [ class "dict-table-wrapper" ]
                            [ table [ class "dict-table" ]
                                [ thead []
                                    [ tr []
                                        [ th [] [ text "Code" ]
                                        , th [] [ text "Bytes" ]
                                        , th [] [ text "Decodes to" ]
                                        ]
                                    ]
                                , tbody []
                                    (List.map dictRow learnedEntries)
                                ]
                            ]
                    ]
                ]
        , footer []
            [ text "LZW: 8-bit input, 9-bit codes (256 base symbols + codes 257–512)" ]
        ]


statsBar : Int -> Int -> String -> Html msg
statsBar inputBits outputBits ratio =
    div [ class "stats-bar" ]
        [ statCard "Input" (String.fromInt inputBits) "bits"
        , div [ class "arrow" ] [ text "→" ]
        , statCard "Output" (String.fromInt outputBits) "bits"
        , div [ class "arrow" ] [ text "=" ]
        , statCard "Ratio" ratio ""
        ]


statCard : String -> String -> String -> Html msg
statCard label value unit =
    div [ class "stat-card" ]
        [ div [ class "stat-label" ] [ text label ]
        , div [ class "stat-value" ]
            [ text value
            , if String.isEmpty unit then
                text ""
              else
                span [ class "stat-unit" ] [ text (" " ++ unit) ]
            ]
        ]


sectionHeader : String -> String -> String -> Html msg
sectionHeader num title badge =
    div [ class "section-header" ]
        [ span [ class "section-num" ] [ text num ]
        , h2 [] [ text title ]
        , span [ class "badge" ] [ text badge ]
        ]


dictRow : ( Int, List Int ) -> Html msg
dictRow ( code, byteList ) =
    tr []
        [ td [ class "code-cell" ] [ text (String.fromInt code) ]
        , td [ class "bytes-cell" ]
            [ text (String.join " " (List.map String.fromInt byteList)) ]
        , td [ class "decode-cell" ]
            [ case UTF8.toString byteList of
                Ok s ->
                    span [ class "decoded-string" ] [ text ("\"" ++ s ++ "\"") ]

                Err _ ->
                    span [ class "non-utf8" ] [ text "(non-UTF-8)" ]
            ]
        ]
