module LzwTest exposing (suite)

import Dict
import Expect
import String.UTF8 as UTF8
import Test exposing (Test, describe, test)

import Main exposing (lzwCompress)


suite : Test
suite =
    describe "LZW compression"
        [ test "empty input produces empty output" <|
            \_ ->
                lzwCompress []
                    |> Tuple.first
                    |> Expect.equal []

        , test "single byte is encoded as code = byte + 1" <|
            \_ ->
                -- byte 65 ('A') → code 66
                lzwCompress [ 65 ]
                    |> Tuple.first
                    |> Expect.equal [ 66 ]

        , test "ABABABAB classic LZW example" <|
            \_ ->
                -- A=65→code66, B=66→code67
                -- Expected: [66, 67, 257, 259, 67]
                let
                    bytes =
                        [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                in
                lzwCompress bytes
                    |> Tuple.first
                    |> Expect.equal [ 66, 67, 257, 259, 67 ]

        , test "ABABABAB produces dictionary entries for patterns" <|
            \_ ->
                let
                    bytes =
                        [ 65, 66, 65, 66, 65, 66, 65, 66 ]

                    ( _, dict ) =
                        lzwCompress bytes
                in
                Expect.all
                    [ \d -> Dict.get 257 d |> Expect.equal (Just [ 65, 66 ])
                    , \d -> Dict.get 258 d |> Expect.equal (Just [ 66, 65 ])
                    , \d -> Dict.get 259 d |> Expect.equal (Just [ 65, 66, 65 ])
                    , \d -> Dict.get 260 d |> Expect.equal (Just [ 65, 66, 65, 66 ])
                    ]
                    dict

        , test "initial dictionary maps byte 0 to code 1" <|
            \_ ->
                lzwCompress [ 0 ]
                    |> Tuple.first
                    |> Expect.equal [ 1 ]

        , test "initial dictionary maps byte 255 to code 256" <|
            \_ ->
                lzwCompress [ 255 ]
                    |> Tuple.first
                    |> Expect.equal [ 256 ]

        , test "UTF-8 encoding of ASCII is single byte" <|
            \_ ->
                UTF8.toBytes "A"
                    |> Expect.equal [ 65 ]

        , test "UTF-8 encoding of 2-byte character" <|
            \_ ->
                -- U+00E9 'é' is 0xC3 0xA9 = [195, 169]
                UTF8.toBytes "é"
                    |> Expect.equal [ 195, 169 ]

        , test "output bit count is 9 * number of codes" <|
            \_ ->
                let
                    codes =
                        lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                            |> Tuple.first
                in
                List.length codes * 9
                    |> Expect.equal 45

        , test "all codes are ≤ 512 for short input (within 9-bit range)" <|
            \_ ->
                let
                    codes =
                        lzwCompress (List.repeat 20 65)
                            |> Tuple.first
                in
                codes
                    |> List.all (\c -> c >= 1 && c <= 512)
                    |> Expect.equal True
        ]
