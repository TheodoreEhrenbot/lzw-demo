module LzwTest exposing (suite)

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import String.UTF8 as UTF8
import Test exposing (Test, describe, fuzz, fuzz2, test)

import Main exposing (lzwCompress, lzwDecompress)


-- ---------------------------------------------------------------------------
-- Fuzzers
-- ---------------------------------------------------------------------------

{-| A single byte: 0..255 -}
byteFuzzer : Fuzzer Int
byteFuzzer =
    Fuzz.intRange 0 255


{-| A list of bytes of length 0..100 -}
byteListFuzzer : Fuzzer (List Int)
byteListFuzzer =
    Fuzz.listOfLengthBetween 0 100 byteFuzzer


{-| A longer list of bytes (up to 500) to stress-test dictionary growth -}
longByteListFuzzer : Fuzzer (List Int)
longByteListFuzzer =
    Fuzz.listOfLengthBetween 0 500 byteFuzzer


{-| A list using only a small alphabet (a/b/c) to generate lots of patterns -}
repetitiveByteListFuzzer : Fuzzer (List Int)
repetitiveByteListFuzzer =
    Fuzz.listOfLengthBetween 0 200 (Fuzz.oneOfValues [ 65, 66, 67 ])


{-| A list of all-identical bytes — maximally compressible -}
uniformByteListFuzzer : Fuzzer (List Int)
uniformByteListFuzzer =
    Fuzz.map2
        (\b n -> List.repeat n b)
        byteFuzzer
        (Fuzz.intRange 0 300)


suite : Test
suite =
    describe "LZW round-trip: compress then decompress = identity"
        [ -- -----------------------------------------------------------------
          -- Core round-trip property
          -- -----------------------------------------------------------------
          describe "round-trip property"
            [ fuzz byteListFuzzer "compress then decompress recovers any byte list" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , fuzz longByteListFuzzer "round-trip holds for long byte lists (up to 500 bytes)" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , fuzz repetitiveByteListFuzzer "round-trip holds for repetitive/highly-compressible input" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , fuzz uniformByteListFuzzer "round-trip holds for all-identical-byte sequences" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , fuzz Fuzz.string "round-trip holds for arbitrary UTF-8 strings" <|
                \s ->
                    let
                        bytes =
                            UTF8.toBytes s
                    in
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)
            ]

        , -- -----------------------------------------------------------------
          -- Output code validity
          -- -----------------------------------------------------------------
          describe "encoded code validity"
            [ fuzz byteListFuzzer "all output codes are >= 1" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> List.all (\c -> c >= 1)
                        |> Expect.equal True

            , fuzz byteListFuzzer "all output codes are <= 512 for inputs ≤ 100 bytes" <|
                -- With at most 100 bytes, at most 99 new dict entries are added,
                -- so codes stay well within 9-bit range (max 512).
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> List.all (\c -> c <= 512)
                        |> Expect.equal True

            , fuzz byteListFuzzer "number of output codes <= number of input bytes" <|
                -- Each output code corresponds to at least one input byte consumed.
                \bytes ->
                    let
                        codes =
                            bytes |> lzwCompress |> Tuple.first
                    in
                    List.length codes
                        |> Expect.atMost (max 1 (List.length bytes))
            ]

        , -- -----------------------------------------------------------------
          -- Dictionary properties
          -- -----------------------------------------------------------------
          describe "dictionary properties"
            [ fuzz byteListFuzzer "dictionary always contains all 256 single-byte entries" <|
                \bytes ->
                    let
                        dict =
                            bytes |> lzwCompress |> Tuple.second
                    in
                    List.range 0 255
                        |> List.all (\b -> Dict.get (b + 1) dict == Just [ b ])
                        |> Expect.equal True

            , fuzz byteListFuzzer "every learned dict entry (>=257) has length >= 2" <|
                \bytes ->
                    let
                        dict =
                            bytes |> lzwCompress |> Tuple.second
                    in
                    Dict.toList dict
                        |> List.filter (\( k, _ ) -> k >= 257)
                        |> List.all (\( _, v ) -> List.length v >= 2)
                        |> Expect.equal True

            , fuzz byteListFuzzer "learned dict entries only contain bytes in 0..255" <|
                \bytes ->
                    let
                        dict =
                            bytes |> lzwCompress |> Tuple.second
                    in
                    Dict.values dict
                        |> List.concat
                        |> List.all (\b -> b >= 0 && b <= 255)
                        |> Expect.equal True
            ]

        , -- -----------------------------------------------------------------
          -- Decompressor properties
          -- -----------------------------------------------------------------
          describe "decompressor properties"
            [ test "decompress empty list gives Just []" <|
                \_ ->
                    lzwDecompress []
                        |> Expect.equal (Just [])

            , fuzz byteListFuzzer "decompressor always returns Just (never Nothing) for valid compressed input" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> (\result ->
                                case result of
                                    Just _ ->
                                        Expect.pass

                                    Nothing ->
                                        Expect.fail "decompressor returned Nothing for valid compressed input"
                           )

            , fuzz byteListFuzzer "decompressor output length matches original input length" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Maybe.map List.length
                        |> Expect.equal (Just (List.length bytes))
            ]

        , -- -----------------------------------------------------------------
          -- Bit-count properties
          -- -----------------------------------------------------------------
          describe "bit count properties"
            [ fuzz byteListFuzzer "input bit count = 8 * byte count" <|
                \bytes ->
                    List.length bytes * 8
                        |> Expect.equal (List.length bytes * 8)

            , fuzz byteListFuzzer "output bit count = 9 * code count" <|
                \bytes ->
                    let
                        codes =
                            bytes |> lzwCompress |> Tuple.first
                    in
                    List.length codes * 9
                        |> Expect.equal (List.length codes * 9)
            ]

        , -- -----------------------------------------------------------------
          -- Known-answer unit tests
          -- -----------------------------------------------------------------
          describe "known-answer tests"
            [ test "empty input → empty output" <|
                \_ ->
                    lzwCompress []
                        |> Tuple.first
                        |> Expect.equal []

            , test "single byte 65 ('A') encodes to code 66" <|
                \_ ->
                    lzwCompress [ 65 ]
                        |> Tuple.first
                        |> Expect.equal [ 66 ]

            , test "single byte round-trips" <|
                \_ ->
                    lzwDecompress [ 66 ]
                        |> Expect.equal (Just [ 65 ])

            , test "ABABABAB encodes to [66, 67, 257, 259, 67]" <|
                \_ ->
                    lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Tuple.first
                        |> Expect.equal [ 66, 67, 257, 259, 67 ]

            , test "ABABABAB round-trips" <|
                \_ ->
                    [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just [ 65, 66, 65, 66, 65, 66, 65, 66 ])

            , test "ABABABAB dict entry 257 = [65, 66] (AB)" <|
                \_ ->
                    lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Tuple.second
                        |> Dict.get 257
                        |> Expect.equal (Just [ 65, 66 ])

            , test "ABABABAB dict entry 259 = [65, 66, 65] (ABA)" <|
                \_ ->
                    lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Tuple.second
                        |> Dict.get 259
                        |> Expect.equal (Just [ 65, 66, 65 ])

            , test "byte 0 encodes to code 1 and round-trips" <|
                \_ ->
                    [ 0 ]
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just [ 0 ])

            , test "byte 255 encodes to code 256 and round-trips" <|
                \_ ->
                    [ 255 ]
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just [ 255 ])

            , test "UTF-8 string 'hello' round-trips" <|
                \_ ->
                    let
                        bytes =
                            UTF8.toBytes "hello"
                    in
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , test "UTF-8 string with emoji '🎉' round-trips" <|
                \_ ->
                    let
                        bytes =
                            UTF8.toBytes "🎉"
                    in
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)

            , test "all 256 distinct bytes round-trip" <|
                \_ ->
                    let
                        bytes =
                            List.range 0 255
                    in
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> lzwDecompress
                        |> Expect.equal (Just bytes)
            ]
        ]
