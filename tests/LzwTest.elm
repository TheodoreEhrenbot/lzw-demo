module LzwTest exposing (suite)

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import String.UTF8 as UTF8
import Test exposing (Test, describe, fuzz, fuzz2, test)

import Main exposing (lzwCompress, lzwDecompress, maxDictCode)


-- ---------------------------------------------------------------------------
-- Fuzzers
-- ---------------------------------------------------------------------------

byteFuzzer : Fuzzer Int
byteFuzzer =
    Fuzz.intRange 0 255

{-| Short byte lists (0..100 bytes) -}
byteListFuzzer : Fuzzer (List Int)
byteListFuzzer =
    Fuzz.listOfLengthBetween 0 100 byteFuzzer

{-| Long byte lists (0..2000 bytes) — enough to fill and freeze the dictionary -}
longByteListFuzzer : Fuzzer (List Int)
longByteListFuzzer =
    Fuzz.listOfLengthBetween 0 2000 byteFuzzer

{-| Small alphabet — generates lots of patterns, fills dict quickly -}
repetitiveFuzzer : Fuzzer (List Int)
repetitiveFuzzer =
    Fuzz.listOfLengthBetween 0 2000 (Fuzz.oneOfValues [ 65, 66, 67 ])

{-| All-identical bytes — worst case for the special decompressor branch,
    and fills dict very quickly -}
uniformFuzzer : Fuzzer (List Int)
uniformFuzzer =
    Fuzz.map2 List.repeat (Fuzz.intRange 0 2000) byteFuzzer

{-| Arbitrary UTF-8 strings -}
stringFuzzer : Fuzzer (List Int)
stringFuzzer =
    Fuzz.map UTF8.toBytes Fuzz.string


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

roundTrip : List Int -> Maybe (List Int)
roundTrip bytes =
    bytes |> lzwCompress |> Tuple.first |> lzwDecompress


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

suite : Test
suite =
    describe "LZW"
        [ -- -----------------------------------------------------------------
          -- Core round-trip: the main correctness property
          -- -----------------------------------------------------------------
          describe "round-trip (compress then decompress = identity)"
            [ fuzz byteListFuzzer "short byte lists" <|
                \bytes -> roundTrip bytes |> Expect.equal (Just bytes)

            , fuzz longByteListFuzzer "long byte lists (up to 2000 bytes, dict will freeze)" <|
                \bytes -> roundTrip bytes |> Expect.equal (Just bytes)

            , fuzz repetitiveFuzzer "repetitive small-alphabet input (dict fills fast)" <|
                \bytes -> roundTrip bytes |> Expect.equal (Just bytes)

            , fuzz uniformFuzzer "all-identical bytes (triggers decompressor special case)" <|
                \bytes -> roundTrip bytes |> Expect.equal (Just bytes)

            , fuzz stringFuzzer "arbitrary UTF-8 strings" <|
                \bytes -> roundTrip bytes |> Expect.equal (Just bytes)
            ]

        , -- -----------------------------------------------------------------
          -- Dictionary freeze: codes must never exceed maxDictCode
          -- -----------------------------------------------------------------
          describe "dictionary freeze at maxDictCode"
            [ fuzz longByteListFuzzer "all output codes are <= maxDictCode" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> List.all (\c -> c <= maxDictCode)
                        |> Expect.equal True
                        |> Expect.onFail "a code exceeded maxDictCode"

            , fuzz longByteListFuzzer "all output codes are >= 1" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.first
                        |> List.all (\c -> c >= 1)
                        |> Expect.equal True

            , fuzz longByteListFuzzer "dict never contains more than maxDictCode entries" <|
                \bytes ->
                    bytes
                        |> lzwCompress
                        |> Tuple.second
                        |> Dict.size
                        |> Expect.atMost maxDictCode

            , test "dict size is exactly maxDictCode when input cycles all 256 bytes twice" <|
                \_ ->
                    -- First cycle (0..255): adds pairs [0,1],[1,2],...,[254,255] → codes 257..511 (255 entries)
                    -- First byte of second cycle (0): adds [255,0] → code 512 (1 entry), dict now full
                    -- Remaining second cycle: dict frozen, no more entries added
                    -- Total: 256 base + 256 learned = 512 = maxDictCode
                    List.range 0 255 ++ List.range 0 255
                        |> lzwCompress
                        |> Tuple.second
                        |> Dict.size
                        |> Expect.equal maxDictCode

            , fuzz repetitiveFuzzer "round-trip still holds after dict freeze" <|
                \bytes ->
                    roundTrip bytes |> Expect.equal (Just bytes)
            ]

        , -- -----------------------------------------------------------------
          -- Output length / bit count
          -- -----------------------------------------------------------------
          describe "output properties"
            [ fuzz byteListFuzzer "output code count <= input byte count (or 1 for non-empty)" <|
                \bytes ->
                    let
                        codes =
                            bytes |> lzwCompress |> Tuple.first
                    in
                    List.length codes
                        |> Expect.atMost (max 1 (List.length bytes))

            , fuzz longByteListFuzzer "decompressor returns Just for all valid compressed streams" <|
                \bytes ->
                    case roundTrip bytes of
                        Just _ ->
                            Expect.pass

                        Nothing ->
                            Expect.fail "decompressor returned Nothing for valid compressed input"

            , fuzz longByteListFuzzer "decompressed length equals original length" <|
                \bytes ->
                    roundTrip bytes
                        |> Maybe.map List.length
                        |> Expect.equal (Just (List.length bytes))
            ]

        , -- -----------------------------------------------------------------
          -- Dictionary invariants
          -- -----------------------------------------------------------------
          describe "dictionary invariants"
            [ fuzz longByteListFuzzer "all 256 single-byte entries are always present" <|
                \bytes ->
                    let
                        dict =
                            bytes |> lzwCompress |> Tuple.second
                    in
                    List.range 0 255
                        |> List.all (\b -> Dict.get (b + 1) dict == Just [ b ])
                        |> Expect.equal True

            , fuzz longByteListFuzzer "learned entries (>=257) all have length >= 2" <|
                \bytes ->
                    let
                        dict =
                            bytes |> lzwCompress |> Tuple.second
                    in
                    Dict.toList dict
                        |> List.filter (\( k, _ ) -> k >= 257)
                        |> List.all (\( _, v ) -> List.length v >= 2)
                        |> Expect.equal True

            , fuzz longByteListFuzzer "all byte values in dict entries are 0..255" <|
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
          -- Known-answer unit tests
          -- -----------------------------------------------------------------
          describe "known-answer tests"
            [ test "empty input" <|
                \_ ->
                    lzwCompress [] |> Tuple.first |> Expect.equal []

            , test "decompress empty gives Just []" <|
                \_ ->
                    lzwDecompress [] |> Expect.equal (Just [])

            , test "byte 65 ('A') -> code 66" <|
                \_ ->
                    lzwCompress [ 65 ] |> Tuple.first |> Expect.equal [ 66 ]

            , test "byte 0 -> code 1" <|
                \_ ->
                    lzwCompress [ 0 ] |> Tuple.first |> Expect.equal [ 1 ]

            , test "byte 255 -> code 256" <|
                \_ ->
                    lzwCompress [ 255 ] |> Tuple.first |> Expect.equal [ 256 ]

            , test "ABABABAB encodes to [66, 67, 257, 259, 67]" <|
                \_ ->
                    lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Tuple.first
                        |> Expect.equal [ 66, 67, 257, 259, 67 ]

            , test "ABABABAB round-trips" <|
                \_ ->
                    roundTrip [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Expect.equal (Just [ 65, 66, 65, 66, 65, 66, 65, 66 ])

            , test "ABABABAB dict[257] = AB" <|
                \_ ->
                    lzwCompress [ 65, 66, 65, 66, 65, 66, 65, 66 ]
                        |> Tuple.second
                        |> Dict.get 257
                        |> Expect.equal (Just [ 65, 66 ])

            , test "all 256 distinct bytes round-trip" <|
                \_ ->
                    roundTrip (List.range 0 255)
                        |> Expect.equal (Just (List.range 0 255))

            , test "1000 'A' bytes round-trip (dict freezes mid-stream)" <|
                \_ ->
                    roundTrip (List.repeat 1000 65)
                        |> Expect.equal (Just (List.repeat 1000 65))

            , test "1000 'A' bytes produce codes all <= maxDictCode" <|
                \_ ->
                    List.repeat 1000 65
                        |> lzwCompress
                        |> Tuple.first
                        |> List.all (\c -> c <= maxDictCode)
                        |> Expect.equal True

            , test "UTF-8 emoji '🎉' round-trips" <|
                \_ ->
                    let bytes = UTF8.toBytes "🎉" in
                    roundTrip bytes |> Expect.equal (Just bytes)

            , test "long repetitive string round-trips" <|
                \_ ->
                    let bytes = UTF8.toBytes (String.repeat 200 "abcabc") in
                    roundTrip bytes |> Expect.equal (Just bytes)
            ]
        ]
