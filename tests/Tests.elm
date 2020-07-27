module Tests exposing (suite)

import Angle
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Geometry.Serialize as Serialize
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Serialize exposing (Codec)
import SketchPlane3d
import Test exposing (Test, describe, test)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


suite : Test
suite =
    describe "Round trip tests"
        [ roundTrip point2d "Point2d" Serialize.point2d
        , roundTrip point3d "Point3d" Serialize.point3d
        , roundTrip vector2d "Vector2d" Serialize.vector2d
        , roundTrip vector3d "Vector3d" Serialize.vector3d
        , roundTripEqualWithin
            direction2d
            "Direction2d"
            Serialize.direction2d
            (Direction2d.equalWithin (Angle.degrees 0.1))
        , roundTripEqualWithin
            direction3d
            "Direction3d"
            Serialize.direction3d
            (Direction3d.equalWithin (Angle.degrees 0.1))
        ]


roundTrip : Fuzzer a -> String -> Codec e a -> Test
roundTrip fuzzer name codec =
    Test.fuzz fuzzer
        name
        (\value ->
            Serialize.encodeToBytes codec value
                |> Serialize.decodeFromBytes codec
                |> Expect.equal (Ok value)
        )


roundTripEqualWithin : Fuzzer a -> String -> Codec e a -> (a -> a -> Bool) -> Test
roundTripEqualWithin fuzzer name codec equalWithin =
    Test.fuzz fuzzer
        name
        (\value ->
            case
                Serialize.encodeToBytes codec value
                    |> Serialize.decodeFromBytes codec
            of
                Ok ok ->
                    equalWithin ok value |> Expect.true "Values are not equal within tolerances"

                Err _ ->
                    Expect.fail "Failed to decode value"
        )


quantity : Fuzzer (Quantity Float units)
quantity =
    Fuzz.map Quantity.Quantity Fuzz.float


point2d : Fuzzer (Point2d units coordinates)
point2d =
    Fuzz.map2 Point2d.xy
        quantity
        quantity


point3d : Fuzzer (Point3d units coordinates)
point3d =
    Fuzz.map3 Point3d.xyz
        quantity
        quantity
        quantity


vector2d : Fuzzer (Vector2d units coordinates)
vector2d =
    Fuzz.map2 Vector2d.xy
        quantity
        quantity


vector3d : Fuzzer (Vector3d units coordinates)
vector3d =
    Fuzz.map3 Vector3d.xyz
        quantity
        quantity
        quantity


direction2d : Fuzzer (Direction2d coordinates)
direction2d =
    Fuzz.map Direction2d.fromAngle quantity


direction3d : Fuzzer (Direction3d coordinates)
direction3d =
    Fuzz.map2 (Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy)
        quantity
        quantity
