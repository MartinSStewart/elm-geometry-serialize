module Tests exposing (suite)

import Angle
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Frame2d exposing (Frame2d)
import Fuzz exposing (Fuzzer)
import Geometry.Serialize as Serialize
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Serialize exposing (Codec)
import SketchPlane3d
import Test exposing (Test)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


suite : Test
suite =
    Test.describe "Round trip tests"
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
        , roundTripEqualWithin axis2d "Axis2d" Serialize.axis2d axis2dEqualWithin
        , roundTripEqualWithin axis3d "Axis3d" Serialize.axis3d axis3dEqualWithin
        , roundTripEqualWithin frame2d "Frame2d" Serialize.frame2d frame2dEqualWithin
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


axis2d : Fuzzer (Axis2d units coordinates)
axis2d =
    Fuzz.map2 Axis2d.withDirection
        direction2d
        point2d


axis3d : Fuzzer (Axis3d units coordinates)
axis3d =
    Fuzz.map2 Axis3d.withDirection
        direction3d
        point3d


frame2d : Fuzzer (Frame2d units coordinates defines)
frame2d =
    Fuzz.map3
        (\angle position mirror ->
            let
                frame =
                    Frame2d.withAngle angle position
            in
            if mirror then
                Frame2d.mirrorAcross Axis2d.y frame

            else
                frame
        )
        quantity
        point2d
        Fuzz.bool


frame2dEqualWithin : Frame2d units coordinates defines -> Frame2d units coordinates defines -> Bool
frame2dEqualWithin f0 f1 =
    (Frame2d.originPoint f0 == Frame2d.originPoint f1)
        && axis2dEqualWithin (Frame2d.xAxis f0) (Frame2d.xAxis f1)
        && axis2dEqualWithin (Frame2d.yAxis f0) (Frame2d.yAxis f1)


direction2dEqualWithin : Direction2d coordinates -> Direction2d coordinates -> Bool
direction2dEqualWithin d0 d1 =
    Direction2d.angleFrom d0 d1
        |> quantityEqualWithin Quantity.zero


quantityEqualWithin : Quantity Float units -> Quantity Float units -> Bool
quantityEqualWithin q0 q1 =
    Quantity.minus q0 q1 |> Quantity.abs |> Quantity.lessThan (Quantity.Quantity 0.01)


direction3dEqualWithin : Direction3d coordinates -> Direction3d coordinates -> Bool
direction3dEqualWithin d0 d1 =
    let
        ( x0, y0, z0 ) =
            Direction3d.components d0

        ( x1, y1, z1 ) =
            Direction3d.components d1

        floatEqualWithin : Float -> Float -> Bool
        floatEqualWithin q0 q1 =
            abs (q0 - q1) < 0.01
    in
    floatEqualWithin x0 x1 && floatEqualWithin y0 y1 && floatEqualWithin z0 z1


axis2dEqualWithin : Axis2d units coordinates -> Axis2d units coordinates -> Bool
axis2dEqualWithin a0 a1 =
    (Axis2d.originPoint a0 == Axis2d.originPoint a1)
        && direction2dEqualWithin (Axis2d.direction a0) (Axis2d.direction a1)


axis3dEqualWithin : Axis3d units coordinates -> Axis3d units coordinates -> Bool
axis3dEqualWithin a0 a1 =
    (Axis3d.originPoint a0 == Axis3d.originPoint a1)
        && direction3dEqualWithin (Axis3d.direction a0) (Axis3d.direction a1)
