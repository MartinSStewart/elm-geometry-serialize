module Tests exposing (suite)

import Arc2d exposing (Arc2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Fuzz
import Geometry.Serialize as Serialize
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle3d exposing (Rectangle3d)
import Serialize exposing (Codec)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Round trip tests"
        [ roundTripEqualWithin Geometry.Fuzz.arc2d "Arc2d" Serialize.arc2d arc2dEqualWithin

        --, roundTripEqualWithin Geometry.Fuzz.arc3d "Arc3d" Serialize.arc3d arc3dEqualWithin
        , roundTrip Geometry.Fuzz.axis2d "Axis2d" Serialize.axis2d
        , roundTrip Geometry.Fuzz.axis3d "Axis3d" Serialize.axis3d
        , roundTripEqualWithin Geometry.Fuzz.block3d "Block3d" Serialize.block3d block3dEqualWithin
        , roundTrip Geometry.Fuzz.boundingBox2d "BoundingBox2d" Serialize.boundingBox2d
        , roundTrip Geometry.Fuzz.boundingBox3d "BoundingBox3d" Serialize.boundingBox3d
        , roundTrip Geometry.Fuzz.circle2d "Circle2d" Serialize.circle2d
        , roundTrip Geometry.Fuzz.circle3d "Circle3d" Serialize.circle3d

        --, roundTripEqualWithin Geometry.Fuzz.cone3d "Cone3d" Serialize.cone3d cone3dEqualWithin
        , roundTrip Geometry.Fuzz.cubicSpline2d "CubicSpline2d" Serialize.cubicSpline2d
        , roundTrip Geometry.Fuzz.cubicSpline3d "CubicSpline3d" Serialize.cubicSpline3d
        , roundTrip Geometry.Fuzz.cylinder3d "Cylinder3d" Serialize.cylinder3d
        , roundTrip Geometry.Fuzz.direction2d "Direction2d" Serialize.direction2d
        , roundTrip Geometry.Fuzz.direction3d "Direction3d" Serialize.direction3d
        , roundTrip Geometry.Fuzz.ellipse2d "Ellipse2d" Serialize.ellipse2d
        , roundTrip Geometry.Fuzz.ellipticalArc2d "EllipticalArc2d" Serialize.ellipticalArc2d
        , roundTrip Geometry.Fuzz.frame2d "Frame2d" Serialize.frame2d
        , roundTripEqualWithin Geometry.Fuzz.frame3d "Frame3d" Serialize.frame3d frame3dEqualWithin
        , roundTrip Geometry.Fuzz.lineSegment2d "LineSegment2d" Serialize.lineSegment2d
        , roundTrip Geometry.Fuzz.lineSegment3d "LineSegment3d" Serialize.lineSegment3d
        , roundTrip Geometry.Fuzz.plane3d "Plane3d" Serialize.plane3d
        , roundTrip Geometry.Fuzz.point2d "Point2d" Serialize.point2d
        , roundTrip Geometry.Fuzz.point3d "Point3d" Serialize.point3d
        , roundTrip Geometry.Fuzz.polygon2d "Polygon2d" Serialize.polygon2d
        , roundTrip Geometry.Fuzz.polyline2d "Polyline2d" Serialize.polyline2d
        , roundTrip Geometry.Fuzz.polyline3d "Polyline3d" Serialize.polyline3d
        , roundTrip Geometry.Fuzz.quadraticSpline2d "QuadraticSpline2d" Serialize.quadraticSpline2d
        , roundTrip Geometry.Fuzz.quadraticSpline3d "QuadraticSpline3d" Serialize.quadraticSpline3d
        , roundTrip Geometry.Fuzz.rectangle2d "Rectangle2d" Serialize.rectangle2d
        , roundTrip rectangle3d "Rectangle3d" Serialize.rectangle3d
        , roundTrip Geometry.Fuzz.sketchPlane3d "SketchPlane3d" Serialize.sketchPlane3d
        , roundTrip Geometry.Fuzz.sphere3d "Sphere3d" Serialize.sphere3d
        , roundTrip Geometry.Fuzz.triangle2d "Triangle2d" Serialize.triangle2d
        , roundTrip Geometry.Fuzz.triangle3d "Triangle3d" Serialize.triangle3d
        , roundTrip Geometry.Fuzz.vector2d "Vector2d" Serialize.vector2d
        , roundTrip Geometry.Fuzz.vector3d "Vector3d" Serialize.vector3d
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
                    equalWithin ok value |> Expect.true ("Values are not equal within tolerances: " ++ Debug.toString ok)

                Err _ ->
                    Expect.fail "Failed to decode value"
        )


rectangle3d : Fuzzer (Rectangle3d Meters coordinates)
rectangle3d =
    Fuzz.map2 Rectangle3d.centeredOn
        Geometry.Fuzz.sketchPlane3d
        (Fuzz.map2 (\w h -> ( Length.meters w, Length.meters h )) Fuzz.float Fuzz.float)


frame3dEqualWithin : Frame3d units coordinates defines -> Frame3d units coordinates defines -> Bool
frame3dEqualWithin f0 f1 =
    (Frame3d.originPoint f0 == Frame3d.originPoint f1)
        && axis3dEqualWithin (Frame3d.xAxis f0) (Frame3d.xAxis f1)
        && axis3dEqualWithin (Frame3d.yAxis f0) (Frame3d.yAxis f1)
        && axis3dEqualWithin (Frame3d.zAxis f0) (Frame3d.zAxis f1)


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


axis3dEqualWithin : Axis3d units coordinates -> Axis3d units coordinates -> Bool
axis3dEqualWithin a0 a1 =
    (Axis3d.originPoint a0 == Axis3d.originPoint a1)
        && direction3dEqualWithin (Axis3d.direction a0) (Axis3d.direction a1)


arc2dEqualWithin : Arc2d units coordinates -> Arc2d units coordinates -> Bool
arc2dEqualWithin a0 a1 =
    (Arc2d.startPoint a0 == Arc2d.startPoint a1)
        && (Arc2d.sweptAngle a0 == Arc2d.sweptAngle a1)
        && point2dEqualWithin (Arc2d.endPoint a0) (Arc2d.endPoint a1)


point2dEqualWithin : Point2d units coordinates -> Point2d units coordinates -> Bool
point2dEqualWithin p0 p1 =
    quantityEqualWithin (Point2d.xCoordinate p0) (Point2d.xCoordinate p1)
        && quantityEqualWithin (Point2d.yCoordinate p0) (Point2d.yCoordinate p1)


block3dEqualWithin : Block3d units coordinates -> Block3d units coordinates -> Bool
block3dEqualWithin b0 b1 =
    (Block3d.dimensions b0 == Block3d.dimensions b1)
        && frame3dEqualWithin (Block3d.axes b0) (Block3d.axes b1)
