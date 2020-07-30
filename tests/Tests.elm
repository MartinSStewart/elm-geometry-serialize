module Tests exposing (suite)

import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Serialize as Serialize
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Serialize exposing (Codec)
import SketchPlane3d exposing (SketchPlane3d)
import Test exposing (Test)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


suite : Test
suite =
    Test.describe "Round trip tests"
        [ roundTrip point2d "Point2d" Serialize.point2d
        , roundTrip point3d "Point3d" Serialize.point3d
        , roundTrip vector2d "Vector2d" Serialize.vector2d
        , roundTrip vector3d "Vector3d" Serialize.vector3d
        , roundTripEqualWithin direction2d "Direction2d" Serialize.direction2d direction2dEqualWithin
        , roundTripEqualWithin direction3d "Direction3d" Serialize.direction3d direction3dEqualWithin
        , roundTripEqualWithin axis2d "Axis2d" Serialize.axis2d axis2dEqualWithin
        , roundTripEqualWithin axis3d "Axis3d" Serialize.axis3d axis3dEqualWithin
        , roundTripEqualWithin frame2d "Frame2d" Serialize.frame2d frame2dEqualWithin
        , roundTripEqualWithin frame3d "Frame3d" Serialize.frame3d frame3dEqualWithin
        , roundTripEqualWithin sketchPlane3d "SketchPlane3d" Serialize.sketchPlane3d sketchPlane3dEqualWithin
        , roundTripEqualWithin plane3d "Plane3d" Serialize.plane3d plane3dEqualWithin
        , roundTrip boundingBox2d "BoundingBox2d" Serialize.boundingBox2d
        , roundTrip boundingBox3d "BoundingBox3d" Serialize.boundingBox3d
        , roundTrip quadraticSpline2d "QuadraticSpline2d" Serialize.quadraticSpline2d
        , roundTrip quadraticSpline3d "QuadraticSpline3d" Serialize.quadraticSpline3d
        , roundTripEqualWithin rectangle2d "Rectangle2d" Serialize.rectangle2d rectangle2dEqualWithin
        , roundTrip triangle2d "Triangle2d" Serialize.triangle2d
        , roundTrip triangle3d "Triangle3d" Serialize.triangle3d
        , roundTripEqualWithin arc2d "Arc2d" Serialize.arc2d arc2dEqualWithin
        , roundTripEqualWithin arc3d "Arc3d" Serialize.arc3d arc3dEqualWithin
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


frame3d : Fuzzer (Frame3d units coordinates defines)
frame3d =
    Fuzz.map4
        (\position xDirection yDirection mirror ->
            let
                ortho =
                    Direction3d.orthogonalize xDirection yDirection (Direction3d.perpendicularTo xDirection)
            in
            case ortho of
                Just ( xDir, yDir, zDir ) ->
                    Frame3d.unsafe
                        { originPoint = position
                        , xDirection = xDir
                        , yDirection = yDir
                        , zDirection =
                            if mirror then
                                Direction3d.reverse zDir

                            else
                                zDir
                        }

                Nothing ->
                    Frame3d.withXDirection xDirection position
        )
        point3d
        direction3d
        direction3d
        Fuzz.bool


sketchPlane3d : Fuzzer (SketchPlane3d units coordinates defines)
sketchPlane3d =
    Fuzz.map3
        (\position direction rotation ->
            SketchPlane3d.through position direction
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.normalAxis rotation
        )
        point3d
        direction3d
        quantity


plane3d : Fuzzer (Plane3d units coordinates)
plane3d =
    Fuzz.map2 Plane3d.withNormalDirection
        direction3d
        point3d


boundingBox2d : Fuzzer (BoundingBox2d units coordinates)
boundingBox2d =
    Fuzz.map2 BoundingBox2d.from
        point2d
        point2d


boundingBox3d : Fuzzer (BoundingBox3d units coordinates)
boundingBox3d =
    Fuzz.map2 BoundingBox3d.from
        point3d
        point3d


quadraticSpline2d : Fuzzer (QuadraticSpline2d units coordinates)
quadraticSpline2d =
    Fuzz.map3 QuadraticSpline2d.fromControlPoints
        point2d
        point2d
        point2d


quadraticSpline3d : Fuzzer (QuadraticSpline3d units coordinates)
quadraticSpline3d =
    Fuzz.map3 QuadraticSpline3d.fromControlPoints
        point3d
        point3d
        point3d


rectangle2d : Fuzzer (Rectangle2d units coordinates)
rectangle2d =
    Fuzz.map3 (\p0 p1 rotation -> Rectangle2d.from p0 p1 |> Rectangle2d.rotateAround Point2d.origin rotation)
        point2d
        point2d
        quantity


triangle2d : Fuzzer (Triangle2d units coordinates)
triangle2d =
    Fuzz.map3 Triangle2d.from
        point2d
        point2d
        point2d


triangle3d : Fuzzer (Triangle3d units coordinates)
triangle3d =
    Fuzz.map3 Triangle3d.from
        point3d
        point3d
        point3d


arc2d : Fuzzer (Arc2d units coordinates)
arc2d =
    Fuzz.map3 Arc2d.from
        point2d
        point2d
        quantity


arc3d : Fuzzer (Arc3d units coordinates)
arc3d =
    Fuzz.map3 Arc3d.sweptAround
        axis3d
        quantity
        point3d


frame2dEqualWithin : Frame2d units coordinates defines -> Frame2d units coordinates defines -> Bool
frame2dEqualWithin f0 f1 =
    (Frame2d.originPoint f0 == Frame2d.originPoint f1)
        && axis2dEqualWithin (Frame2d.xAxis f0) (Frame2d.xAxis f1)
        && axis2dEqualWithin (Frame2d.yAxis f0) (Frame2d.yAxis f1)


frame3dEqualWithin : Frame3d units coordinates defines -> Frame3d units coordinates defines -> Bool
frame3dEqualWithin f0 f1 =
    (Frame3d.originPoint f0 == Frame3d.originPoint f1)
        && axis3dEqualWithin (Frame3d.xAxis f0) (Frame3d.xAxis f1)
        && axis3dEqualWithin (Frame3d.yAxis f0) (Frame3d.yAxis f1)
        && axis3dEqualWithin (Frame3d.zAxis f0) (Frame3d.zAxis f1)


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


sketchPlane3dEqualWithin : SketchPlane3d units coordinates defines -> SketchPlane3d units coordinates defines -> Bool
sketchPlane3dEqualWithin s0 s1 =
    axis3dEqualWithin (SketchPlane3d.normalAxis s0) (SketchPlane3d.normalAxis s1)
        && direction3dEqualWithin (SketchPlane3d.xDirection s0) (SketchPlane3d.xDirection s1)
        && direction3dEqualWithin (SketchPlane3d.yDirection s0) (SketchPlane3d.yDirection s1)


plane3dEqualWithin : Plane3d units coordinates -> Plane3d units coordinates -> Bool
plane3dEqualWithin p0 p1 =
    axis3dEqualWithin (Plane3d.normalAxis p0) (Plane3d.normalAxis p1)


rectangle2dEqualWithin : Rectangle2d units coordinates -> Rectangle2d units coordinates -> Bool
rectangle2dEqualWithin r0 r1 =
    (Rectangle2d.dimensions r0 == Rectangle2d.dimensions r1)
        && (Rectangle2d.centerPoint r0 == Rectangle2d.centerPoint r1)
        && axis2dEqualWithin (Rectangle2d.xAxis r0) (Rectangle2d.xAxis r1)
        && axis2dEqualWithin (Rectangle2d.yAxis r0) (Rectangle2d.yAxis r1)


arc2dEqualWithin : Arc2d units coordinates -> Arc2d units coordinates -> Bool
arc2dEqualWithin a0 a1 =
    (Arc2d.startPoint a0 == Arc2d.startPoint a1)
        && (Arc2d.sweptAngle a0 == Arc2d.sweptAngle a1)
        && point2dEqualWithin (Arc2d.endPoint a0) (Arc2d.endPoint a1)


point2dEqualWithin : Point2d units coordinates -> Point2d units coordinates -> Bool
point2dEqualWithin p0 p1 =
    quantityEqualWithin (Point2d.xCoordinate p0) (Point2d.xCoordinate p1)
        && quantityEqualWithin (Point2d.yCoordinate p0) (Point2d.yCoordinate p1)


arc3dEqualWithin : Arc3d units coordinates -> Arc3d units coordinates -> Bool
arc3dEqualWithin a0 a1 =
    (Arc3d.centerPoint a0 == Arc3d.centerPoint a1)
        && (Arc3d.startPoint a0 == Arc3d.startPoint a1)
