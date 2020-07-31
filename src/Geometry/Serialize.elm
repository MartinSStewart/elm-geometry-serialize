module Geometry.Serialize exposing
    ( arc2d
    , axis2d
    , axis3d
    , block3d
    , boundingBox2d
    , boundingBox3d
    , circle2d
    , circle3d
    , cone3d
    , cubicSpline2d
    , cubicSpline3d
    , cylinder3d
    , direction2d
    , direction3d
    , ellipse2d
    , ellipticalArc2d
    , frame2d
    , frame3d
    , lineSegment2d
    , lineSegment3d
    , plane3d
    , point2d
    , point3d
    , polygon2d
    , polyline2d
    , polyline3d
    , quadraticSpline2d
    , quadraticSpline3d
    , rectangle2d
    , rectangle3d
    , sketchPlane3d
    , sphere3d
    , triangle2d
    , triangle3d
    , vector2d
    , vector3d
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Rectangle3d exposing (Rectangle3d)
import Serialize as S
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


quantity : S.Codec e (Quantity Float units)
quantity =
    S.float |> S.map Quantity.Quantity (\(Quantity.Quantity a) -> a)


{-| Codec for [Arc2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Arc2d)

**Warning:** Due to rounding errors, the Arc2d you get after decoding will be slightly different from what you encoded.

-}
arc2d : S.Codec e (Arc2d units coordinates)
arc2d =
    S.record
        (\startPoint sweptAngle p1 ->
            if isLargeAngle sweptAngle then
                Arc2d.sweptAround p1 sweptAngle startPoint

            else
                Arc2d.from startPoint p1 sweptAngle
        )
        |> S.field Arc2d.startPoint point2d
        |> S.field Arc2d.sweptAngle quantity
        |> S.field
            (\arc ->
                if isLargeAngle (Arc2d.sweptAngle arc) then
                    Arc2d.centerPoint arc

                else
                    Arc2d.endPoint arc
            )
            point2d
        |> S.finishRecord


isLargeAngle : Angle -> Bool
isLargeAngle =
    Quantity.abs >> Quantity.greaterThan (Angle.degrees 90)


{-| Codec for [Arc3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Arc3d)

Don't use this, it's numerically unstable.

-}
arc3d : S.Codec e (Arc3d units coordinates)
arc3d =
    S.record Arc3d.sweptAround
        |> S.field Arc3d.axis axis3d
        |> S.field Arc3d.sweptAngle quantity
        |> S.field Arc3d.startPoint point3d
        |> S.finishRecord


{-| Codec for [Axis2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Axis2d)
-}
axis2d : S.Codec e (Axis2d units coordinates)
axis2d =
    S.record Axis2d.withDirection
        |> S.field Axis2d.direction direction2d
        |> S.field Axis2d.originPoint point2d
        |> S.finishRecord


{-| Codec for [Axis3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Axis3d)
-}
axis3d : S.Codec e (Axis3d units coordinates)
axis3d =
    S.record Axis3d.withDirection
        |> S.field Axis3d.direction direction3d
        |> S.field Axis3d.originPoint point3d
        |> S.finishRecord


{-| Codec for [Block3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Block3d)
-}
block3d : S.Codec e (Block3d units coordinates)
block3d =
    S.record Block3d.centeredOn
        |> S.field Block3d.axes frame3d
        |> S.field Block3d.dimensions (S.triple quantity quantity quantity)
        |> S.finishRecord


{-| Codec for [BoundingBox2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/BoundingBox2d)
-}
boundingBox2d : S.Codec e (BoundingBox2d units coordinates)
boundingBox2d =
    S.record (\minX minY maxX maxY -> { minX = minX, minY = minY, maxX = maxX, maxY = maxY })
        |> S.field .minX quantity
        |> S.field .minY quantity
        |> S.field .maxX quantity
        |> S.field .maxY quantity
        |> S.finishRecord
        |> S.map BoundingBox2d.fromExtrema BoundingBox2d.extrema


{-| Codec for [BoundingBox3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/BoundingBox3d)
-}
boundingBox3d : S.Codec e (BoundingBox3d units coordinates)
boundingBox3d =
    S.record
        (\minX minY minZ maxX maxY maxZ ->
            { minX = minX, minY = minY, minZ = minZ, maxX = maxX, maxY = maxY, maxZ = maxZ }
        )
        |> S.field .minX quantity
        |> S.field .minY quantity
        |> S.field .minZ quantity
        |> S.field .maxX quantity
        |> S.field .maxY quantity
        |> S.field .maxZ quantity
        |> S.finishRecord
        |> S.map BoundingBox3d.fromExtrema BoundingBox3d.extrema


{-| Codec for [Circle2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Circle2d)
-}
circle2d : S.Codec e (Circle2d units coordinates)
circle2d =
    S.record Circle2d.withRadius
        |> S.field Circle2d.radius quantity
        |> S.field Circle2d.centerPoint point2d
        |> S.finishRecord


{-| Codec for [Circle3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Circle3d)
-}
circle3d : S.Codec e (Circle3d units coordinates)
circle3d =
    S.record Circle3d.withRadius
        |> S.field Circle3d.radius quantity
        |> S.field Circle3d.axialDirection direction3d
        |> S.field Circle3d.centerPoint point3d
        |> S.finishRecord


{-| Codec for [Cone3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Cone3d)
-}
cone3d : S.Codec e (Cone3d units coordinates)
cone3d =
    S.record
        (\basePoint direction radius length ->
            Cone3d.startingAt basePoint direction { radius = radius, length = length }
        )
        |> S.field Cone3d.basePoint point3d
        |> S.field Cone3d.axialDirection direction3d
        |> S.field Cone3d.radius quantity
        |> S.field Cone3d.length quantity
        |> S.finishRecord


{-| Codec for [CubicSpline2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/CubicSpline2d)
-}
cubicSpline2d : S.Codec e (CubicSpline2d units coordinates)
cubicSpline2d =
    S.record CubicSpline2d.fromControlPoints
        |> S.field CubicSpline2d.firstControlPoint point2d
        |> S.field CubicSpline2d.secondControlPoint point2d
        |> S.field CubicSpline2d.thirdControlPoint point2d
        |> S.field CubicSpline2d.fourthControlPoint point2d
        |> S.finishRecord


{-| Codec for [CubicSpline3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/CubicSpline3d)
-}
cubicSpline3d : S.Codec e (CubicSpline3d units coordinates)
cubicSpline3d =
    S.record CubicSpline3d.fromControlPoints
        |> S.field CubicSpline3d.firstControlPoint point3d
        |> S.field CubicSpline3d.secondControlPoint point3d
        |> S.field CubicSpline3d.thirdControlPoint point3d
        |> S.field CubicSpline3d.fourthControlPoint point3d
        |> S.finishRecord


{-| Codec for [Cylinder3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Cylinder3d)
-}
cylinder3d : S.Codec e (Cylinder3d.Cylinder3d units coordinates)
cylinder3d =
    S.record
        (\centerPoint axialDirection radius length ->
            Cylinder3d.centeredOn centerPoint axialDirection { radius = radius, length = length }
        )
        |> S.field Cylinder3d.centerPoint point3d
        |> S.field Cylinder3d.axialDirection direction3d
        |> S.field Cylinder3d.radius quantity
        |> S.field Cylinder3d.length quantity
        |> S.finishRecord


{-| Codec for [Direction2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Direction2d)
-}
direction2d : S.Codec e (Direction2d coordinates)
direction2d =
    S.record
        (\x y ->
            if abs (x ^ 2 + y ^ 2 - 1) < 0.0000000000001 then
                Direction2d.unsafe { x = x, y = y }

            else
                Vector2d.xy (Quantity.Quantity x) (Quantity.Quantity y)
                    |> Vector2d.direction
                    |> Maybe.withDefault Direction2d.x
        )
        |> S.field (Direction2d.unwrap >> .x) S.float
        |> S.field (Direction2d.unwrap >> .y) S.float
        |> S.finishRecord


{-| Codec for [Direction3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Direction3d)
-}
direction3d : S.Codec e (Direction3d coordinates)
direction3d =
    S.record
        (\x y z ->
            if abs (x ^ 2 + y ^ 2 + z ^ 2 - 1) < 0.0000000000001 then
                Direction3d.unsafe { x = x, y = y, z = z }

            else
                Vector3d.xyz (Quantity.Quantity x) (Quantity.Quantity y) (Quantity.Quantity z)
                    |> Vector3d.direction
                    |> Maybe.withDefault Direction3d.x
        )
        |> S.field (Direction3d.unwrap >> .x) S.float
        |> S.field (Direction3d.unwrap >> .y) S.float
        |> S.field (Direction3d.unwrap >> .z) S.float
        |> S.finishRecord


{-| Codec for [Ellipse2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Ellipse2d)
-}
ellipse2d : S.Codec e (Ellipse2d units coordinates)
ellipse2d =
    S.record
        (\centerPoint xDirection xRadius yRadius ->
            Ellipse2d.with
                { centerPoint = centerPoint, xDirection = xDirection, xRadius = xRadius, yRadius = yRadius }
        )
        |> S.field Ellipse2d.centerPoint point2d
        |> S.field Ellipse2d.xDirection direction2d
        |> S.field Ellipse2d.xRadius quantity
        |> S.field Ellipse2d.yRadius quantity
        |> S.finishRecord


{-| Codec for [EllipticalArc2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/EllipticalArc2d)
-}
ellipticalArc2d : S.Codec e (EllipticalArc2d units coordinates)
ellipticalArc2d =
    S.record
        (\centerPoint xDirection xRadius yRadius startAngle sweptAngle ->
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
        )
        |> S.field EllipticalArc2d.centerPoint point2d
        |> S.field EllipticalArc2d.xDirection direction2d
        |> S.field EllipticalArc2d.xRadius quantity
        |> S.field EllipticalArc2d.yRadius quantity
        |> S.field EllipticalArc2d.startAngle quantity
        |> S.field EllipticalArc2d.sweptAngle quantity
        |> S.finishRecord


{-| Codec for [Frame2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Frame2d)
-}
frame2d : S.Codec e (Frame2d units coordinates defines)
frame2d =
    S.record
        (\position yDirection isRightHanded ->
            { position = position, yDirection = yDirection, isRightHanded = isRightHanded }
        )
        |> S.field .position point2d
        |> S.field .yDirection direction2d
        |> S.field .isRightHanded S.bool
        |> S.finishRecord
        |> S.map
            (\{ position, yDirection, isRightHanded } ->
                let
                    frame =
                        Frame2d.withYDirection yDirection position
                in
                if isRightHanded then
                    frame

                else
                    Frame2d.reverseX frame
            )
            (\frame ->
                { position = Frame2d.originPoint frame
                , yDirection = Frame2d.yDirection frame
                , isRightHanded = Frame2d.isRightHanded frame
                }
            )


{-| Codec for [Frame3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Frame3d)
-}
frame3d : S.Codec e (Frame3d units coordinates defines)
frame3d =
    S.record
        (\originPoint xDirection yDirection zDirection ->
            let
                angleDiff0 =
                    Direction3d.angleFrom xDirection yDirection

                angleDiff1 =
                    Direction3d.angleFrom xDirection zDirection

                angleDiff2 =
                    Direction3d.angleFrom zDirection yDirection
            in
            if isRightAngle angleDiff0 && isRightAngle angleDiff1 && isRightAngle angleDiff2 then
                Frame3d.unsafe
                    { originPoint = originPoint
                    , xDirection = xDirection
                    , yDirection = yDirection
                    , zDirection = zDirection
                    }

            else
                case Direction3d.orthogonalize xDirection yDirection zDirection of
                    Just ( xDir, yDir, zDir ) ->
                        Frame3d.unsafe
                            { originPoint = originPoint
                            , xDirection = xDir
                            , yDirection = yDir
                            , zDirection = zDir
                            }

                    Nothing ->
                        Frame3d.withXDirection xDirection originPoint
        )
        |> S.field Frame3d.originPoint point3d
        |> S.field Frame3d.xDirection direction3d
        |> S.field Frame3d.yDirection direction3d
        |> S.field Frame3d.zDirection direction3d
        |> S.finishRecord


isRightAngle : Angle -> Bool
isRightAngle angle =
    (angle |> Quantity.lessThan (Angle.degrees 90.000000000001))
        && (angle |> Quantity.greaterThan (Angle.degrees 89.999999999999))


{-| Codec for [LineSegment2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/LineSegment2d)
-}
lineSegment2d : S.Codec e (LineSegment2d units coordinates)
lineSegment2d =
    S.record LineSegment2d.from
        |> S.field LineSegment2d.startPoint point2d
        |> S.field LineSegment2d.endPoint point2d
        |> S.finishRecord


{-| Codec for [LineSegment3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/LineSegment3d)
-}
lineSegment3d : S.Codec e (LineSegment3d units coordinates)
lineSegment3d =
    S.record LineSegment3d.from
        |> S.field LineSegment3d.startPoint point3d
        |> S.field LineSegment3d.endPoint point3d
        |> S.finishRecord


{-| Codec for [Plane3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Plane3d)
-}
plane3d : S.Codec e (Plane3d units coordinates)
plane3d =
    S.record Plane3d.through
        |> S.field Plane3d.originPoint point3d
        |> S.field Plane3d.normalDirection direction3d
        |> S.finishRecord


{-| Codec for [Point2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point2d)
-}
point2d : S.Codec e (Point2d units coordinates)
point2d =
    S.record Point2d.xy
        |> S.field Point2d.xCoordinate quantity
        |> S.field Point2d.yCoordinate quantity
        |> S.finishRecord


{-| Codec for [Point3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point3d)
-}
point3d : S.Codec e (Point3d units coordinates)
point3d =
    S.record Point3d.xyz
        |> S.field Point3d.xCoordinate quantity
        |> S.field Point3d.yCoordinate quantity
        |> S.field Point3d.zCoordinate quantity
        |> S.finishRecord


{-| Codec for [Polygon2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Polygon2d)
-}
polygon2d : S.Codec e (Polygon2d units coordinates)
polygon2d =
    S.record Polygon2d.withHoles
        |> S.field Polygon2d.innerLoops (S.list (S.list point2d))
        |> S.field Polygon2d.outerLoop (S.list point2d)
        |> S.finishRecord


{-| Codec for [Polyline2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Polyline2d)
-}
polyline2d : S.Codec e (Polyline2d units coordinates)
polyline2d =
    S.record Polyline2d.fromVertices
        |> S.field Polyline2d.vertices (S.list point2d)
        |> S.finishRecord


{-| Codec for [Polyline3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Polyline3d)
-}
polyline3d : S.Codec e (Polyline3d units coordinates)
polyline3d =
    S.record Polyline3d.fromVertices
        |> S.field Polyline3d.vertices (S.list point3d)
        |> S.finishRecord


{-| Codec for [QuadraticSpline2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/QuadraticSpline2d)
-}
quadraticSpline2d : S.Codec e (QuadraticSpline2d units coordinates)
quadraticSpline2d =
    S.record QuadraticSpline2d.fromControlPoints
        |> S.field QuadraticSpline2d.firstControlPoint point2d
        |> S.field QuadraticSpline2d.secondControlPoint point2d
        |> S.field QuadraticSpline2d.thirdControlPoint point2d
        |> S.finishRecord


{-| Codec for [QuadraticSpline3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/QuadraticSpline3d)
-}
quadraticSpline3d : S.Codec e (QuadraticSpline3d units coordinates)
quadraticSpline3d =
    S.record QuadraticSpline3d.fromControlPoints
        |> S.field QuadraticSpline3d.firstControlPoint point3d
        |> S.field QuadraticSpline3d.secondControlPoint point3d
        |> S.field QuadraticSpline3d.thirdControlPoint point3d
        |> S.finishRecord


{-| Codec for [Rectangle2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Rectangle2d)
-}
rectangle2d : S.Codec e (Rectangle2d units coordinates)
rectangle2d =
    S.record Rectangle2d.centeredOn
        |> S.field Rectangle2d.axes frame2d
        |> S.field Rectangle2d.dimensions (S.tuple quantity quantity)
        |> S.finishRecord


{-| Codec for [Rectangle3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Rectangle3d)
-}
rectangle3d : S.Codec e (Rectangle3d units coordinates)
rectangle3d =
    S.record Rectangle3d.centeredOn
        |> S.field Rectangle3d.axes sketchPlane3d
        |> S.field Rectangle3d.dimensions (S.tuple quantity quantity)
        |> S.finishRecord


{-| Codec for [SketchPlane3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/SketchPlane3d)
-}
sketchPlane3d : S.Codec e (SketchPlane3d units coordinates defines)
sketchPlane3d =
    S.record
        (\position xDirection yDirection ->
            let
                angleDiff =
                    Direction3d.angleFrom xDirection yDirection
            in
            if isRightAngle angleDiff then
                SketchPlane3d.unsafe { originPoint = position, xDirection = xDirection, yDirection = yDirection }

            else
                SketchPlane3d.unsafe
                    { originPoint = position
                    , xDirection = xDirection
                    , yDirection = Direction3d.perpendicularTo xDirection
                    }
        )
        |> S.field SketchPlane3d.originPoint point3d
        |> S.field SketchPlane3d.xDirection direction3d
        |> S.field SketchPlane3d.yDirection direction3d
        |> S.finishRecord


{-| Codec for [Sphere3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Sphere3d)
-}
sphere3d : S.Codec e (Sphere3d units coordinates)
sphere3d =
    S.record Sphere3d.withRadius
        |> S.field Sphere3d.radius quantity
        |> S.field Sphere3d.centerPoint point3d
        |> S.finishRecord


{-| Codec for [Triangle2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Triangle2d)
-}
triangle2d : S.Codec e (Triangle2d units coordinates)
triangle2d =
    S.triple point2d point2d point2d |> S.map Triangle2d.fromVertices Triangle2d.vertices


{-| Codec for [Triangle3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Triangle3d)
-}
triangle3d : S.Codec e (Triangle3d units coordinates)
triangle3d =
    S.triple point3d point3d point3d |> S.map Triangle3d.fromVertices Triangle3d.vertices


{-| Codec for [Vector2d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Vector2d)
-}
vector2d : S.Codec e (Vector2d units coordinates)
vector2d =
    S.record Vector2d.xy
        |> S.field Vector2d.xComponent quantity
        |> S.field Vector2d.yComponent quantity
        |> S.finishRecord


{-| Codec for [Vector3d](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Vector3d)
-}
vector3d : S.Codec e (Vector3d units coordinates)
vector3d =
    S.record Vector3d.xyz
        |> S.field Vector3d.xComponent quantity
        |> S.field Vector3d.yComponent quantity
        |> S.field Vector3d.zComponent quantity
        |> S.finishRecord
