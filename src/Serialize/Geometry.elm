module Serialize.Geometry exposing (axis2d, axis3d, boundingBox2d, boundingBox3d, circle2d, circle3d, cone3d, cubicSpline2d, cubicSpline3d, cylinder3d, delaunayTriangulation2d, direction2d, direction3d, ellipse2d, ellipticalArc2d, frame2d, lineSegment2d, lineSegment3d, plane3d, point2d, point3d, polygon2d, polyline2d, polyline3d, quadraticSpline2d, quadraticSpline3d, rectangle2d, rectangle3d, sketchPlane3d, sphere3d, triangle2d, triangle3d, vector2d, vector3d, voronoiDiagram2d)

import ArcLengthParameterization
import Axis2d
import Axis3d
import Block3d exposing (Block3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d
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
import VoronoiDiagram2d exposing (VoronoiDiagram2d)


quantity : S.Codec e (Quantity Float units)
quantity =
    S.float |> S.map Quantity.Quantity (\(Quantity.Quantity a) -> a)



--arc2d =
--    S.record (\)
--        |> S.field
--        |> S.finishRecord
--
--arc3d =
--    S.record (\)
--        |> S.field
--        |> S.finishRecord
--arcLengthParameterization =
--    S.record ArcLengthParameterization.build
--        |> S.field
--        |> S.finishRecord


axis2d : S.Codec e (Axis2d.Axis2d units coordinates)
axis2d =
    S.record Axis2d.withDirection
        |> S.field Axis2d.direction direction2d
        |> S.field Axis2d.originPoint point2d
        |> S.finishRecord


axis3d : S.Codec e (Axis3d.Axis3d units coordinates)
axis3d =
    S.record Axis3d.withDirection
        |> S.field Axis3d.direction direction3d
        |> S.field Axis3d.originPoint point3d
        |> S.finishRecord



--
--block3d =
--    S.record (\)
--        |> S.field
--        |> S.finishRecord


boundingBox2d : S.Codec e (BoundingBox2d units coordinates)
boundingBox2d =
    S.record (\minX minY maxX maxY -> { minX = minX, minY = minY, maxX = maxX, maxY = maxY })
        |> S.field .minX quantity
        |> S.field .minY quantity
        |> S.field .maxX quantity
        |> S.field .maxY quantity
        |> S.finishRecord
        |> S.map BoundingBox2d.fromExtrema BoundingBox2d.extrema


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


circle2d : S.Codec e (Circle2d units coordinates)
circle2d =
    S.record Circle2d.withRadius
        |> S.field Circle2d.radius quantity
        |> S.field Circle2d.centerPoint point2d
        |> S.finishRecord


circle3d : S.Codec e (Circle3d units coordinates)
circle3d =
    S.record Circle3d.withRadius
        |> S.field Circle3d.radius quantity
        |> S.field Circle3d.axialDirection direction3d
        |> S.field Circle3d.centerPoint point3d
        |> S.finishRecord


cone3d : S.Codec e (Cone3d units coordinates)
cone3d =
    S.record Cone3d.from
        |> S.field Cone3d.basePoint point3d
        |> S.field Cone3d.tipPoint point3d
        |> S.field Cone3d.radius quantity
        |> S.finishRecord


cubicSpline2d : S.Codec e (CubicSpline2d units coordinates)
cubicSpline2d =
    S.record CubicSpline2d.fromControlPoints
        |> S.field CubicSpline2d.firstControlPoint point2d
        |> S.field CubicSpline2d.secondControlPoint point2d
        |> S.field CubicSpline2d.thirdControlPoint point2d
        |> S.field CubicSpline2d.fourthControlPoint point2d
        |> S.finishRecord


cubicSpline3d : S.Codec e (CubicSpline3d units coordinates)
cubicSpline3d =
    S.record CubicSpline3d.fromControlPoints
        |> S.field CubicSpline3d.firstControlPoint point3d
        |> S.field CubicSpline3d.secondControlPoint point3d
        |> S.field CubicSpline3d.thirdControlPoint point3d
        |> S.field CubicSpline3d.fourthControlPoint point3d
        |> S.finishRecord


cylinder3d : S.Codec e (Cylinder3d.Cylinder3d units coordinates)
cylinder3d =
    S.record Cylinder3d.centeredOn
        |> S.field Cylinder3d.centerPoint point3d
        |> S.field Cylinder3d.axialDirection direction3d
        |> S.field Cylinder3d.radius quantity
        |> S.finishRecord


delaunayTriangulation2d : S.Codec (DelaunayTriangulation2d.Error (Point2d units coordinates)) (DelaunayTriangulation2d vertex units coordinates)
delaunayTriangulation2d =
    S.array point2d |> S.mapValid DelaunayTriangulation2d.fromPoints DelaunayTriangulation2d.vertices


direction2d : S.Codec e (Direction2d coordinates)
direction2d =
    quantity |> S.map Direction2d.fromAngle Direction2d.toAngle


direction3d : S.Codec e (Direction3d coordinates)
direction3d =
    S.tuple quantity quantity
        |> S.map
            (\( azimuth, elevation ) -> Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy azimuth elevation)
            (\direction ->
                ( Direction3d.azimuthIn SketchPlane3d.xy direction
                , Direction3d.elevationFrom SketchPlane3d.xy direction
                )
            )


ellipse2d : S.Codec e (Ellipse2d units coordinates)
ellipse2d =
    S.record (\centerPoint xDirection xRadius yRadius -> Ellipse2d.with { centerPoint = centerPoint, xDirection = xDirection, xRadius = xRadius, yRadius = yRadius })
        |> S.field Ellipse2d.centerPoint point2d
        |> S.field Ellipse2d.xDirection direction2d
        |> S.field Ellipse2d.xRadius quantity
        |> S.field Ellipse2d.yRadius quantity
        |> S.finishRecord


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


frame2d : S.Codec e (Frame2d units coordinates defines)
frame2d =
    S.record (\position yDirection isRightHanded -> { position = position, yDirection = yDirection, isRightHanded = isRightHanded })
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
                    Frame2d.mirrorAcross (Frame2d.yAxis frame) frame
            )
            (\frame ->
                { position = Frame2d.originPoint frame
                , yDirection = Frame2d.yDirection frame
                , isRightHanded = Frame2d.isRightHanded frame
                }
            )



--frame3d =
--    S.record (Frame3d.)
--        |> S.field
--        |> S.finishRecord


lineSegment2d : S.Codec e (LineSegment2d units coordinates)
lineSegment2d =
    S.record LineSegment2d.from
        |> S.field LineSegment2d.startPoint point2d
        |> S.field LineSegment2d.endPoint point2d
        |> S.finishRecord


lineSegment3d : S.Codec e (LineSegment3d units coordinates)
lineSegment3d =
    S.record LineSegment3d.from
        |> S.field LineSegment3d.startPoint point3d
        |> S.field LineSegment3d.endPoint point3d
        |> S.finishRecord


plane3d : S.Codec e (Plane3d units coordinates)
plane3d =
    S.record Plane3d.through
        |> S.field Plane3d.originPoint point3d
        |> S.field Plane3d.normalDirection direction3d
        |> S.finishRecord


point2d : S.Codec e (Point2d units coordinates)
point2d =
    S.record Point2d.xy
        |> S.field Point2d.xCoordinate quantity
        |> S.field Point2d.yCoordinate quantity
        |> S.finishRecord


point3d : S.Codec e (Point3d units coordinates)
point3d =
    S.record Point3d.xyz
        |> S.field Point3d.xCoordinate quantity
        |> S.field Point3d.yCoordinate quantity
        |> S.field Point3d.zCoordinate quantity
        |> S.finishRecord


polygon2d : S.Codec e (Polygon2d units coordinates)
polygon2d =
    S.record Polygon2d.withHoles
        |> S.field Polygon2d.innerLoops (S.list (S.list point2d))
        |> S.field Polygon2d.outerLoop (S.list point2d)
        |> S.finishRecord


polyline2d : S.Codec e (Polyline2d units coordinates)
polyline2d =
    S.list point2d |> S.map Polyline2d.fromVertices Polyline2d.vertices


polyline3d : S.Codec e (Polyline3d units coordinates)
polyline3d =
    S.list point3d |> S.map Polyline3d.fromVertices Polyline3d.vertices


quadraticSpline2d : S.Codec e (QuadraticSpline2d units coordinates)
quadraticSpline2d =
    S.record QuadraticSpline2d.fromControlPoints
        |> S.field QuadraticSpline2d.firstControlPoint point2d
        |> S.field QuadraticSpline2d.secondControlPoint point2d
        |> S.field QuadraticSpline2d.thirdControlPoint point2d
        |> S.finishRecord


quadraticSpline3d : S.Codec e (QuadraticSpline3d units coordinates)
quadraticSpline3d =
    S.record QuadraticSpline3d.fromControlPoints
        |> S.field QuadraticSpline3d.firstControlPoint point3d
        |> S.field QuadraticSpline3d.secondControlPoint point3d
        |> S.field QuadraticSpline3d.thirdControlPoint point3d
        |> S.finishRecord


rectangle2d : S.Codec e (Rectangle2d units coordinates)
rectangle2d =
    S.record Rectangle2d.withYAxis
        |> S.field Rectangle2d.yAxis axis2d
        |> S.field Rectangle2d.dimensions (S.tuple quantity quantity)
        |> S.finishRecord



--rectangle3d : S.Codec e (Rectangle3d units coordinates)
--rectangle3d =
--    S.record Rectangle3d.centeredOn
--        |> S.field Rectangle3d.yAxis axis3d
--        |> S.field Rectangle3d.dimensions (S.tuple quantity quantity)
--        |> S.finishRecord


sketchPlane3d : S.Codec e (SketchPlane3d units coordinates defines)
sketchPlane3d =
    S.record SketchPlane3d.through
        |> S.field SketchPlane3d.originPoint point3d
        |> S.field SketchPlane3d.normalDirection direction3d
        |> S.finishRecord


sphere3d : S.Codec e (Sphere3d units coordinates)
sphere3d =
    S.record Sphere3d.withRadius
        |> S.field Sphere3d.radius quantity
        |> S.field Sphere3d.centerPoint point3d
        |> S.finishRecord


triangle2d : S.Codec e (Triangle2d units coordinates)
triangle2d =
    S.triple point2d point2d point2d |> S.map Triangle2d.fromVertices Triangle2d.vertices


triangle3d : S.Codec e (Triangle3d units coordinates)
triangle3d =
    S.triple point3d point3d point3d |> S.map Triangle3d.fromVertices Triangle3d.vertices


vector2d : S.Codec e (Vector2d units coordinates)
vector2d =
    S.record Vector2d.xy
        |> S.field Vector2d.xComponent quantity
        |> S.field Vector2d.yComponent quantity
        |> S.finishRecord


vector3d : S.Codec e (Vector3d units coordinates)
vector3d =
    S.record Vector3d.xyz
        |> S.field Vector3d.xComponent quantity
        |> S.field Vector3d.yComponent quantity
        |> S.field Vector3d.zComponent quantity
        |> S.finishRecord


voronoiDiagram2d : S.Codec (VoronoiDiagram2d.Error (Point2d units coordinates)) (VoronoiDiagram2d (Point2d units coordinates) units coordinates)
voronoiDiagram2d =
    S.array point2d |> S.mapValid VoronoiDiagram2d.fromPoints VoronoiDiagram2d.vertices
