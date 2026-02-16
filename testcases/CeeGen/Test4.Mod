(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)

MODULE Test4;
(* Test4: Records with type extension, WITH type guard, IS type test, pointer casting *)
(* No dependencies on other modules *)

TYPE
	Shape = RECORD
		x, y: REAL
	END;
	ShapePtr = POINTER TO Shape;

	Circle = RECORD (Shape)
		radius: REAL
	END;
	CirclePtr = POINTER TO Circle;

	Rect = RECORD (Shape)
		width, height: REAL
	END;
	RectPtr = POINTER TO Rect;

	FilledRect = RECORD (Rect)
		r, g, b: SHORTINT
	END;
	FilledRectPtr = POINTER TO FilledRect;

VAR
	gArea: REAL;
	gCount: LONGINT;

PROCEDURE MakeCircle(cx, cy, r: REAL): CirclePtr;
VAR c: CirclePtr;
BEGIN
	NEW(c);
	c.x := cx;
	c.y := cy;
	c.radius := r;
	RETURN c
END MakeCircle;

PROCEDURE MakeRect(rx, ry, w, h: REAL): RectPtr;
VAR r: RectPtr;
BEGIN
	NEW(r);
	r.x := rx;
	r.y := ry;
	r.width := w;
	r.height := h;
	RETURN r
END MakeRect;

PROCEDURE MakeFilledRect(rx, ry, w, h: REAL; red, green, blue: SHORTINT): FilledRectPtr;
VAR f: FilledRectPtr;
BEGIN
	NEW(f);
	f.x := rx;
	f.y := ry;
	f.width := w;
	f.height := h;
	f.r := red;
	f.g := green;
	f.b := blue;
	RETURN f
END MakeFilledRect;

PROCEDURE Area(s: ShapePtr): REAL;
BEGIN
	IF s IS CirclePtr THEN
		RETURN 3.14159 * s(CirclePtr).radius * s(CirclePtr).radius
	ELSIF s IS RectPtr THEN
		RETURN s(RectPtr).width * s(RectPtr).height
	ELSE
		RETURN 0.0
	END
END Area;

PROCEDURE MoveShape(s: ShapePtr; dx, dy: REAL);
BEGIN
	s.x := s.x + dx;
	s.y := s.y + dy
END MoveShape;

PROCEDURE DescribeArea(s: ShapePtr): REAL;
BEGIN
	WITH s: CirclePtr DO
		RETURN 3.14159 * s.radius * s.radius
	END;
	RETURN 0.0
END DescribeArea;

PROCEDURE TestTypeExtension;
VAR
	c: CirclePtr;
	r: RectPtr;
	f: FilledRectPtr;
	a: REAL;
BEGIN
	c := MakeCircle(0.0, 0.0, 5.0);
	ASSERT(c.x = 0.0);
	ASSERT(c.y = 0.0);
	ASSERT(c.radius = 5.0);

	r := MakeRect(1.0, 1.0, 10.0, 20.0);
	ASSERT(r.x = 1.0);
	ASSERT(r.width = 10.0);
	ASSERT(r.height = 20.0);

	f := MakeFilledRect(0.0, 0.0, 3.0, 4.0, 1, 2, 3);
	ASSERT(f.width = 3.0);
	ASSERT(f.height = 4.0);
	ASSERT(f.r = 1);
	ASSERT(f.g = 2);
	ASSERT(f.b = 3);

	a := Area(c);
	ASSERT(a > 78.0);
	ASSERT(a < 79.0);

	a := Area(r);
	ASSERT(a = 200.0);

	a := Area(f);
	ASSERT(a = 12.0);

	MoveShape(c, 1.0, 2.0);
	ASSERT(c.x = 1.0);
	ASSERT(c.y = 2.0);

	MoveShape(r, -1.0, -2.0);
	ASSERT(r.x = 0.0);
	ASSERT(r.y = -1.0)
END TestTypeExtension;

PROCEDURE TestIsGuard;
VAR
	shapes: ARRAY 4 OF ShapePtr;
	i: LONGINT;
	circles, rects: LONGINT;
BEGIN
	shapes[0] := MakeCircle(0.0, 0.0, 1.0);
	shapes[1] := MakeRect(0.0, 0.0, 2.0, 3.0);
	shapes[2] := MakeCircle(1.0, 1.0, 2.0);
	shapes[3] := MakeFilledRect(0.0, 0.0, 5.0, 5.0, 10, 20, 30);
	circles := 0;
	rects := 0;
	FOR i := 0 TO 3 DO
		IF shapes[i] IS CirclePtr THEN
			INC(circles)
		ELSIF shapes[i] IS RectPtr THEN
			INC(rects)
		END
	END;
	ASSERT(circles = 2);
	ASSERT(rects = 2);
	gCount := circles + rects;
	ASSERT(gCount = 4)
END TestIsGuard;

PROCEDURE TestWith;
VAR
	s: ShapePtr;
	a: REAL;
BEGIN
	s := MakeCircle(0.0, 0.0, 10.0);
	a := DescribeArea(s);
	ASSERT(a > 314.0);
	ASSERT(a < 315.0)
END TestWith;

PROCEDURE TestDeepHierarchy;
VAR
	f: FilledRectPtr;
	r: RectPtr;
	s: ShapePtr;
	a: REAL;
BEGIN
	f := MakeFilledRect(0.0, 0.0, 7.0, 8.0, 5, 10, 15);
	r := f;
	s := f;

	ASSERT(r.width = 7.0);
	ASSERT(r.height = 8.0);

	a := Area(s);
	ASSERT(a = 56.0);

	ASSERT(s IS FilledRectPtr);
	ASSERT(s IS RectPtr);
	ASSERT(s IS ShapePtr);
	ASSERT(~(s IS CirclePtr));

	IF s IS RectPtr THEN
		ASSERT(s(RectPtr).width = 7.0)
	END;

	IF s IS FilledRectPtr THEN
		ASSERT(s(FilledRectPtr).r = 5);
		ASSERT(s(FilledRectPtr).g = 10);
		ASSERT(s(FilledRectPtr).b = 15)
	END
END TestDeepHierarchy;

BEGIN
	TestTypeExtension;
	TestIsGuard;
	TestWith;
	TestDeepHierarchy
END Test4.
