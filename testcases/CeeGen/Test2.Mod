(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)

MODULE Test2;
(* Test2: Arrays (fixed), records, pointers, NEW, structured assignment *)
(* No dependencies on other modules *)

TYPE
	Vector3 = ARRAY 3 OF REAL;
	Matrix2 = ARRAY 2, 3 OF LONGINT;
	Name = ARRAY 32 OF CHAR;

	Point = RECORD
		x, y: REAL
	END;

	Color = RECORD
		r, g, b: SHORTINT
	END;

	Vertex = RECORD
		pos: Point;
		col: Color;
		label: Name
	END;

	PointPtr = POINTER TO Point;
	VertexPtr = POINTER TO Vertex;

	Node = RECORD
		value: LONGINT;
		next: POINTER TO Node
	END;
	NodePtr = POINTER TO Node;

VAR
	gp: Point;
	gv: Vertex;

PROCEDURE TestFixedArray;
VAR
	a: ARRAY 10 OF LONGINT;
	v: Vector3;
	i: LONGINT;
	sum: LONGINT;
BEGIN
	FOR i := 0 TO 9 DO
		a[i] := i * i
	END;
	ASSERT(a[0] = 0);
	ASSERT(a[1] = 1);
	ASSERT(a[3] = 9);
	ASSERT(a[9] = 81);
	sum := 0;
	FOR i := 0 TO 9 DO
		sum := sum + a[i]
	END;
	ASSERT(sum = 285);
	v[0] := 1.0;
	v[1] := 2.0;
	v[2] := 3.0;
	ASSERT(v[0] = 1.0);
	ASSERT(v[1] = 2.0);
	ASSERT(v[2] = 3.0)
END TestFixedArray;

PROCEDURE TestMultiDimArray;
VAR
	m: Matrix2;
	i, j: LONGINT;
	sum: LONGINT;
BEGIN
	FOR i := 0 TO 1 DO
		FOR j := 0 TO 2 DO
			m[i, j] := i * 3 + j
		END
	END;
	ASSERT(m[0, 0] = 0);
	ASSERT(m[0, 2] = 2);
	ASSERT(m[1, 0] = 3);
	ASSERT(m[1, 2] = 5);
	sum := 0;
	FOR i := 0 TO 1 DO
		FOR j := 0 TO 2 DO
			sum := sum + m[i, j]
		END
	END;
	ASSERT(sum = 15)
END TestMultiDimArray;

PROCEDURE TestRecord;
VAR
	p: Point;
	c: Color;
BEGIN
	p.x := 1.5;
	p.y := 2.5;
	ASSERT(p.x = 1.5);
	ASSERT(p.y = 2.5);
	c.r := 10;
	c.g := 20;
	c.b := 30;
	ASSERT(c.r = 10);
	ASSERT(c.g = 20);
	ASSERT(c.b = 30);
	gp := p;
	ASSERT(gp.x = 1.5);
	ASSERT(gp.y = 2.5)
END TestRecord;

PROCEDURE TestNestedRecord;
VAR
	v: Vertex;
BEGIN
	v.pos.x := 10.0;
	v.pos.y := 20.0;
	v.col.r := 1;
	v.col.g := 2;
	v.col.b := 3;
	ASSERT(v.pos.x = 10.0);
	ASSERT(v.pos.y = 20.0);
	ASSERT(v.col.r = 1);
	ASSERT(v.col.g = 2);
	ASSERT(v.col.b = 3);
	gv := v;
	ASSERT(gv.pos.x = 10.0);
	ASSERT(gv.col.b = 3)
END TestNestedRecord;

PROCEDURE TestPointerToRecord;
VAR
	p: PointPtr;
BEGIN
	NEW(p);
	p.x := 42.0;
	p.y := 84.0;
	ASSERT(p.x = 42.0);
	ASSERT(p.y = 84.0);
	gp := p^;
	ASSERT(gp.x = 42.0);
	ASSERT(gp.y = 84.0)
END TestPointerToRecord;

PROCEDURE TestLinkedList;
VAR
	head, cur, n: NodePtr;
	i: LONGINT;
	sum: LONGINT;
	count: LONGINT;
BEGIN
	head := NIL;
	FOR i := 1 TO 5 DO
		NEW(n);
		n.value := i;
		n.next := head;
		head := n
	END;
	ASSERT(head # NIL);
	ASSERT(head.value = 5);
	sum := 0;
	count := 0;
	cur := head;
	WHILE cur # NIL DO
		sum := sum + cur.value;
		INC(count);
		cur := cur.next
	END;
	ASSERT(sum = 15);
	ASSERT(count = 5)
END TestLinkedList;

PROCEDURE TestRecordAssignment;
VAR
	p1, p2: Point;
BEGIN
	p1.x := 1.0;
	p1.y := 2.0;
	p2 := p1;
	ASSERT(p2.x = 1.0);
	ASSERT(p2.y = 2.0);
	p1.x := 99.0;
	ASSERT(p2.x = 1.0)
END TestRecordAssignment;

PROCEDURE TestArrayAssignment;
VAR
	a, b: ARRAY 5 OF LONGINT;
	i: LONGINT;
BEGIN
	FOR i := 0 TO 4 DO
		a[i] := i + 1
	END;
	b := a;
	ASSERT(b[0] = 1);
	ASSERT(b[4] = 5);
	a[0] := 99;
	ASSERT(b[0] = 1)
END TestArrayAssignment;

PROCEDURE TestCharArray;
VAR
	s: Name;
BEGIN
	s := "Hello World";
	ASSERT(s[0] = "H");
	ASSERT(s[1] = "e");
	ASSERT(s[5] = " ");
	gv.label := s;
	ASSERT(gv.label[0] = "H")
END TestCharArray;

BEGIN
	TestFixedArray;
	TestMultiDimArray;
	TestRecord;
	TestNestedRecord;
	TestPointerToRecord;
	TestLinkedList;
	TestRecordAssignment;
	TestArrayAssignment;
	TestCharArray
END Test2.
