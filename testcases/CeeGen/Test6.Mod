(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)

MODULE Test6;
(* Test6: Regression tests for transpiler fixes *)
(* Covers fixes from OP2 compiler error analysis and Awfy benchmark fixes *)
(* No dependencies on other modules except SYSTEM *)

IMPORT SYSTEM;

TYPE
	(* C keyword field escaping - field names that are C keywords *)
	NodeDesc = RECORD
		struct: LONGINT;
		union: LONGINT;
		register: LONGINT;
		short: LONGINT;
		unsigned: LONGINT
	END;

	(* Pointer to fixed array types *)
	IntArray = ARRAY 10 OF LONGINT;
	IntArrayPtr = POINTER TO IntArray;
	CharBuf = ARRAY 64 OF CHAR;
	CharBufPtr = POINTER TO CharBuf;

	(* Pointer upcasting in type hierarchies *)
	Base = RECORD
		id: LONGINT
	END;
	BasePtr = POINTER TO Base;
	Derived = RECORD (Base)
		value: REAL
	END;
	DerivedPtr = POINTER TO Derived;
	Derived2 = RECORD (Derived)
		extra: LONGINT
	END;
	Derived2Ptr = POINTER TO Derived2;

VAR
	gResult: LONGINT;

(* C keyword field escaping *)

PROCEDURE TestKeywordFields;
VAR n: NodeDesc;
BEGIN
	n.struct := 42;
	n.union := 100;
	n.register := -1;
	n.short := 7;
	n.unsigned := 255;
	ASSERT(n.struct = 42);
	ASSERT(n.union = 100);
	ASSERT(n.register = -1);
	ASSERT(n.short = 7);
	ASSERT(n.unsigned = 255);
	n.struct := n.struct + n.union;
	ASSERT(n.struct = 142);
	n.register := n.short * n.unsigned;
	ASSERT(n.register = 1785)
END TestKeywordFields;

(* Pointer-to-fixed-array deref and indexing *)

PROCEDURE TestPtrToFixedArray;
VAR
	p: IntArrayPtr;
	i, sum: LONGINT;
BEGIN
	NEW(p);
	FOR i := 0 TO 9 DO
		p[i] := (i + 1) * 10
	END;
	ASSERT(p[0] = 10);
	ASSERT(p[4] = 50);
	ASSERT(p[9] = 100);
	sum := 0;
	FOR i := 0 TO 9 DO
		sum := sum + p[i]
	END;
	ASSERT(sum = 550);
	p[0] := p[0] + p[9];
	ASSERT(p[0] = 110)
END TestPtrToFixedArray;

(* Fixed array via pointer passed to open array param *)

PROCEDURE SumOpen(VAR a: ARRAY OF LONGINT): LONGINT;
VAR i, s: LONGINT;
BEGIN
	s := 0;
	FOR i := 0 TO LEN(a) - 1 DO
		s := s + a[i]
	END;
	RETURN s
END SumOpen;

PROCEDURE FillOpen(VAR a: ARRAY OF LONGINT; val: LONGINT);
VAR i: LONGINT;
BEGIN
	FOR i := 0 TO LEN(a) - 1 DO
		a[i] := val
	END
END FillOpen;

PROCEDURE TestPtrArrayToOpenParam;
VAR
	p: IntArrayPtr;
	i, s: LONGINT;
BEGIN
	NEW(p);
	FOR i := 0 TO 9 DO
		p[i] := i + 1
	END;
	s := SumOpen(p^);
	ASSERT(s = 55);
	FillOpen(p^, 7);
	ASSERT(p[0] = 7);
	ASSERT(p[9] = 7);
	s := SumOpen(p^);
	ASSERT(s = 70)
END TestPtrArrayToOpenParam;

(* Pointer-to-array deref in string comparison (no .$ in strcmp) *)

PROCEDURE TestPtrStringCompare;
VAR
	p1, p2: CharBufPtr;
BEGIN
	NEW(p1);
	NEW(p2);
	p1[0] := "H"; p1[1] := "e"; p1[2] := "l"; p1[3] := "l"; p1[4] := "o"; p1[5] := 0X;
	p2[0] := "H"; p2[1] := "e"; p2[2] := "l"; p2[3] := "l"; p2[4] := "o"; p2[5] := 0X;
	ASSERT(p1^ = p2^);
	ASSERT(~(p1^ # p2^));
	p2[0] := "W"; p2[1] := "o"; p2[2] := "r"; p2[3] := "l"; p2[4] := "d"; p2[5] := 0X;
	ASSERT(p1^ # p2^);
	ASSERT(p1^ < p2^);
	ASSERT(p2^ > p1^)
END TestPtrStringCompare;

(* Pointer-to-array deref in string assignment and COPY *)

PROCEDURE TestPtrStringAssign;
VAR
	p: CharBufPtr;
BEGIN
	NEW(p);
	p^ := "Oberon";
	ASSERT(p[0] = "O");
	ASSERT(p[1] = "b");
	ASSERT(p[5] = "n");
	ASSERT(p[6] = 0X)
END TestPtrStringAssign;

PROCEDURE TestPtrStringCopy;
VAR
	p1, p2: CharBufPtr;
	src: ARRAY 32 OF CHAR;
BEGIN
	NEW(p1);
	NEW(p2);
	src := "CopyTest";
	COPY(src, p1^);
	ASSERT(p1[0] = "C");
	ASSERT(p1[4] = "T");
	COPY(p1^, p2^);
	ASSERT(p2[0] = "C");
	ASSERT(p2[4] = "T");
	ASSERT(p1^ = p2^)
END TestPtrStringCopy;

(* Single-char string literal in strcmp *)

PROCEDURE TestSingleCharStrCompare;
VAR
	name: ARRAY 32 OF CHAR;
BEGIN
	name := "C";
	ASSERT(name = "C");
	ASSERT(name < "D");
	ASSERT(name > "B");
	ASSERT(name # "X");
	name := "Z";
	ASSERT(name = "Z");
	ASSERT(name > "A");
	ASSERT(name > "Y")
END TestSingleCharStrCompare;

(* Single-char string in array assignment *)

PROCEDURE TestSingleCharAssign;
VAR s: ARRAY 32 OF CHAR;
BEGIN
	s := "#";
	ASSERT(s[0] = "#");
	s := "X";
	ASSERT(s[0] = "X");
	s := " ";
	ASSERT(s[0] = " ")
END TestSingleCharAssign;

(* Single-char string in COPY builtin *)

PROCEDURE TestSingleCharCopy;
VAR src, dst: ARRAY 32 OF CHAR;
BEGIN
	src := "A";
	COPY(src, dst);
	ASSERT(dst[0] = "A");
	src := "!";
	COPY(src, dst);
	ASSERT(dst[0] = "!")
END TestSingleCharCopy;

(* Single-char string literal passed to open array param *)

PROCEDURE CheckFirstChar(s: ARRAY OF CHAR; expected: CHAR): BOOLEAN;
BEGIN
	RETURN s[0] = expected
END CheckFirstChar;

PROCEDURE OpenLen(s: ARRAY OF CHAR): LONGINT;
BEGIN
	RETURN LEN(s)
END OpenLen;

PROCEDURE TestSingleCharOpenArray;
BEGIN
	ASSERT(CheckFirstChar("X", "X"));
	ASSERT(CheckFirstChar("A", "A"));
	ASSERT(~CheckFirstChar("B", "A"));
	ASSERT(OpenLen("Z") >= 1)
END TestSingleCharOpenArray;

(* Nested procedure accessing outer VAR param (closure lifting) *)

PROCEDURE OuterProc(VAR x: LONGINT);
	PROCEDURE Inner;
	BEGIN
		x := x + 10
	END Inner;
	PROCEDURE Inner2;
	BEGIN
		x := x * 2
	END Inner2;
BEGIN
	Inner;
	Inner2
END OuterProc;

PROCEDURE TestClosureVarParam;
VAR v: LONGINT;
BEGIN
	v := 5;
	OuterProc(v);
	ASSERT(v = 30);
	v := 0;
	OuterProc(v);
	ASSERT(v = 20)
END TestClosureVarParam;

(* IN operator boolean normalization *)

PROCEDURE TestInOperatorBool;
VAR
	s: SET;
	b: BOOLEAN;
	count: LONGINT;
	i: LONGINT;
BEGIN
	s := {0, 2, 4, 6, 8};
	b := 0 IN s;
	ASSERT(b);
	b := 1 IN s;
	ASSERT(~b);
	b := 2 IN s;
	ASSERT(b);
	count := 0;
	FOR i := 0 TO 31 DO
		IF i IN s THEN
			INC(count)
		END
	END;
	ASSERT(count = 5);
	b := (3 IN s) OR (4 IN s);
	ASSERT(b);
	b := (1 IN s) & (3 IN s);
	ASSERT(~b)
END TestInOperatorBool;

(* Dynamic array indexing *)

PROCEDURE TestDynamicArrayIndex;
VAR
	p: POINTER TO ARRAY OF LONGINT;
	i, sum: LONGINT;
BEGIN
	NEW(p, 20);
	FOR i := 0 TO 19 DO
		p[i] := i * i
	END;
	ASSERT(p[0] = 0);
	ASSERT(p[1] = 1);
	ASSERT(p[4] = 16);
	ASSERT(p[10] = 100);
	ASSERT(p[19] = 361);
	sum := 0;
	FOR i := 0 TO 19 DO
		sum := sum + p[i]
	END;
	ASSERT(sum = 2470)
END TestDynamicArrayIndex;

(* Pointer upcasting in type hierarchy *)

PROCEDURE MakeDerived(id: LONGINT; val: REAL): DerivedPtr;
VAR d: DerivedPtr;
BEGIN
	NEW(d);
	d.id := id;
	d.value := val;
	RETURN d
END MakeDerived;

PROCEDURE MakeDerived2(id: LONGINT; val: REAL; ex: LONGINT): Derived2Ptr;
VAR d: Derived2Ptr;
BEGIN
	NEW(d);
	d.id := id;
	d.value := val;
	d.extra := ex;
	RETURN d
END MakeDerived2;

PROCEDURE GetId(b: BasePtr): LONGINT;
BEGIN
	RETURN b.id
END GetId;

PROCEDURE TestPointerUpcast;
VAR
	b: BasePtr;
	d: DerivedPtr;
	d2: Derived2Ptr;
BEGIN
	d := MakeDerived(10, 3.14);
	b := d;
	ASSERT(b.id = 10);
	ASSERT(GetId(d) = 10);
	d2 := MakeDerived2(20, 2.718, 99);
	b := d2;
	ASSERT(b.id = 20);
	ASSERT(GetId(d2) = 20);
	d := d2;
	ASSERT(d.value > 2.71);
	ASSERT(d.value < 2.72)
END TestPointerUpcast;

(* String literal comparison *)

PROCEDURE TestStringLitCompare;
VAR
	s: ARRAY 64 OF CHAR;
	eq: BOOLEAN;
BEGIN
	eq := "hello" = "hello";
	ASSERT(eq);
	eq := "aaa" < "bbb";
	ASSERT(eq);
	eq := "bbb" < "aaa";
	ASSERT(~eq);
	eq := "abc" = "abd";
	ASSERT(~eq);
	s := "alpha";
	ASSERT(s = "alpha");
	ASSERT(s < "beta");
	ASSERT(s > "aaa");
	ASSERT(s # "Alpha")
END TestStringLitCompare;

(* SYSTEM.VAL type punning *)

PROCEDURE TestSYSTEMVAL;
VAR
	n: LONGINT;
	s: SET;
BEGIN
	n := 5;
	s := SYSTEM.VAL(SET, n);
	ASSERT(0 IN s);
	ASSERT(~(1 IN s));
	ASSERT(2 IN s);
	ASSERT(~(3 IN s));
	n := SYSTEM.VAL(LONGINT, s);
	ASSERT(n = 5);
	s := {};
	n := SYSTEM.VAL(LONGINT, s);
	ASSERT(n = 0);
	s := {0..31};
	n := SYSTEM.VAL(LONGINT, s);
	ASSERT(n = -1)
END TestSYSTEMVAL;

(* SET operations regression *)

PROCEDURE TestSetOps;
VAR s1, s2, s3: SET;
BEGIN
	s1 := {0, 1, 2, 3};
	s2 := {2, 3, 4, 5};
	s3 := s1 + s2;
	ASSERT(0 IN s3);
	ASSERT(5 IN s3);
	s3 := s1 * s2;
	ASSERT(2 IN s3);
	ASSERT(~(0 IN s3));
	s3 := s1 - s2;
	ASSERT(0 IN s3);
	ASSERT(~(2 IN s3));
	s3 := s1 / s2;
	ASSERT(0 IN s3);
	ASSERT(~(2 IN s3));
	ASSERT(4 IN s3)
END TestSetOps;

(* CHAR comparison regression (should use == not strcmp) *)

PROCEDURE TestCharCompare;
VAR
	c: CHAR;
	s: ARRAY 32 OF CHAR;
BEGIN
	c := "A";
	ASSERT(c = "A");
	ASSERT(c # "B");
	ASSERT(c < "B");
	ASSERT(c > "0");
	s := "Hello";
	ASSERT(s[0] = "H");
	ASSERT(s[4] = "o");
	ASSERT(s[0] # "X");
	c := CAP("z");
	ASSERT(c = "Z")
END TestCharCompare;

BEGIN
	TestKeywordFields;
	TestPtrToFixedArray;
	TestPtrArrayToOpenParam;
	TestPtrStringCompare;
	TestPtrStringAssign;
	TestPtrStringCopy;
	TestSingleCharStrCompare;
	TestSingleCharAssign;
	TestSingleCharCopy;
	TestSingleCharOpenArray;
	TestClosureVarParam;
	TestInOperatorBool;
	TestDynamicArrayIndex;
	TestPointerUpcast;
	TestStringLitCompare;
	TestSYSTEMVAL;
	TestSetOps;
	TestCharCompare
END Test6.
