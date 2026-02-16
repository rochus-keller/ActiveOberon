(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)

MODULE Test5;
(* Test5: Sets, standard builtins, SYSTEM builtins *)
(* No dependencies on other modules *)

IMPORT SYSTEM;

VAR
	gi: LONGINT;
	gr: REAL;
	gb: BOOLEAN;
	gc: CHAR;
	gs: SET;

PROCEDURE TestSets;
VAR
	s1, s2, s3: SET;
	b: BOOLEAN;
BEGIN
	s1 := {0, 1, 2, 3};
	s2 := {2, 3, 4, 5};
	s3 := s1 + s2;
	ASSERT(0 IN s3);
	ASSERT(1 IN s3);
	ASSERT(4 IN s3);
	ASSERT(5 IN s3);

	s3 := s1 * s2;
	ASSERT(2 IN s3);
	ASSERT(3 IN s3);
	ASSERT(~(0 IN s3));
	ASSERT(~(4 IN s3));

	s3 := s1 - s2;
	ASSERT(0 IN s3);
	ASSERT(1 IN s3);
	ASSERT(~(2 IN s3));
	ASSERT(~(3 IN s3));

	s3 := s1 / s2;
	ASSERT(0 IN s3);
	ASSERT(1 IN s3);
	ASSERT(~(2 IN s3));
	ASSERT(~(3 IN s3));
	ASSERT(4 IN s3);
	ASSERT(5 IN s3);

	b := 2 IN s1;
	ASSERT(b);
	b := 7 IN s1;
	ASSERT(~b);

	INCL(s1, 10);
	ASSERT(10 IN s1);
	EXCL(s1, 0);
	ASSERT(~(0 IN s1));

	s3 := {1..5};
	ASSERT(1 IN s3);
	ASSERT(3 IN s3);
	ASSERT(5 IN s3);
	ASSERT(~(0 IN s3));
	ASSERT(~(6 IN s3));

	s3 := {0, 3..7, 15};
	ASSERT(0 IN s3);
	ASSERT(3 IN s3);
	ASSERT(5 IN s3);
	ASSERT(7 IN s3);
	ASSERT(15 IN s3);
	ASSERT(~(1 IN s3));
	ASSERT(~(8 IN s3))
END TestSets;

PROCEDURE TestABS;
VAR
	a: LONGINT;
	x: REAL;
BEGIN
	a := ABS(-42);
	ASSERT(a = 42);
	a := ABS(42);
	ASSERT(a = 42);
	a := ABS(0);
	ASSERT(a = 0);
	x := ABS(-3.14);
	ASSERT(x > 3.13);
	ASSERT(x < 3.15)
END TestABS;

PROCEDURE TestODD;
BEGIN
	ASSERT(ODD(3));
	ASSERT(ODD(1));
	ASSERT(ODD(-1));
	ASSERT(~ODD(4));
	ASSERT(~ODD(0));
	ASSERT(~ODD(2))
END TestODD;

PROCEDURE TestCAP;
VAR c: CHAR;
BEGIN
	c := CAP("a");
	ASSERT(c = "A");
	c := CAP("z");
	ASSERT(c = "Z");
	c := CAP("A");
	ASSERT(c = "A");
	c := CAP("m");
	ASSERT(c = "M")
END TestCAP;

PROCEDURE TestLEN;
VAR
	a: ARRAY 10 OF LONGINT;
	s: ARRAY 32 OF CHAR;
	n: LONGINT;
BEGIN
	n := LEN(a);
	ASSERT(n = 10);
	n := LEN(s);
	ASSERT(n = 32)
END TestLEN;

PROCEDURE TestLenOpen(VAR a: ARRAY OF LONGINT);
VAR n: LONGINT;
BEGIN
	n := LEN(a);
	ASSERT(n = 8)
END TestLenOpen;

PROCEDURE TestORDCHR;
VAR
	n: LONGINT;
	c: CHAR;
BEGIN
	n := ORD("A");
	ASSERT(n = 65);
	c := CHR(66);
	ASSERT(c = "B");
	n := ORD(c);
	ASSERT(n = 66);
	c := CHR(48);
	ASSERT(c = "0")
END TestORDCHR;

PROCEDURE TestSHORTLONG;
VAR
	h: HUGEINT;
	l: LONGINT;
	i: INTEGER;
	s: SHORTINT;
	x: REAL;
	d: LONGREAL;
BEGIN
	h := 1000000;
	l := SHORT(h);
	ASSERT(l = 1000000);
	i := SHORT(l);
	s := SHORT(i);
	s := 10;
	i := LONG(s);
	ASSERT(i = 10);
	l := LONG(i);
	ASSERT(l = 10);
	h := LONG(l);
	ASSERT(h = 10);
	x := 3.14;
	d := LONG(x);
	x := SHORT(d);
	ASSERT(x > 3.13);
	ASSERT(x < 3.15)
END TestSHORTLONG;

PROCEDURE TestENTIER;
VAR
	n: LONGINT;
BEGIN
	n := ENTIER(3.7);
	ASSERT(n = 3);
	n := ENTIER(-2.3);
	ASSERT(n = -3);
	n := ENTIER(0.0);
	ASSERT(n = 0);
	n := ENTIER(5.0);
	ASSERT(n = 5)
END TestENTIER;

PROCEDURE TestINCDEC;
VAR n: LONGINT;
BEGIN
	n := 10;
	INC(n);
	ASSERT(n = 11);
	DEC(n);
	ASSERT(n = 10);
	INC(n, 5);
	ASSERT(n = 15);
	DEC(n, 3);
	ASSERT(n = 12);
	INC(n, 0);
	ASSERT(n = 12);
	DEC(n, 12);
	ASSERT(n = 0)
END TestINCDEC;

PROCEDURE TestINCLEXCL;
VAR s: SET;
BEGIN
	s := {};
	INCL(s, 0);
	ASSERT(0 IN s);
	INCL(s, 5);
	ASSERT(5 IN s);
	INCL(s, 31);
	ASSERT(31 IN s);
	EXCL(s, 5);
	ASSERT(~(5 IN s));
	ASSERT(0 IN s);
	ASSERT(31 IN s)
END TestINCLEXCL;

PROCEDURE TestCOPY;
VAR
	src, dst: ARRAY 64 OF CHAR;
BEGIN
	src := "Hello, Oberon!";
	COPY(src, dst);
	ASSERT(dst[0] = "H");
	ASSERT(dst[1] = "e");
	ASSERT(dst[7] = "O")
END TestCOPY;

PROCEDURE TestMAXMIN;
VAR
	n: LONGINT;
BEGIN
	n := MAX(LONGINT);
	ASSERT(n > 0);
	n := MIN(LONGINT);
	ASSERT(n < 0);
	n := MAX(INTEGER);
	ASSERT(n > 0);
	n := MIN(INTEGER);
	ASSERT(n < 0)
END TestMAXMIN;

PROCEDURE TestSIZE;
VAR n: LONGINT;
BEGIN
	n := SIZE(CHAR);
	ASSERT(n = 1);
	n := SIZE(BOOLEAN);
	ASSERT(n = 1);
	n := SIZE(REAL);
	ASSERT(n = 4);
	n := SIZE(LONGREAL);
	ASSERT(n = 8)
END TestSIZE;

PROCEDURE TestASH;
VAR n: LONGINT;
BEGIN
	n := ASH(1, 4);
	ASSERT(n = 16);
	n := ASH(16, -2);
	ASSERT(n = 4);
	n := ASH(1, 0);
	ASSERT(n = 1);
	n := ASH(255, -8);
	ASSERT(n = 0)
END TestASH;

PROCEDURE TestNEW;
VAR
	p: POINTER TO ARRAY OF LONGINT;
BEGIN
	NEW(p, 100);
	ASSERT(p # NIL)
END TestNEW;

PROCEDURE TestASSERT;
BEGIN
	ASSERT(1 = 1);
	ASSERT(TRUE);
	ASSERT(~FALSE);
	ASSERT(2 + 2 = 4)
END TestASSERT;

PROCEDURE TestSYSTEMADR;
VAR
	n: LONGINT;
	adr: SYSTEM.PTR;
BEGIN
	n := 42;
	adr := SYSTEM.ADR(n);
	ASSERT(adr # NIL)
END TestSYSTEMADR;

PROCEDURE TestSYSTEMMOVE;
VAR
	src, dst: ARRAY 16 OF CHAR;
BEGIN
	src := "TestMove";
	SYSTEM.MOVE(SYSTEM.ADR(src), SYSTEM.ADR(dst), 16);
	ASSERT(dst[0] = "T");
	ASSERT(dst[4] = "M")
END TestSYSTEMMOVE;

PROCEDURE TestSYSTEMGETPUT;
VAR
	n: LONGINT;
	m: LONGINT;
	adr: SYSTEM.PTR;
BEGIN
	n := 12345;
	adr := SYSTEM.ADR(n);
	SYSTEM.GET(adr, m);
	ASSERT(m = 12345);
	SYSTEM.PUT(adr, 99999);
	ASSERT(n = 99999)
END TestSYSTEMGETPUT;

PROCEDURE TestSYSTEMBIT;
VAR
	n: LONGINT;
BEGIN
	n := 5;
	ASSERT(SYSTEM.BIT(SYSTEM.ADR(n), 0));
	ASSERT(~SYSTEM.BIT(SYSTEM.ADR(n), 1));
	ASSERT(SYSTEM.BIT(SYSTEM.ADR(n), 2));
	ASSERT(~SYSTEM.BIT(SYSTEM.ADR(n), 3))
END TestSYSTEMBIT;

BEGIN
	TestSets;
	TestABS;
	TestODD;
	TestCAP;
	TestLEN;
	TestORDCHR;
	TestSHORTLONG;
	TestENTIER;
	TestINCDEC;
	TestINCLEXCL;
	TestCOPY;
	TestMAXMIN;
	TestSIZE;
	TestASH;
	TestNEW;
	TestASSERT;
	TestSYSTEMADR;
	TestSYSTEMMOVE;
	TestSYSTEMGETPUT;
	TestSYSTEMBIT
END Test5.
