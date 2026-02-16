(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)
   
MODULE Test1;
(* Test1: Basic types, constants, variables, arithmetic, and control flow *)
(* No dependencies on other modules *)

CONST
	Pi = 3.14159;
	MaxVal = 100;
	MinVal = -50;
	Flag = TRUE;
	Greeting = "Hello";
	HexConst = 0FFH;
	CharConst = 41X;

VAR
	gi: LONGINT;
	gr: REAL;
	gb: BOOLEAN;
	gc: CHAR;

PROCEDURE TestArithmetic;
VAR
	a, b, c: LONGINT;
	x, y: REAL;
	s: SHORTINT;
	h: HUGEINT;
BEGIN
	a := 10;
	b := 3;
	c := a + b;
	ASSERT(c = 13);
	c := a - b;
	ASSERT(c = 7);
	c := a * b;
	ASSERT(c = 30);
	c := a DIV b;
	ASSERT(c = 3);
	c := a MOD b;
	ASSERT(c = 1);
	x := 1.5;
	y := x + 2.5;
	ASSERT(y = 4.0);
	s := SHORT(b);
	ASSERT(s = 3);
	h := LONG(LONG(a));
	ASSERT(h = 10)
END TestArithmetic;

PROCEDURE TestConstants;
BEGIN
	ASSERT(MaxVal = 100);
	ASSERT(MinVal = -50);
	ASSERT(Flag = TRUE);
	ASSERT(HexConst = 255);
	ASSERT(CharConst = 41X)
END TestConstants;

PROCEDURE TestIfElse;
VAR n: LONGINT;
BEGIN
	n := 42;
	IF n > 100 THEN
		gi := 1
	ELSIF n > 50 THEN
		gi := 2
	ELSIF n > 0 THEN
		gi := 3
	ELSE
		gi := 4
	END;
	ASSERT(gi = 3);

	n := 200;
	IF n > 100 THEN
		gi := 1
	ELSIF n > 50 THEN
		gi := 2
	ELSE
		gi := 0
	END;
	ASSERT(gi = 1);

	n := -5;
	IF n > 0 THEN
		gi := 1
	ELSE
		gi := 0
	END;
	ASSERT(gi = 0)
END TestIfElse;

PROCEDURE TestWhile;
VAR i, sum: LONGINT;
BEGIN
	i := 0;
	sum := 0;
	WHILE i < 10 DO
		sum := sum + i;
		INC(i)
	END;
	ASSERT(sum = 45);
	ASSERT(i = 10)
END TestWhile;

PROCEDURE TestRepeat;
VAR i: LONGINT;
BEGIN
	i := 10;
	REPEAT
		DEC(i)
	UNTIL i = 0;
	ASSERT(i = 0)
END TestRepeat;

PROCEDURE TestFor;
VAR i, sum: LONGINT;
BEGIN
	sum := 0;
	FOR i := 1 TO 10 DO
		sum := sum + i
	END;
	ASSERT(sum = 55);

	sum := 0;
	FOR i := 10 TO 1 BY -1 DO
		sum := sum + i
	END;
	ASSERT(sum = 55);

	sum := 0;
	FOR i := 0 TO 20 BY 3 DO
		sum := sum + i
	END;
	ASSERT(sum = 63)
END TestFor;

PROCEDURE TestCase;
VAR n, result: LONGINT;
BEGIN
	n := 3;
	CASE n OF
		1: result := 10
	|	2: result := 20
	|	3: result := 30
	|	4, 5: result := 45
	ELSE
		result := 0
	END;
	ASSERT(result = 30);

	n := 5;
	CASE n OF
		1: result := 10
	|	2: result := 20
	|	3: result := 30
	|	4, 5: result := 45
	ELSE
		result := 0
	END;
	ASSERT(result = 45);

	n := 99;
	CASE n OF
		1: result := 10
	ELSE
		result := -1
	END;
	ASSERT(result = -1)
END TestCase;

PROCEDURE TestCaseRange;
VAR n, result: LONGINT;
BEGIN
	n := 3;
	CASE n OF
		0..5: result := 1
	|	6..10: result := 2
	|	11..20: result := 3
	ELSE
		result := 0
	END;
	ASSERT(result = 1);

	n := 7;
	CASE n OF
		0..5: result := 1
	|	6..10: result := 2
	|	11..20: result := 3
	ELSE
		result := 0
	END;
	ASSERT(result = 2);

	n := 15;
	CASE n OF
		0..5: result := 1
	|	6..10: result := 2
	|	11..20: result := 3
	ELSE
		result := 0
	END;
	ASSERT(result = 3)
END TestCaseRange;

PROCEDURE TestLoop;
VAR i: LONGINT;
BEGIN
	i := 0;
	LOOP
		IF i >= 10 THEN EXIT END;
		INC(i)
	END;
	ASSERT(i = 10)
END TestLoop;

PROCEDURE TestBoolean;
VAR a, b, c: BOOLEAN;
BEGIN
	a := TRUE;
	b := FALSE;
	c := a & b;
	ASSERT(~c);
	c := a OR b;
	ASSERT(c);
	c := ~a;
	ASSERT(~c);
	ASSERT(a & ~b);
	gi := 50;
	gb := (gi > 0) & (gi < 100);
	ASSERT(gb)
END TestBoolean;

PROCEDURE TestRelations;
VAR a, b: LONGINT;
BEGIN
	a := 5;
	b := 10;
	ASSERT(~(a = b));
	ASSERT(a # b);
	ASSERT(a < b);
	ASSERT(a <= b);
	ASSERT(~(a > b));
	ASSERT(~(a >= b));
	ASSERT(a <= a);
	ASSERT(a >= a);
	ASSERT(a = a)
END TestRelations;

PROCEDURE TestNested;
VAR i, j, sum: LONGINT;
BEGIN
	sum := 0;
	FOR i := 0 TO 4 DO
		FOR j := 0 TO 4 DO
			IF (i + j) MOD 2 = 0 THEN
				sum := sum + 1
			END
		END
	END;
	ASSERT(sum = 13)
END TestNested;

BEGIN
	gi := 0;
	gr := 0.0;
	gb := FALSE;
	gc := 0X;
	TestArithmetic;
	TestConstants;
	TestIfElse;
	TestWhile;
	TestRepeat;
	TestFor;
	TestCase;
	TestCaseRange;
	TestLoop;
	TestBoolean;
	TestRelations;
	TestNested
END Test1.
