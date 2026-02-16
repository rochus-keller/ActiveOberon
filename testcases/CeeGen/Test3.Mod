(* 2026 Rochus Keller <mailto:me@rochus-keller.ch>
   Available for GNU General Public License Usage *)

MODULE Test3;
(* Test3: Procedures, VAR params, open array params, strings, recursion *)
(* No dependencies on other modules *)

VAR
	result: LONGINT;

PROCEDURE Add(a, b: LONGINT): LONGINT;
BEGIN
	RETURN a + b
END Add;

PROCEDURE Swap(VAR a, b: LONGINT);
VAR tmp: LONGINT;
BEGIN
	tmp := a;
	a := b;
	b := tmp
END Swap;

PROCEDURE Inc3(VAR x: LONGINT; delta: LONGINT);
BEGIN
	x := x + delta
END Inc3;

PROCEDURE Factorial(n: LONGINT): LONGINT;
BEGIN
	IF n <= 1 THEN
		RETURN 1
	ELSE
		RETURN n * Factorial(n - 1)
	END
END Factorial;

PROCEDURE Fibonacci(n: LONGINT): LONGINT;
VAR a, b, i, tmp: LONGINT;
BEGIN
	IF n <= 1 THEN RETURN n END;
	a := 0;
	b := 1;
	FOR i := 2 TO n DO
		tmp := b;
		b := a + b;
		a := tmp
	END;
	RETURN b
END Fibonacci;

PROCEDURE SumArray(VAR a: ARRAY OF LONGINT): LONGINT;
VAR i, s: LONGINT;
BEGIN
	s := 0;
	FOR i := 0 TO LEN(a) - 1 DO
		s := s + a[i]
	END;
	RETURN s
END SumArray;

PROCEDURE FillArray(VAR a: ARRAY OF LONGINT; val: LONGINT);
VAR i: LONGINT;
BEGIN
	FOR i := 0 TO LEN(a) - 1 DO
		a[i] := val
	END
END FillArray;

PROCEDURE MaxInArray(VAR a: ARRAY OF LONGINT): LONGINT;
VAR i, m: LONGINT;
BEGIN
	m := a[0];
	FOR i := 1 TO LEN(a) - 1 DO
		IF a[i] > m THEN
			m := a[i]
		END
	END;
	RETURN m
END MaxInArray;

PROCEDURE StrLen(VAR s: ARRAY OF CHAR): LONGINT;
VAR i: LONGINT;
BEGIN
	i := 0;
	WHILE (i < LEN(s)) & (s[i] # 0X) DO
		INC(i)
	END;
	RETURN i
END StrLen;

PROCEDURE StrEqual(VAR a, b: ARRAY OF CHAR): BOOLEAN;
VAR i: LONGINT;
BEGIN
	i := 0;
	WHILE (i < LEN(a)) & (i < LEN(b)) & (a[i] # 0X) & (a[i] = b[i]) DO
		INC(i)
	END;
	IF (i < LEN(a)) & (i < LEN(b)) THEN
		RETURN a[i] = b[i]
	ELSIF (i >= LEN(a)) & (i >= LEN(b)) THEN
		RETURN TRUE
	ELSE
		RETURN FALSE
	END
END StrEqual;

PROCEDURE GCD(a, b: LONGINT): LONGINT;
BEGIN
	WHILE b # 0 DO
		IF a > b THEN
			a := a - b
		ELSE
			b := b - a
		END
	END;
	RETURN a
END GCD;

PROCEDURE Power(base: REAL; exp: LONGINT): REAL;
VAR r: REAL;
BEGIN
	r := 1.0;
	WHILE exp > 0 DO
		r := r * base;
		DEC(exp)
	END;
	RETURN r
END Power;

PROCEDURE TestBasicProc;
VAR r: LONGINT;
BEGIN
	r := Add(3, 4);
	ASSERT(r = 7);
	r := Add(0, 0);
	ASSERT(r = 0);
	r := Add(-5, 5);
	ASSERT(r = 0)
END TestBasicProc;

PROCEDURE TestSwap;
VAR a, b: LONGINT;
BEGIN
	a := 10;
	b := 20;
	Swap(a, b);
	ASSERT(a = 20);
	ASSERT(b = 10)
END TestSwap;

PROCEDURE TestVarParam;
VAR x: LONGINT;
BEGIN
	x := 100;
	Inc3(x, 5);
	ASSERT(x = 105);
	Inc3(x, -10);
	ASSERT(x = 95)
END TestVarParam;

PROCEDURE TestRecursion;
VAR r: LONGINT;
BEGIN
	ASSERT(Factorial(0) = 1);
	ASSERT(Factorial(1) = 1);
	ASSERT(Factorial(5) = 120);
	ASSERT(Factorial(10) = 3628800);
	ASSERT(Fibonacci(0) = 0);
	ASSERT(Fibonacci(1) = 1);
	ASSERT(Fibonacci(10) = 55);
	ASSERT(Fibonacci(20) = 6765);
	ASSERT(GCD(48, 36) = 12);
	ASSERT(GCD(100, 75) = 25);
	ASSERT(GCD(17, 13) = 1)
END TestRecursion;

PROCEDURE TestOpenArrays;
VAR
	arr: ARRAY 5 OF LONGINT;
	i, s, m: LONGINT;
BEGIN
	FOR i := 0 TO 4 DO
		arr[i] := (i + 1) * 10
	END;
	s := SumArray(arr);
	ASSERT(s = 150);
	m := MaxInArray(arr);
	ASSERT(m = 50);
	FillArray(arr, 99);
	ASSERT(arr[0] = 99);
	ASSERT(arr[4] = 99);
	s := SumArray(arr);
	ASSERT(s = 495)
END TestOpenArrays;

PROCEDURE TestStringProcs;
VAR
	name: ARRAY 32 OF CHAR;
	s1, s2: ARRAY 32 OF CHAR;
	len: LONGINT;
	eq: BOOLEAN;
BEGIN
	name := "Oberon";
	len := StrLen(name);
	ASSERT(len = 6);
	s1 := "Hello";
	s2 := "Hello";
	eq := StrEqual(s1, s2);
	ASSERT(eq);
	s2 := "World";
	eq := StrEqual(s1, s2);
	ASSERT(~eq)
END TestStringProcs;

PROCEDURE TestStringCompare;
VAR
	s1, s2: ARRAY 64 OF CHAR;
	eq: BOOLEAN;
BEGIN
	s1 := "alpha";
	s2 := "alpha";
	eq := s1 = s2;
	ASSERT(eq);
	s2 := "beta";
	eq := s1 = s2;
	ASSERT(~eq);
	eq := s1 < s2;
	ASSERT(eq);
	eq := "hello" = "hello";
	ASSERT(eq);
	eq := "aaa" < "bbb";
	ASSERT(eq);
	eq := "bbb" < "aaa";
	ASSERT(~eq)
END TestStringCompare;

PROCEDURE TestRealProc;
VAR x: REAL;
BEGIN
	x := Power(2.0, 10);
	ASSERT(x = 1024.0);
	x := Power(3.0, 0);
	ASSERT(x = 1.0);
	x := Power(5.0, 3);
	ASSERT(x = 125.0)
END TestRealProc;

BEGIN
	TestBasicProc;
	TestSwap;
	TestVarParam;
	TestRecursion;
	TestOpenArrays;
	TestStringProcs;
	TestStringCompare;
	TestRealProc
END Test3.
