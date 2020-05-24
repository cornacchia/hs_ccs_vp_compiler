# CCS-VP to CCS compiler written in Haskell

The compiler can be used from `ghci` by calling the function `main`: this will prompt for a CCS-VP source file path and it will proceed to compile and print it out on the terminal.

```
> ghci main.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 7] Compiling Parse            ( Parse.hs, interpreted )
[2 of 7] Compiling ParseExpressionVar ( ParseExpressionVar.hs, interpreted )
[3 of 7] Compiling ParseVPCCS       ( ParseVPCCS.hs, interpreted )
[4 of 7] Compiling EvalExpr         ( EvalExpr.hs, interpreted )
[5 of 7] Compiling CompileCCS       ( CompileCCS.hs, interpreted )
[6 of 7] Compiling PrettyPrinter    ( PrettyPrinter.hs, interpreted )
[7 of 7] Compiling Main             ( main.hs, interpreted )
Ok, modules loaded: CompileCCS, EvalExpr, Main, Parse, ParseExpressionVar, ParseVPCCS, PrettyPrinter.
*Main> main
Type the path of the CCS-VP source file to compile and press ENTER:
programs/input1
B = (in1. (BB1)) + (in2. (BB2))
BB1 = ('out2.B)\{out2}
BB2 = ('out3.B)\{out3}
```

A small test suite is provided in the `test` folder.
This suite can also be run from `ghci`:

```
> ghci test/Test.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 7] Compiling Parse            ( Parse.hs, interpreted )
[2 of 7] Compiling ParseExpressionVar ( ParseExpressionVar.hs, interpreted )
[3 of 7] Compiling ParseVPCCS       ( ParseVPCCS.hs, interpreted )
[4 of 7] Compiling EvalExpr         ( EvalExpr.hs, interpreted )
[5 of 7] Compiling CompileCCS       ( CompileCCS.hs, interpreted )
[6 of 7] Compiling PrettyPrinter    ( PrettyPrinter.hs, interpreted )
[7 of 7] Compiling Test             ( test/Test.hs, interpreted )
Ok, modules loaded: CompileCCS, EvalExpr, Parse, ParseExpressionVar, ParseVPCCS, PrettyPrinter, Test.
*Test> runTests
OK: [[ 0 ]] ==> 0
OK: [[ P = 0 ]] ==> P = 0
OK: [[ P(x) = 0 ]] ==> P1 = 0;P2 = 0
OK: [[ P(x,y) = 0 ]] ==> P1,1 = 0;P1,2 = 0;P2,1 = 0;P2,2 = 0
OK: [[ in(x).K ]] ==> (in1. (K)) + (in2. (K))
OK: [[ in(x).K(x) ]] ==> (in1. (K1)) + (in2. (K2))
OK: [[ in(x).in(x).0 ]] ==> (in1. ((in1. (0)) + (in2. (0)))) + (in2. ((in1. (0)) + (in2. (0))))
OK: [[ 'out(1).0 ]] ==> 'out1.0
OK: [[ in(x).'out(x+1).0 ]] ==> (in1. ('out2.0)) + (in2. ('out3.0))
OK: [[ in(x).in(y).'out(x + y).0 ]] ==> (in1. ((in1. ('out2.0)) + (in2. ('out3.0)))) + (in2. ((in1. ('out3.0)) + (in2. ('out4.0))))
OK: [[ tau.0 ]] ==> tau.0
OK: [[ 0 | 0 ]] ==> (0) | (0)
OK: [[ 0 | 0 | 0 ]] ==> (0) | ((0) | (0))
OK: [[ (in(x).0) | ('out(2).0) ]] ==> ((in1. (0)) + (in2. (0))) | ('out2.0)
OK: [[ 0 + 0 ]] ==> (0) + (0)
OK: [[ 0 + 0 + 0 ]] ==> (0) + ((0) + (0))
OK: [[ ('out(1).0) + tau.0 ]] ==> ('out1.0) + (tau.0)
OK: [[ 0\{p} ]] ==> 0
OK: [[ (in(x).0)\{in} ]] ==> ((in1. (0)) + (in2. (0)))\{in1,in2}
OK: [[ ('out(3).0)\{out} ]] ==> ('out3.0)\{out3}
OK: [[ (in(x).'out(x + 1).0)\{out,in} ]] ==> ((in1. ('out2.0)) + (in2. ('out3.0)))\{out2,out3,in1,in2}
OK: [[ 0[newIn/in] ]] ==> 0
OK: [[ (in(x).0)[newIn/in] ]] ==> ((in1. (0)) + (in2. (0))){newIn1/in1, newIn2/in2}
OK: [[ ('out(3).0)[newOut/out] ]] ==> ('out3.0){newOut3/out3}
OK: [[ (in(x).'out(x+1).0)[newIn/in, newOut/out] ]] ==> ((in1. ('out2.0)) + (in2. ('out3.0))){newIn1/in1, newIn2/in2, newOut2/out2, newOut3/out3}
Test passed: 25 out of 25
```