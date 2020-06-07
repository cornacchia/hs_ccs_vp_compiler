# CCS-VP to CCS compiler written in Haskell

The compiler can be used from `ghci` by calling the function `main`: this will prompt for a CCS-VP source file path and it will proceed to compile and print it out on the terminal.

```
> ghci main.hs 
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
[1 of 8] Compiling Parse            ( Parse.hs, interpreted )
[2 of 8] Compiling ParseExpressionVar ( ParseExpressionVar.hs, interpreted )
[3 of 8] Compiling Context          ( Context.hs, interpreted )
[4 of 8] Compiling EvalExpr         ( EvalExpr.hs, interpreted )
[5 of 8] Compiling ParseVPCCS       ( ParseVPCCS.hs, interpreted )
[6 of 8] Compiling Compile          ( Compile.hs, interpreted )
[7 of 8] Compiling PrettyPrinter    ( PrettyPrinter.hs, interpreted )
[8 of 8] Compiling Main             ( main.hs, interpreted )
Ok, 8 modules loaded.
*Main> main
Type the path of the CCS-VP source file to compile and press ENTER:
programs/input1
B = (in_1. (BB1)) + ((in_2. (BB2)) + (in_3. (BB3)))
BB1 = ('out_2.B)\{out_2}
BB2 = ('out_3.B)\{out_3}
```

A small test suite is provided in the `test` folder.
This suite can also be run from `ghci`:

```
> ghci test/Test.hs 
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
[1 of 8] Compiling Parse            ( Parse.hs, interpreted )
[2 of 8] Compiling ParseExpressionVar ( ParseExpressionVar.hs, interpreted )
[3 of 8] Compiling Context          ( Context.hs, interpreted )
[4 of 8] Compiling EvalExpr         ( EvalExpr.hs, interpreted )
[5 of 8] Compiling ParseVPCCS       ( ParseVPCCS.hs, interpreted )
[6 of 8] Compiling Compile          ( Compile.hs, interpreted )
[7 of 8] Compiling PrettyPrinter    ( PrettyPrinter.hs, interpreted )
[8 of 8] Compiling Test             ( test/Test.hs, interpreted )
Ok, 8 modules loaded.
*Test> runTests
OK: [[ 0 ]] ==> 0
OK: [[ P = 0 ]] ==> P = 0
OK: [[ P = P(1) P(x:Int) = 0 ]] ==> P = P1; P1 = 0
OK: [[ P = P(1,2) P(x:Int,y:Int) = 0 ]] ==> P = P12; P12 = 0
OK: [[ in(x : Int).K ]] ==> (in_0. (K)) + ((in_1. (K)) + (in_2. (K)))
OK: [[ in(x : Int).K(x) ]] ==> (in_0. (K0)) + ((in_1. (K1)) + (in_2. (K2)))
OK: [[ in(x : Int).in(x : Int).0 ]] ==> (in_0. ((in_0. (0)) + ((in_1. (0)) + (in_2. (0))))) + ((in_1. ((in_0. (0)) + ((in_1. (0)) + (in_2. (0))))) + (in_2. ((in_0. (0)) + ((in_1. (0)) + (in_2. (0))))))
OK: [[ 'out(1).0 ]] ==> 'out_1.0
OK: [[ in(x : Int).'out(x+1).0 ]] ==> (in_0. ('out_1.0)) + (in_1. ('out_2.0))
OK: [[ in(x : Int).in(y : Int).'out(x + y).0 ]] ==> (in_0. ((in_0. ('out_0.0)) + ((in_1. ('out_1.0)) + (in_2. ('out_2.0))))) + ((in_1. ((in_0. ('out_1.0)) + (in_1. ('out_2.0)))) + (in_2. (in_0. ('out_2.0))))
OK: [[ tau.0 ]] ==> tau.0
OK: [[ 0 | 0 ]] ==> (0) | (0)
OK: [[ 0 | 0 | 0 ]] ==> (0) | ((0) | (0))
OK: [[ (in(x : Int).0) | ('out(2).0) ]] ==> ((in_0. (0)) + ((in_1. (0)) + (in_2. (0)))) | ('out_2.0)
OK: [[ 0 + 0 ]] ==> (0) + (0)
OK: [[ 0 + 0 + 0 ]] ==> (0) + ((0) + (0))
OK: [[ ('out(1).0) + tau.0 ]] ==> ('out_1.0) + (tau.0)
OK: [[ 0\{p} ]] ==> 0
OK: [[ (in(x : Int).0)\{in} ]] ==> ((in_0. (0)) + ((in_1. (0)) + (in_2. (0))))\{in_0,in_1,in_2}
OK: [[ ('out(2).0)\{out} ]] ==> ('out_2.0)\{out_2}
OK: [[ (in(x : Int).'out(x + 1).0)\{out,in} ]] ==> ((in_0. ('out_1.0)) + (in_1. ('out_2.0)))\{out_1,out_2,in_0,in_1}
OK: [[ 0[newIn/in] ]] ==> 0
OK: [[ (in(x : Int).0)[newIn/in] ]] ==> ((in_0. (0)) + ((in_1. (0)) + (in_2. (0)))){newIn_0/in_0, newIn_1/in_1, newIn_2/in_2}
OK: [[ ('out(2).0)[newOut/out] ]] ==> ('out_2.0){newOut_2/out_2}
OK: [[ (in(x : Int).'out(x+1).0)[newIn/in, newOut/out] ]] ==> ((in_0. ('out_1.0)) + (in_1. ('out_2.0))){newIn_0/in_0, newIn_1/in_1, newOut_1/out_1, newOut_2/out_2}
Test passed: 25 out of 25
```