{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-} -- get warnings when we deviate from best practice

-----------------------------------------------------------------------------
-- |
-- Module      :  TypeCheckerTest
-- Copyright   :  (c) Wieger Wesselink 2016
--
-----------------------------------------------------------------------------

module Main(main) where

import qualified Test.Framework as F
import Test.Framework.Providers.HUnit
import Test.HUnit

import Madl.SourceInformation

import Parser.MadlParser
import Parser.MadlTypeChecker

main :: IO ()
main = F.defaultMain (hUnitTestToTests tests)

type Comment = String
type NetworkSource = String
type ExpectedResult = String

testNetwork :: String -> String
testNetwork text = case madlParser "(source)" text of
    Left err -> "OtherError " ++show err
    Right network -> case typecheckNetwork network of
        Right _ -> "NoError"
        Left err -> (words . show $ removeSourceInfo err) !! 0

makeTest :: Comment -> NetworkSource -> ExpectedResult -> Test
makeTest comment_ source expectedResult = TestCase (assertEqual comment_ expectedResult result) where
    result =  testNetwork source

showSource :: String -> String
showSource text = "--- network ---\n" ++ text ++ "\n---------------"

error_testcases :: [(String, ExpectedResult)]
error_testcases = [
      (ambiguousInputOutputSizeTest, "AmbiguousInputOutputSizeError")
    , (busIndexTest, "BusIndexError")
    , (busInputAlreadyAssignedTest, "BusInputAlreadyAssignedError")
    , (busOutputAlreadyAssignedTest, "BusOutputAlreadyAssignedError")
    , (busInputOutputUsageTest1, "BusInputOutputUsageError")
    , (busInputOutputUsageTest2, "BusInputOutputUsageError")
    , (channelInputAlreadyAssignedTest, "ChannelInputAlreadyAssignedError")
    , (channelOutputAlreadyAssignedTest, "ChannelOutputAlreadyAssignedError")
    , (channelInputOutputUsageTest1, "ChannelInputOutputUsageError")
    , (channelInputOutputUsageTest2, "ChannelInputOutputUsageError")
    , (dataTypeNoIntegerConstantTest, "DataTypeNoIntegerConstantError")
    , (doublyDefinedStateTest, "DoublyDefinedStateError")
    , (duplicateParameterNamesTest1, "DuplicateParameterNamesError")
    , (duplicateParameterNamesTest2, "DuplicateParameterNamesError")
    , (duplicateVariableNamesTest, "DuplicateVariableNamesError")
    , (illegalAttributeTest, "IllegalAttributeError")
    , (illegalForLoopNameTest1, "MultiplyDefinedNameError")
    , (illegalForLoopNameTest2, "MultiplyDefinedNameError")
    , (illegalForLoopNameTest3, "MultiplyDefinedNameError")
    , (illegalParameterNameTest1, "MultiplyDefinedNameError")
    , (illegalParameterNameTest2, "MultiplyDefinedNameError")
    , (illegalParameterNameTest3, "MultiplyDefinedNameError")
    , (illegalParameterNameTest4, "MultiplyDefinedNameError")
    , (illegalProcessChannelTest1, "IllegalProcessChannelError")
    , (illegalProcessChannelTest2, "IllegalProcessChannelError")
    , (illegalTypeSelectionTest1, "IllegalTypeSelectionError")
    , (illegalTypeSelectionTest2, "IllegalTypeSelectionError")
    , (incompleteFunctionAssignmentTest, "IncompleteFunctionAssignmentError")
    , (incorrectNrOfArgumentsTest, "IncorrectNrOfArgumentsError")
    , (incorrectNrOfArgumentsFunctionTest, "IncorrectNrOfArgumentsFunctionError")
    , (incorrectNrOfArgumentsPredicateTest1, "IncorrectNrOfArgumentsPredicateError")
    , (incorrectNrOfArgumentsPredicateTest2, "IncorrectNrOfArgumentsPredicateError")
    , (incorrectNrOfArgumentsPredicateTest3, "IncorrectNrOfArgumentsPredicateError")
    , (incorrectNrOfArgumentsPredicateTest4, "IncorrectNrOfArgumentsPredicateError")
    , (incorrectNrOfArgumentsPredicateTest5, "IncorrectNrOfArgumentsPredicateError")
    , (incorrectReturnTypeTest1, "IncorrectReturnTypeError")
    , (incorrectReturnTypeTest2, "IncorrectReturnTypeError")
    , (incorrectReturnTypeTest3, "IncorrectReturnTypeError")
    , (incorrectSwitchCasesTest1, "IncorrectSwitchCasesError")
    , (incorrectSwitchCasesTest2, "IncorrectSwitchCasesError")
    , (inputOutputSizeTest, "InputOutputSizeError")
    , (integerNotPositiveTest1, "IntegerNotPositiveError")
    , (integerNotPositiveTest2, "IntegerNotPositiveError")
    , (invalidArgumentTest1, "InvalidArgumentError")
    , (invalidArgumentTest2, "InvalidArgumentError")
    , (invalidArgumentTest3, "InvalidArgumentError")
    , (invalidArgumentTest4, "InvalidArgumentError")
    , (invalidEnumerationTest, "InvalidEnumerationError")
    , (invalidStructTest, "InvalidStructError")
    , (invalidUnionTest, "InvalidUnionError")
    , (multipleJoitchOtherwiseTest, "MultipleJoitchOtherwiseError")
    , (multipleNextActionsTest, "MultipleNextActionsError")
    , (multipleReadActionsTest, "MultipleReadActionsError")
    , (multipleSwitchOtherwiseTest, "MultipleSwitchOtherwiseError")
    , (multipleWriteActionsTest, "MultipleWriteActionsError")
    , (multiplyDefinedNameTest, "MultiplyDefinedNameError")
    , (noNextActionTest, "NoNextActionError")
    , (noParameterPresentTest1, "NoParameterPresentError")
    , (noParameterPresentTest2, "NoParameterPresentError")
    , (parameterMismatchTest1, "ParameterMismatchError")
    , (parameterMismatchTest2, "ParameterMismatchError")
    , (parameterMismatchTest3, "ParameterMismatchError")
    , (parameterMismatchTest4, "ParameterMismatchError")
    , (parameterMismatchTest5, "ParameterMismatchError")
    , (parameterMismatchTest6, "ParameterMismatchError")
    , (parameterMismatchTest7, "ParameterMismatchError")
    , (processInitialStateParamsTest, "ProcessInitialStateParamsError")
    , (typeNotEmptyTest, "TypeNotEmptyError")
    , (undeclaredBusNameTest, "UndeclaredBusNameError")
    , (undeclaredChannelNameTest, "UndeclaredChannelNameError")
    , (undeclaredFunctionTest, "UndeclaredFunctionError")
    , (undeclaredIntegerParameterTest, "UndeclaredIntegerParameterError")
    , (undeclaredParameterTest, "UndeclaredParameterError")
    , (undeclaredPredicateTest, "UndeclaredPredicateError")
    , (undeclaredPrimitiveTest, "UndeclaredPrimitiveError")
    , (undeclaredTypeVariableTest, "UndeclaredTypeVariableError")
    , (undefinedDataUnknownTest, "UndefinedDataUnknownError")
    , (undefinedDataIntegerUnknownTest, "UndefinedDataIntegerUnknownError")
    , (undefinedPrimitiveUnknownTest, "UndefinedPrimitiveUnknownError")
    , (undefinedSwitchUnknownTest, "UndefinedSwitchUnknownError")
    , (unequalBusSizeTest1, "UnequalBusSizeError")
    , (unequalBusSizeTest2, "UnequalBusSizeError")
    , (unequalBusSizeTest3, "UnequalBusSizeError")
    , (unequalBusSizeTest4, "UnequalBusSizeError")
    , (unequalBusSizeTest5, "UnequalBusSizeError")
    , (unequalBusSizeTest6, "UnequalBusSizeError")
    , (unequalBusSizeTest7, "UnequalBusSizeError")
    , (unequalBusSizeTest8, "UnequalBusSizeError")
    , (unknownStateTest, "UnknownStateError")
    , (uninstantiatedParametersTest1, "UninstantiatedParametersError")
    , (uninstantiatedParametersTest2, "UninstantiatedParametersError")
    , (unsuitableDataIntegerAttributeTest, "UnsuitableIntegerAttributeError")
    , (unsuitableSwitchExpressionTest1, "UnsuitableSwitchExpressionError")
    , (unsuitableSwitchExpressionTest2, "UnsuitableSwitchExpressionError")
    ]

error_tests :: [Test]
error_tests = map (\(source, expectedResult) -> makeTest (showSource source) source expectedResult) error_testcases

noerror_testcases :: [String]
noerror_testcases = [
    "chan q := Vars(q);",

    "const type; Source(type);",

    "const type;                                \n\
    \chan q0, q1 := Fork(Source(type));         \n\
    \chan b1, r1 := Switch(Queue(2, q0), type, otherwise); \n\
    \chan r2, b2 := Switch(Queue(2, q1), type, otherwise); \n\
    \Sink(CtrlJoin(r1, r2));                    \n\
    \Sink(CtrlJoin(b1, b2));                    \n",

    "const val;                                 \n\
    \macro simpleMacro (chan in) => chan o {    \n\
    \ let o:= Queue(2,in);                      \n\
    \};                                         \n\
    \Sink(simpleMacro(Source(val)));            \n",

    "macro A(bus<2> i) => bus<2> o \n\
    \{                             \n\
    \  let o[0] := Queue(1, i[0]); \n\
    \  let o[1] := Queue(1, i[1]); \n\
    \};                            \n\
    \macro B(bus<2> i) => bus<2> o \n\
    \{                             \n\
    \  let o[0] := Queue(1, i[0]); \n\
    \  let o[1] := Queue(1, i[1]); \n\
    \};                            \n\
    \bus<2> b;                     \n\
    \bus<2> c := A(b);             \n\
    \bus<2> d := A(B(c));          \n\
    \let b := B(d);                \n",

    "const e;                                               \n\
    \macro A(bus<3> in) => bus<4> out                       \n\
    \{                                                      \n\
    \  for (int i = 0; i < 3; i++)                          \n\
    \  {                                                    \n\
    \    let out[i] := Queue(1, in[i]);                     \n\
    \  };                                                   \n\
    \  let out[3] := Source(e);                             \n\
    \};                                                     \n\
    \bus<4> b;                                              \n\
    \let b[0],  b[1],  b[2],  b[3]  := A(b[1], b[2], b[3]); \n\
    \Sink(b[0]);                                            \n"

    , parameterMatchTest1
    , parameterMatchTest2
    , parameterMatchTest3
    , parameterMatchTest4
    , parameterMatchTest5
    , parameterMatchTest6
    , parameterMatchTest7
    , parameterMatchTest8
    ]

noerror_tests :: [Test]
noerror_tests = map (\source -> makeTest source source "NoError") noerror_testcases

tests :: Test
tests = TestList (error_tests ++ noerror_tests)

-- Error Tests
ambiguousInputOutputSizeTest :: String
ambiguousInputOutputSizeTest =
    "const t; \n\
    \pred p (a:t, b:t) {true}; \n\
    \Sink(MultiMatch(p, LoadBalancer(Source(t))));\n"

busIndexTest :: String
busIndexTest =
    "const t; \n\
    \bus<2> b; \n\
    \let b[2] := Source(t);\n"

busInputAlreadyAssignedTest :: String
busInputAlreadyAssignedTest =
    "const t; \n\
    \bus<1> b := Source(t);\n\
    \let b[0] := Source(t);"

busOutputAlreadyAssignedTest :: String
busOutputAlreadyAssignedTest =
    "bus<1> b := Queue(1, b[0]);\n\
    \Sink(b[0]);"

busInputOutputUsageTest1 :: String
busInputOutputUsageTest1 =
    "const t;\n\
    \bus<2> b;\n\
    \Sink(b[0]);\n\
    \Sink(b[1]);\n\
    \let b[0] := Source(t);"

busInputOutputUsageTest2 :: String
busInputOutputUsageTest2 =
    "const t;\n\
    \bus<2> b;\n\
    \Sink(b[0]);\n\
    \let b[0] := Source(t);\n\
    \let b[1] := Source(t);"

channelInputAlreadyAssignedTest :: String
channelInputAlreadyAssignedTest =
    "const t; \n\
    \chan c := Source(t);\n\
    \let c := Source(t);"

channelOutputAlreadyAssignedTest :: String
channelOutputAlreadyAssignedTest =
    "chan c := Queue(1, c);\n\
    \Sink(c);"

channelInputOutputUsageTest1 :: String
channelInputOutputUsageTest1 =
    "chan c;\n\
    \Sink(c);"

channelInputOutputUsageTest2 :: String
channelInputOutputUsageTest2 =
    "const t;\n\
    \chan c := Source(t);"

dataTypeNoIntegerConstantTest :: String
dataTypeNoIntegerConstantTest =
    "const t;\n\
    \chan c := Source(t);\n\
    \if (t == 0) {Sink(c);} else {Sink(c);};"

doublyDefinedStateTest :: String
doublyDefinedStateTest =
    "process p (chan i) => chan o {\n\
    \  state s() {}; state s() {};\n\
    \};"

duplicateParameterNamesTest1 :: String
duplicateParameterNamesTest1 =
    "macro m (chan a) => chan a {};"

duplicateParameterNamesTest2 :: String
duplicateParameterNamesTest2 =
    "process p (chan a, int a) {};"

duplicateVariableNamesTest :: String
duplicateVariableNamesTest =
    "const t;\n\
    \function f (a:t, a:t) : t { a };"

illegalAttributeTest :: String
illegalAttributeTest =
    "struct t {a:[0:0]; b:[0:0];};\n\
    \function f() : t { a = 0; b = 0; c = 0;};"

illegalForLoopNameTest1 :: String
illegalForLoopNameTest1 =
    "const t;\n\
    \function i (p:t) : t {p};\n\
    \for (int i = 0; i < 5; i++) {};"

illegalForLoopNameTest2 :: String
illegalForLoopNameTest2 =
    "pred i () {true};\n\
    \for (int i = 0; i < 5; i++) {};"

illegalForLoopNameTest3 :: String
illegalForLoopNameTest3 =
    "const i;\n\
    \for (int i = 0; i < 5; i++) {};"

illegalParameterNameTest1 :: String
illegalParameterNameTest1 =
    "const t;\n\
    \for (int i = 0; i < 5; i++) {\n\
    \  function f (i:t) : t {i};\n\
    \};"

illegalParameterNameTest2 :: String
illegalParameterNameTest2 =
    "const t;\n\
    \param int i = 1;\n\
    \function f (i:t) : t {i};"

illegalParameterNameTest3 :: String
illegalParameterNameTest3 =
    "const t;\n\
    \macro m (int i, chan a) => chan b {\n\
    \  function f (i:t) : t {i};\n\
    \  let b := Function(f, a);\n\
    \};"

illegalParameterNameTest4 :: String
illegalParameterNameTest4 =
    "const a; const b;\n\
    \function f (a:b) : b {a};"

illegalProcessChannelTest1 :: String
illegalProcessChannelTest1 =
    "const t;\n\
    \process p (bus<2> i) => chan o {\n\
    \  state s() {\n\
    \    trans {t a <- i; next s();};\n\
    \  };\n\
    \};"

illegalProcessChannelTest2 :: String
illegalProcessChannelTest2 =
    "const t;\n\
    \process p (chan i) => bus <2> o {\n\
    \  state s() {\n\
    \    trans {t a <- i; a -> o; next s();};\n\
    \  };\n\
    \};"

illegalTypeSelectionTest1 :: String
illegalTypeSelectionTest1 =
    "const a;\n\
    \function f() : a {b{}};"

illegalTypeSelectionTest2 :: String
illegalTypeSelectionTest2 =
    "struct t {fld:[0:0]};\n\
    \function f() : t {t{}};"

incompleteFunctionAssignmentTest :: String
incompleteFunctionAssignmentTest =
    "struct t {a:[0:0]; b:[0:0];};\n\
    \function f() : t { a = 0;};"

incorrectNrOfArgumentsTest :: String
incorrectNrOfArgumentsTest =
    "const a;\n\
    \function f (p:a) : a {p};\n\
    \function g () : a {f()};"

incorrectNrOfArgumentsFunctionTest :: String
incorrectNrOfArgumentsFunctionTest =
    "const a;\n\
    \function f (p:a, q:a) : a {p};\n\
    \chan c1 := Source(a);\n\
    \Sink(Function(f, c1));"

incorrectNrOfArgumentsPredicateTest1 :: String
incorrectNrOfArgumentsPredicateTest1 =
    "const a;\n\
    \pred p (p1:a) {true};\n\
    \chan c1, c2 := Fork(Source(a));\n\
    \Sink(FCtrlJoin(p, c1, c2));"

incorrectNrOfArgumentsPredicateTest2 :: String
incorrectNrOfArgumentsPredicateTest2 =
    "const a;\n\
    \pred p (p1:a) {true};\n\
    \chan c1, c2;\n\
    \chan c3, c4, c5, c6 := Joitch(c1, c2, p, otherwise);"

incorrectNrOfArgumentsPredicateTest3 :: String
incorrectNrOfArgumentsPredicateTest3 =
    "const a;\n\
    \pred p (p1:a) {true};\n\
    \chan c1, c2 := Fork(Source(a));\n\
    \chan c3, c4 := Match(p, c1, c2);\n\
    \Sink(Merge(c3, c4));"

incorrectNrOfArgumentsPredicateTest4 :: String
incorrectNrOfArgumentsPredicateTest4 =
    "const a;\n\
    \pred p (p1:a) {true};\n\
    \chan c1, c2 := Fork(Source(a));\n\
    \Sink(MultiMatch(p, c1, c2));"

incorrectNrOfArgumentsPredicateTest5 :: String
incorrectNrOfArgumentsPredicateTest5 =
    "const a;\n\
    \pred p (p1:a, p2:a) {true};\n\
    \chan c1, c2 := Switch(Source(a), p, otherwise);\n\
    \Sink(Merge(c1, c2));"

incorrectReturnTypeTest1 :: String
incorrectReturnTypeTest1 =
    "const a; const b;\n\
    \function f (p:a) : b {p};"

incorrectReturnTypeTest2 :: String
incorrectReturnTypeTest2 =
    "const a;\n\
    \function f () : a {0};"

incorrectReturnTypeTest3 :: String
incorrectReturnTypeTest3 =
    "const a; const b;\n\
    \struct t {fld:a;};\n\
    \function f (p:t) : b {p.fld;};"

incorrectSwitchCasesTest1 :: String
incorrectSwitchCasesTest1 =
    "enum t {a;b};\n\
    \function f (p:t) : t { switch p {case a:p;}};"

incorrectSwitchCasesTest2 :: String
incorrectSwitchCasesTest2 =
    "enum t {a;b};\n\
    \function f (p:t) : t { switch p {case a:p; case b:p; case c:p;}};"

inputOutputSizeTest :: String
inputOutputSizeTest =
    "const t;\n\
    \pred p (p:t) {true};\n\
    \Sink(Switch(Source(t), p, otherwise));"

integerNotPositiveTest1 :: String
integerNotPositiveTest1 = "bus<0> b;"

integerNotPositiveTest2 :: String
integerNotPositiveTest2 =
    "const t; \n\
    \chan a := Queue(0, Source(t));\n"

invalidArgumentTest1 :: String
invalidArgumentTest1 =
    "const a; const b;\n\
    \function f (p:a) : a {p};\n\
    \function g (p:b) : a {f(p)};"

invalidArgumentTest2 :: String
invalidArgumentTest2 =
    "const a;\n\
    \function f (p:a) : a {p};\n\
    \function g () : a {f(0)};"

invalidArgumentTest3 :: String
invalidArgumentTest3 =
    "const a; const b;\n\
    \struct c {fld:b};\n\
    \function f (p:a) : a {p};\n\
    \function g (p:c) : a {f(p.fld)};"

invalidArgumentTest4 :: String
invalidArgumentTest4 =
    "const a; const b;\n\
    \function f (p:a) : a {p};\n\
    \function h () : b {b};\n\
    \function g () : a {f(h())};"

invalidEnumerationTest :: String
invalidEnumerationTest = "enum t {a;a};"

invalidStructTest :: String
invalidStructTest = "struct t {a:[0:0]; a:[0:0]};"

invalidUnionTest :: String
invalidUnionTest = "union t {a:[0:0]; a:[0:0]};"

multipleJoitchOtherwiseTest :: String
multipleJoitchOtherwiseTest = "bus<6> b; let b[2], b[3], b[4], b[5] := Joitch(b[0], b[1], otherwise, otherwise);"

multipleNextActionsTest :: String
multipleNextActionsTest =
    "process p (chan i) => chan o {\n\
    \  state s() {\n\
    \    trans {next s(); next s();};\n\
    \  };\n\
    \};"

multipleReadActionsTest :: String
multipleReadActionsTest =
    "const t;\n\
    \process p (chan i) => chan o {\n\
    \  state s() {\n\
    \    trans {t a <- i; t b <- i; next s();};\n\
    \  };\n\
    \};"

multipleSwitchOtherwiseTest :: String
multipleSwitchOtherwiseTest = "chan a; chan b := Switch(a, otherwise, otherwise);"

multipleWriteActionsTest :: String
multipleWriteActionsTest =
    "const t;\n\
    \process p (chan i) => chan o {\n\
    \  state s() {\n\
    \    trans {t a <- i; a -> o; a -> o; next s();};\n\
    \  };\n\
    \};"

multiplyDefinedNameTest :: String
multiplyDefinedNameTest = "param int a = 0; chan a;"

noNextActionTest :: String
noNextActionTest =
    "process p (chan i) => chan o {\n\
    \  state s() {\n\
    \    trans {};\n\
    \  };\n\
    \};"

noParameterPresentTest1 :: String
noParameterPresentTest1 =
    "macro m (chan i1, chan i2) => chan o {\n\
    \  let o := Merge(i1, i2);\n\
    \};\n\
    \bus<3> b;\n\
    \chan a := m(b);"

noParameterPresentTest2 :: String
noParameterPresentTest2 =
    "macro m (bus<2> i) => chan o {\n\
    \  let o := Merge(i[0], i[1]);\n\
    \};\n\
    \chan c1, c2, c3;\n\
    \chan a := m(c1, c2, c3);"

parameterMismatchTest1 :: String
parameterMismatchTest1 =
    "macro m (chan i1, int x, chan i2) => chan o {\n\
    \  let o := Merge(i1, i2);\n\
    \};\n\
    \bus<2> b;\n\
    \chan a := m(b);"

parameterMismatchTest2 :: String
parameterMismatchTest2 =
    "macro m (chan i1, int x, chan i2) => chan o {\n\
    \  let o := Merge(i1, i2);\n\
    \};\n\
    \param int SIZE = 0;\n\
    \bus<2> b;\n\
    \chan a := m(b, SIZE);"

parameterMismatchTest3 :: String
parameterMismatchTest3 =
    "macro m (chan i1, int x, chan i2) => chan o {\n\
    \  let o := Merge(i1, i2);\n\
    \};\n\
    \param int SIZE = 0;\n\
    \bus<2> b;\n\
    \chan a := m(SIZE, b);"

parameterMismatchTest4 :: String
parameterMismatchTest4 =
    "const a;\n\
    \macro m (chan i1, a f (a, a), chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \function f (p:a) : a {p};\n\
    \bus<2> b;\n\
    \m(f, b);"

parameterMismatchTest5 :: String
parameterMismatchTest5 =
    "const a;\n\
    \macro m (chan i1, a f (a, a), chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \function f (p:a) : a {p};\n\
    \bus<2> b;\n\
    \m(b, f);"

parameterMismatchTest6 :: String
parameterMismatchTest6 =
    "const a;\n\
    \macro m (chan i1, a f (a, a), chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \function f (p:a) : a {p};\n\
    \bus<2> b;\n\
    \Sink(m(b[0], f, b[1]));"

parameterMismatchTest7 :: String
parameterMismatchTest7 =
    "const a;\n\
    \macro m (int i, a f (a, a)) {\n\
    \  Sink(Source(a));\n\
    \};\n\
    \function f (p:a, q:a) : a {p};\n\
    \param int SIZE = 0;\n\
    \Sink(m(f, SIZE));"

parameterMatchTest1 :: String
parameterMatchTest1 =
    "macro m (chan i1, chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \const a;\n\
    \bus<2> b := Fork(Source(a));\n\
    \m(b);"

parameterMatchTest2 :: String
parameterMatchTest2 =
    "macro m (bus<2> i) {\n\
    \  Sink(Merge(i[0], i[1]));\n\
    \};\n\
    \const a;\n\
    \chan c1, c2 := Fork(Source(a));\n\
    \m(c1, c2);"

parameterMatchTest3 :: String
parameterMatchTest3 =
    "macro m (bus<2> i, chan c) {\n\
    \  Sink(Merge(Merge(i[0], i[1]), c));\n\
    \};\n\
    \const a;\n\
    \bus<2> b := Fork(Source(a));\n\
    \chan c := Source(a);\n\
    \m(b, c);"

parameterMatchTest4 :: String
parameterMatchTest4 =
    "macro m (bus<2> i, chan c) {\n\
    \  Sink(Merge(Merge(i[0], i[1]), c));\n\
    \};\n\
    \const a;\n\
    \bus<2> b := Fork(Source(a));\n\
    \chan c := Source(a);\n\
    \m(c, b);"

parameterMatchTest5 :: String
parameterMatchTest5 =
    "macro m (bus<2> i) {\n\
    \  Sink(Merge(i[0], i[1]));\n\
    \};\n\
    \const a;\n\
    \chan c1, c2 := Fork(Source(a));\n\
    \m(c1, c2);"

parameterMatchTest6 :: String
parameterMatchTest6 =
    "macro m (chan i1, int x, chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \const a;\n\
    \param int SIZE = 0;\n\
    \bus<2> b := Fork(Source(a));\n\
    \m(b[0], SIZE, b[1]);"

parameterMatchTest7 :: String
parameterMatchTest7 =
    "const a;\n\
    \macro m (chan i1, a f (a), chan i2) {\n\
    \  Sink(Merge(i1, i2));\n\
    \};\n\
    \function f (p:a) : a {p};\n\
    \bus<2> b := Fork(Source(a));\n\
    \m(b[0], f, b[1]);"

parameterMatchTest8 :: String
parameterMatchTest8 =
    "const a;\n\
    \macro m (int i, a f (a)) {\n\
    \  Sink(Source(a));\n\
    \};\n\
    \function f (p:a) : a {p};\n\
    \param int SIZE = 0;\n\
    \m(SIZE, f);"

processInitialStateParamsTest :: String
processInitialStateParamsTest =
    "const t;\n\
    \process p (chan i) => chan o {\n\
    \  state s0 (t x) {};\n\
    \};"

typeNotEmptyTest :: String
typeNotEmptyTest =
    "union t {a:[0:0]};\n\
    \function f () : t { a{} };"

undeclaredBusNameTest :: String
undeclaredBusNameTest = "Sink(b[0]);"

undeclaredChannelNameTest :: String
undeclaredChannelNameTest = "Sink(q);"

undeclaredFunctionTest :: String
undeclaredFunctionTest = "chan a; chan b := Function(f, a);"

undeclaredIntegerParameterTest :: String
undeclaredIntegerParameterTest = "chan a; chan b := Queue(k, a);"

undeclaredParameterTest :: String
undeclaredParameterTest = "const t; function f () : t { a.b };"

undeclaredPredicateTest :: String
undeclaredPredicateTest = "chan a, b; chan c, d := Match(p, a, b);"

undeclaredPrimitiveTest :: String
undeclaredPrimitiveTest = "chan a; primitive(a);"

undeclaredTypeVariableTest :: String
undeclaredTypeVariableTest = "chan a := Source(t);"

undefinedDataUnknownTest :: String
undefinedDataUnknownTest = "const t; function f () : t {a};"

undefinedDataIntegerUnknownTest :: String
undefinedDataIntegerUnknownTest = "struct t {fld:[0:0]}; function f () : t { fld = a + 1};"

undefinedPrimitiveUnknownTest :: String
undefinedPrimitiveUnknownTest =
    "macro Reg (chan i) => chan o { let o := Queue(1, i);};\n\
    \chan a;\n\
    \let a := Reg(b);"

undefinedSwitchUnknownTest :: String
undefinedSwitchUnknownTest = "chan a; chan b := Switch(a, p, otherwise);"

unequalBusSizeTest1 :: String
unequalBusSizeTest1 =
    "const t;\n\
    \bus<2> b := Source(t);"

unequalBusSizeTest2 :: String
unequalBusSizeTest2 =
    "const t;\n\
    \bus<1> b := Switch(Source(t), t, otherwise);"

unequalBusSizeTest3 :: String
unequalBusSizeTest3 =
    "const t;\n\
    \chan a, b := Source(t);"

unequalBusSizeTest4 :: String
unequalBusSizeTest4 =
    "const t;\n\
    \chan a := Switch(Source(t), t, otherwise);"

unequalBusSizeTest5 :: String
unequalBusSizeTest5 =
    "const t;\n\
    \bus<2> b;\n\
    \let b := Source(t);\n\
    \Sink(b[0]); Sink(b[1]);"

unequalBusSizeTest6 :: String
unequalBusSizeTest6 =
    "const t;\n\
    \bus<1> b;\n\
    \let b := Switch(Source(t), t, otherwise);\n\
    \Sink(b);"

unequalBusSizeTest7 :: String
unequalBusSizeTest7 =
    "const t;\n\
    \chan a, b;\n\
    \let a, b := Source(t);\n\
    \Sink(a); Sink(b);"

unequalBusSizeTest8 :: String
unequalBusSizeTest8 =
    "const t;\n\
    \chan a;\n\
    \let a := Switch(Source(t), t, otherwise);\n\
    \Sink(a);"

unknownStateTest :: String
unknownStateTest =
    "process p (chan i) => chan o {\n\
    \  state s() {trans{next t();};};\n\
    \};"

uninstantiatedParametersTest1 :: String
uninstantiatedParametersTest1 =
    "macro m (chan i1, chan i2) => chan o {\n\
    \  let o := Merge(i1, i2);\n\
    \};\n\
    \bus<1> b;\n\
    \chan a := m(b);"

uninstantiatedParametersTest2 :: String
uninstantiatedParametersTest2 =
    "macro m (bus<2> i) => chan o {\n\
    \  let o := Merge(i[0], i[1]);\n\
    \};\n\
    \chan c1;\n\
    \chan a := m(c1);"

unsuitableDataIntegerAttributeTest :: String
unsuitableDataIntegerAttributeTest =
    "struct t {fld1:[0:0]; fld2:enum{a;b};};\n\
    \function f (p:t) :t {fld1 = 1 + p.fld2; fld2 = a{};};"

unsuitableSwitchExpressionTest1 :: String
unsuitableSwitchExpressionTest1 =
    "struct t {fld1:[0:0]; fld2:enum{a;b};};\n\
    \function f (p:t) :t {switch p {}};"

unsuitableSwitchExpressionTest2 :: String
unsuitableSwitchExpressionTest2 =
    "struct t {fld1:[0:0]; fld2:enum{a;b};};\n\
    \function f (p:t) :t {switch p.fld1 {}};"