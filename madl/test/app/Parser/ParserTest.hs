{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-} -- get warnings when we deviate from best practice

-----------------------------------------------------------------------------
-- |
-- Module      :  ParserTest
-- Copyright   :  (c) Wieger Wesselink 2015-2016
--
-----------------------------------------------------------------------------

module Main(main, run) where

import qualified Test.Framework as F
import Test.Framework.Providers.HUnit
import Parser.MadlLexer
import Parser.MadlParser
import Test.HUnit
import Text.Parsec
import Text.Parsec.String (Parser)
-- import qualified Data.Text as T
-- import Debug.Trace

type Comment = String

main :: IO ()
main = F.defaultMain (hUnitTestToTests tests)

makeTest :: Show a => Comment -> String -> Parser a -> Test
makeTest comment_ text parser = TestCase (assertBool comment_ result) where
    result = case parse parser "(source)" text of
        Left _error -> False
        Right _value -> True

makeFailTest :: Show a => Comment -> String -> Parser a -> Test
makeFailTest comment_ text parser = TestCase (assertBool comment_ (not result)) where
    result = case parse parser "(source)" text of
        Left _error -> False
        Right _value -> True

string_testcases :: [String]
string_testcases = [
    "abc0",
    "ABC" ,
    "_bc"
    ]

string_failcases :: [String]
string_failcases = [
    "!"   ,
    "0ABC"
    ]

string_tests :: [Test]
string_tests = (map (\text -> makeTest text text identifier) string_testcases) ++
               (map (\text -> makeFailTest text text identifier) string_failcases)

stringlist_testcases :: [String]
stringlist_testcases = [
    "aap, noot, mies",
    "aap , noot  , mies"
    ]

stringlist_failcases :: [String]
stringlist_failcases = [
    "aap, noot, mies,"
    ]

stringlist_tests :: [Test]
stringlist_tests = (map (\text -> makeTest text text (commaSep identifier)) stringlist_testcases) ++
                   (map (\text -> makeFailTest text text (commaSep identifier)) stringlist_failcases)

typedeclaration_testcases :: [String]
typedeclaration_testcases = [
    "struct A   \n\
    \{          \n\
    \  x : nr;  \n\
    \  y : char \n\
    \}          \n\
    \",

    "struct A    \n\
    \{           \n\
    \  x : nr;   \n\
    \  y : char; \n\
    \}           \n\
    \",

    "struct empty {}",

    "struct A                          \n\
    \{                                 \n\
    \  x : nr;                         \n\
    \  y : enum { a; b };              \n\
    \  z : struct { c: nr; d: char };  \n\
    \  p : union { e: nr; f: char };   \n\
    \}                                 \n\
    \",

    "struct a {              \n\
    \    a : [LOG_SIZE-1:0]; \n\
    \};                      \n\
    \",

    "enum B \n\
    \{      \n\
    \  b1;  \n\
    \  b2   \n\
    \}      \n\
    \",

    "enum B \n\
    \{      \n\
    \  b1;  \n\
    \  b2;  \n\
    \}      \n\
    \",

    "enum empty {}",

    "union C    \n\
    \{          \n\
    \  x : nr;  \n\
    \  y : char \n\
    \}          \n\
    \",

    "union C     \n\
    \{           \n\
    \  x : nr;   \n\
    \  y : char; \n\
    \}           \n\
    \",

    "union empty {}",

    "const req"
    ]

typedeclaration_failcases :: [String]
typedeclaration_failcases = [
    "struct A   \n\
    \{          \n\
    \  x : int; \n\
    \  y : char \n\
    \}          \n\
    \"
    ]

typedeclaration_tests :: [Test]
typedeclaration_tests = (map (\text -> makeTest text text parseTypeDeclaration) typedeclaration_testcases) ++
                        (map (\text -> makeFailTest text text parseTypeDeclaration) typedeclaration_failcases)



channelexpression_testcases :: [String]
channelexpression_testcases = [
    "i"
    ]
channelexpression_tests :: [Test]
channelexpression_tests = (map (\text -> makeTest text text parseChannelExpression) channelexpression_testcases)

networkprimitive_testcases :: [String]
networkprimitive_testcases = [
    "Sink(i)"
    ]
networkprimitive_tests :: [Test]
networkprimitive_tests = (map (\text -> makeTest text text parseNetworkPrimitive) networkprimitive_testcases)

integerexpression_testcases :: [String]
integerexpression_testcases = [
    "1",
    "i",
    "i-5",
    "i+5"
    ]

integerexpression_tests :: [Test]
integerexpression_tests = (map (\text -> makeTest text text parseIntegerExpression) integerexpression_testcases)

sutypeexpression_testcases :: [String]
sutypeexpression_testcases = [
    "enum { a; b }",
    "[3 : 4]",
    "[ 3 : 4 ] ",
    "[3:4]",
    "[a:b]"
    ]

sutypeexpression_failcases :: [String]
sutypeexpression_failcases = [
    "enum { 1; 2 }"
    ]

sutypeexpression_tests :: [Test]
sutypeexpression_tests = (map (\text -> makeTest text text parseSUTypeExpression) sutypeexpression_testcases) ++
                         (map (\text -> makeFailTest text text parseSUTypeExpression) sutypeexpression_failcases)

typeexpression_testcases :: [String]
typeexpression_testcases = [
    "req"
    ]

typeexpression_failcases :: [String]
typeexpression_failcases = [
    "otherwise",
    "int"
    ]

typeexpression_tests :: [Test]
typeexpression_tests = (map (\text -> makeTest text text parseTypeExpression) typeexpression_testcases) ++
                       (map (\text -> makeFailTest text text parseTypeExpression) typeexpression_failcases)

switchexpression_testcases :: [String]
switchexpression_testcases = [
    "otherwise",
    "sometype",
    "a.b == 1",
    "ARpkt.aid == idx + 1"
    ]

switchexpression_tests :: [Test]
switchexpression_tests = (map (\text -> makeTest text text parseSwitchExpression) switchexpression_testcases)

function_testcases :: [String]
function_testcases = [
    "function f(p: Req): Rsp \n\
    \{                       \n\
    \  t = rsp;              \n\
    \  x = p.x;              \n\
    \  y = p.y;              \n\
    \}                       \n",

    "function m(p: Req): Rsp \n\
    \{                       \n\
    \  switch p {            \n\
    \    case wreq: wrsp;    \n\
    \    case rreq: rrsp;    \n\
    \  }                     \n\
    \}                       \n",

    "function toRsp(m: req): rsp \n\
    \{                           \n\
    \  rsp;                      \n\
    \}                           \n",

    "function f(a:msgtype, b:msgtype): msgtype { \n\
    \ switch a {                                 \n\
    \   case rsp: a;                             \n\
    \   case req: switch b {                     \n\
    \     case rsp: b;                           \n\
    \     case req: rsp;                         \n\
    \   }                                        \n\
    \ }                                          \n\
    \}                                           \n",

    "function g(a:msg): msg {            \n\
    \ type = rsp;                        \n\
    \ content.header = a.content.header; \n\
    \ content.data = switch a {          \n\
    \   case rsp: 0;                     \n\
    \   case req: a.content.data;        \n\
    \ }                                  \n\
    \}                                   \n",

    "function f ( p : pkt ) : pkt { \n\
    \ b{ d = 1; };                  \n\
    \}                              \n",

    "function f ( p : pkt ) : pkt { \n\
    \ switch p {                    \n\
    \    case a: a{};               \n\
    \      case b: b{d = 1;};       \n\
    \  }                            \n\
    \}                              \n",

    "function f ( p : pkt ) : pkt {                                    \n\
    \  switch p {                                                      \n\
    \      case a: a{red;};                                            \n\
    \      case b: b{ switch p.b {                                     \n\
    \                     case c: d{ data = p.b.c.data; other = 1; };  \n\
    \                     case d: c{ data = 1; };                      \n\
    \                 };                                               \n\
    \              };                                                  \n\
    \  };                                                              \n\
    \}                                                                 \n",

    "function decACR (p: SPRouter8pkt) : SPRouter8pkt {  \n\
    \    numACR = 1 + p.numACR;                          \n\
    \    numCCW = p.numCCW - 1;                          \n\
    \    numCW = p.numCW;                                \n\
    \}                                                   \n",

    "function decACR (p: SPRouter8pkt) : SPRouter8pkt {  \n\
    \    numACR = if (p.numACR == 0)                     \n\
    \         0 else p.numACR - 1;                       \n\
    \    numCCW = if (p.numCCW == 0)                     \n\
    \         0; else {p.numCCW - 1;};                   \n\
    \    numCW = if (p.numCW == 0)                       \n\
    \        {0} else {p.numCW - 1};                     \n\
    \}                                                   \n",

    "function decACR (p: pkt) : pkt {                    \n\
    \    if (SIZE > 0)                                   \n\
    \     switch p {                                     \n\
    \      case a: a{red;};                              \n\
    \      case b: a{blue;};                             \n\
    \     } else b{};                                    \n\
    \}                                                   \n"
    ]

function_tests :: [Test]
function_tests = (map (\text -> makeTest text text parseFunctionDeclaration) function_testcases)

predicatedeclaration_testcases :: [String]
predicatedeclaration_testcases = [
    "pred f(a : type, b : type)  \n\
    \{                           \n\
    \  a.x == b.x && a.y == b.y; \n\
    \}                           \n",

    "pred f (p : pkt, q : pkt) { \n\
    \    switch p {              \n\
    \      case req : false;     \n\
    \      case rsp : true;      \n\
    \    }                       \n\
    \}                           \n",

    "pred f (p : pkt, q : pkt) {                     \n\
    \    if (p.id == 0) { true; } else p.id == q.id  \n\
    \}                                               \n"
    ]

predicatedeclaration_tests :: [Test]
predicatedeclaration_tests = (map (\text -> makeTest text text parsePredicateDeclaration) predicatedeclaration_testcases)

booleanexpression_testcases :: [String]
booleanexpression_testcases = [
    "true",
    "false",
    "x.y == x.z",
    "x == 3",
    "a.x == b.y && c.x == d.x",
    "a.x == b.y || c.x == d.x",
    "!false",
    "x != 3",
    "x < 3",
     "x <= 3",
     "x >= 3",
     "x > 3"
    ]

booleanexpression_tests :: [Test]
booleanexpression_tests = (map (\text -> makeTest text text parseBooleanExpression) booleanexpression_testcases)

dataexpression_testcases :: [String]
dataexpression_testcases = [
    "x.y",
    "3",
    "f(x, g(y))",
    "x.y + f(3) + 2 + a.b"
    ]

dataexpression_tests :: [Test]
dataexpression_tests = (map (\text -> makeTest text text parseDataExpression) dataexpression_testcases)

primitiveexpression_testcases :: [String]
primitiveexpression_testcases = [
    "x",
    "Fork(x)",
    "Source(e)",
    "Sink(e)",
    "Switch(a, pkt.id == idx, otherwise)",
    "Match1()",
    "Switch(c[x], ARpkt.aid == idx + 1, otherwise)"
    ]

primitiveexpression_tests :: [Test]
primitiveexpression_tests = (map (\text -> makeTest text text parsePrimitiveExpression) primitiveexpression_testcases)

network_testcases :: [String]
network_testcases = [
    "chan q0;",
    "chan q0 := Source(x);",

    "chan up, down; chan q0 := Sink(Queue(2, CtrlJoin(up, down)));",

    "chan q0, q1 := Fork(Source(type));         \n\
    \chan b1, r1 := Switch(Queue(2, q0), type); \n\
    \chan r2, b2 := Switch(Queue(2, q1), type); \n\
    \Sink(CtrlJoin(r1, r2));                    \n\
    \Sink(CtrlJoin(b1, b2));                    \n",

    "chan x := Queue(2, T(Queue(2, S(x))));",

    "chan x := Queue(2, D(Queue(2, C(x, Source(Repl)))));",

    "const type;                                         \n\
    \chan q0, q1 := Switch(NSource(1, type), type);      \n\
    \chan q2 := CtrlJoin(Queue(1, q0), Queue(1, q1));    \n\
    \chan q3, q4 := Fork(Queue(1, q2));                  \n\
    \Sink(Queue(1, Merge(Queue(1, q3), Queue(1, q4))));  \n",

    "chan x0 := Source(type);            \n\
    \chan x1, x2 := Fork(x0);            \n\
    \chan x6 := Queue(2, x1);            \n\
    \chan x3 := Source(type);            \n\
    \chan x5 := Queue(2, Merge(x2, x3)); \n\
    \chan x7, x8 := Switch(x5, type);    \n\
    \chan x8prime := Function(type, x8); \n\
    \chan x9 := CtrlJoin(x7, x6);        \n\
    \chan x10 := Merge(x8prime, x9);     \n\
    \Sink(x10);                          \n",

    "chan q0 := Source(x); chan q1 := Source(y);",

    "chan i; Sink(i);",

    "macro Agent(bus<2> i) => bus<2> o {        \n\
    \  let o[0] := Source(type);                \n\
    \  Sink(i[1]);                              \n\
    \  let o[1] := Function(type, Delay(i[0])); \n\
    \};                                         \n",

    "chan a, b; chan q0 := ControlJoin(a,b);",

    "chan b1, r1 := CtrlJoin(q1, q2); chan q1, q2;",

    "enum B                                            \n\
    \{                                                 \n\
    \  b1;                                             \n\
    \  b2                                              \n\
    \};                                                \n\
    \                                                  \n\
    \function f(p: Req): Rsp                           \n\
    \{                                                 \n\
    \  t = rsp;                                        \n\
    \  x = p.x;                                        \n\
    \  y = p.y;                                        \n\
    \};                                                \n\
    \                                                  \n\
    \chan q0, q1 := Fork(Source(type));                \n\
    \                                                  \n\
    \struct A                                          \n\
    \{                                                 \n\
    \  x : nr;                                         \n\
    \  y : char;                                       \n\
    \};                                                \n\
    \                                                  \n\
    \chan b1, r1 := Switch(Queue(2, q0), type);        \n\
    \chan r2, b2 := Switch(Queue(2, q1), type);        \n",

    "chan i; chan o := Switch(i, otherwise);",

    "pred f(a : type, b : type)  \n\
    \{                           \n\
    \  a.x == b.x && a.y == b.y; \n\
    \};                          \n\
    \                            \n\
    \chan i;                     \n\
    \Sink(i);                    \n",

    "bus<2> a;",

    "bus<2> a, b, c;",

    "chan q0, q1 := Fork(Source(type));   \n\
    \for (int size = 2; size < 5; size++) \n\
    \{                                    \n\
    \  Queue(size, Merge(a, b));          \n\
    \};                                   \n",

    "/* comment */                      \n\
    \chan q0, q1 := Fork(Source(type)); \n",

    unlines ["// comment", "chan q0, q1 := Fork(Source(type));"],

    -- networkRedBlue
    "chan q0, q1 := Fork(Source(type));         \n\
    \chan b1, r1 := Switch(Queue(2, q0), type); \n\
    \chan r2, b2 := Switch(Queue(2, q1), type); \n\
    \                                           \n\
    \Sink(CtrlJoin(r1, r2));                    \n\
    \Sink(CtrlJoin(b1, b2));                    \n",

    -- networkReqRsp
    "chan x0 := Source(type);           \n\
    \                                   \n\
    \chan x1, x2 := Fork(x0);           \n\
    \chan x6 := Queue(2, x1);           \n\
    \                                   \n\
    \chan x3 := Source(type);           \n\
    \chan x5 := Queue(2, Merge(x2, x3));\n\
    \                                   \n\
    \chan x7, x8 := Switch(x5, type);   \n\
    \chan x8prime := Function(type, x8);\n\
    \                                   \n\
    \chan x9 := CtrlJoin(x7, x6);       \n\
    \chan x10 := Merge(x8prime, x9);    \n\
    \Sink(x10);                         \n",

    -- networkTwoAgents
    "param int dx = 2;                                                            \n\
    \param int N = 2;                                                             \n\
    \param int counters = 2;                                                      \n\
    \                                                                             \n\
    \macro Delay(chan i) => chan o {                                              \n\
    \ chan wait := Source(type);                                                  \n\
    \ let o := CtrlJoin(i, wait);                                                 \n\
    \};                                                                           \n\
    \                                                                             \n\
    \macro CreditCounter(chan i) => chan o {                                      \n\
    \ chan q_in;                                                                  \n\
    \ let q_in, o := Fork(Source(type));                                          \n\
    \ Sink(CtrlJoin(Queue(counters, q_in), i));                                   \n\
    \};                                                                           \n\
    \                                                                             \n\
    \macro DualChannel(bus<2> i) => bus<2> o {                                    \n\
    \ chan count_rsp, count_req;                                                  \n\
    \                                                                             \n\
    \ chan data_in := Merge(CtrlJoin(i[0], count_req), CtrlJoin(i[1], count_rsp));\n\
    \ chan data_out := Queue(dx, data_in);                                        \n\
    \                                                                             \n\
    \                                                                             \n\
    \ chan buffer_req_in, buffer_rsp_in := Switch(type, data_out);                \n\
    \                                                                             \n\
    \ chan buffer_req_out := Queue(N, buffer_req_in);                             \n\
    \ chan buffer_rsp_out := Queue(N, buffer_rsp_in);                             \n\
    \                                                                             \n\
    \ chan req_out, rsp_out;                                                      \n\
    \ let o[0], req_out := Fork(buffer_req_out);                                  \n\
    \ let o[1], rsp_out := Fork(buffer_rsp_out);                                  \n\
    \                                                                             \n\
    \ let count_rsp := Queue(N, Queue(dx, CreditCounter(rsp_out)));               \n\
    \ let count_req := Queue(N, Queue(dx, CreditCounter(req_out)));               \n\
    \};                                                                           \n\
    \                                                                             \n\
    \                                                                             \n\
    \macro Agent(bus<2> i) => bus<2> o {                                          \n\
    \ let o[0] := Source(type);                                                   \n\
    \ Sink(i[1]);                                                                 \n\
    \                                                                             \n\
    \ let o[1] := Function(type, Delay(i[0]));                                    \n\
    \};                                                                           \n\
    \                                                                             \n\
    \bus<2> agent0_in;                                                            \n\
    \bus<2> agent0_out := Agent(agent0_in);                                       \n\
    \bus<2> agent1_out := Agent(DualChannel(agent0_out));                         \n\
    \let agent0_in := DualChannel(agent1_out);                                    \n",

    -- networkLoadBalancer
    "chan lane00,lane01,lane02 := LoadBalancer(Source(type)); \n\
    \Sink(lane00);                                            \n\
    \Sink(lane01);                                            \n\
    \Sink(lane02);                                            \n",

    -- networkSimpleMacro
    "const val;                                  \n\
    \                                            \n\
    \macro simpleMacro (chan in) => chan o {     \n\
    \   let o:= Queue(2,in);                     \n\
    \};                                          \n\
    \                                            \n\
    \Sink(simpleMacro(Source(val)));             \n",

    -- networkComplexSwitch
    "struct pkt {                                       \n\
    \    id : [1:0];                                    \n\
    \};                                                 \n\
    \param int idx = 3;                                 \n\
    \chan a := Source(pkt);                             \n\
    \chan b, c := Switch(a, pkt.id == idx, otherwise);  \n\
    \Sink(b); Sink(c);                                  \n",

    -- networkIfThenElse
    "param int idx = 3;                                   \n\
    \chan a := Source(pkt);                               \n\
    \if (idx % 4 == 0) {                                  \n\
    \  chan b, c := Switch(a, pkt.id == idx, otherwise);  \n\
    \} else {                                             \n\
    \  chan b, c := Switch(a, pkt.id != idx, otherwise);  \n\
    \};                                                   \n\
    \Sink(b); Sink(c);                                    \n"
    ]

network_tests :: [Test]
network_tests = (map (\text -> makeTest text text parseNetwork) network_testcases)

tests :: Test
tests = TestList (string_tests ++
                  typedeclaration_tests ++
                  stringlist_tests ++
                  typedeclaration_tests ++
                  channelexpression_tests ++
                  networkprimitive_tests ++
                  integerexpression_tests ++
                  sutypeexpression_tests ++
                  typeexpression_tests ++
                  switchexpression_tests ++
                  function_tests ++
                  predicatedeclaration_tests ++
                  booleanexpression_tests ++
                  dataexpression_tests ++
                  primitiveexpression_tests ++
                  network_tests
                 )

-- shortcut to run the tests
run :: IO Counts
run = runTestTT tests
