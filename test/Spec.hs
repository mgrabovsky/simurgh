import Control.Applicative (liftA2)
import Control.Monad       (sequence_)
import Test.Tasty
import Test.Tasty.HUnit

import Simurgh.Eval
import Simurgh.Parser
import Simurgh.Pretty
import Simurgh.Syntax

-- TODO: Should we consider syntactic equality (==) rather than α-equivalence?
-- That would require an Eq instance for Expr et al.

main :: IO ()
main = defaultMain tests
  where tests = testGroup "Simurgh" [evaluatorTests, parserTests]

evaluatorTests = testGroup "Evaluator"
    [ testCase "more arguments than binders" $
        testEval moreArgsTerm moreArgsResult
    , testCase "as many arguments as binders" $
        testEval sameArgsTerm sameArgsResult
    , testCase "fewer arguments than binders" $
        testEval fewerArgsTerm fewerArgsResult
    ]
    where
        testEval term expected = assertAeq (eval term) expected
        moreArgsTerm = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"])) [mkVar "Nat", mkVar "Unit", mkLam [("_", mkVar "Nat")] (mkVar "tt"), mkVar "3", mkVar "extra", Set0]
        moreArgsResult = App (mkVar "tt") [mkVar "extra", Set0]
        sameArgsTerm = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"])) [mkVar "Nat", mkVar "Unit", mkLam [("_", mkVar "Nat")] (mkVar "tt"), mkVar "3"]
        sameArgsResult = mkVar "tt"
        fewerArgsTerm = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"])) [mkVar "Nat", mkVar "Unit"]
        fewerArgsResult = mkLam [("f", mkPi [("_", mkVar "Nat")] (mkVar "Unit")), ("a", mkVar "Nat")] (App (mkVar "f") [mkVar "a"])

parserTests = testGroup "Parser: Parsing expressions"
    [ testCase "Set literal" $
        testParse "Set" Set0
    , testCase "whitespace is ignored" $
        testParse "   \n  Set\t\n  " Set0
    , testCase "abstraction is parsed correctly" $
        testParse "  fun (A : Set) (x : A) (y : A) => eq A x y\n"
            (mkLam [("A", Set0), ("x", mkVar "A"), ("y", mkVar "A")] $
                  App (mkVar "eq") (mkVar <$> ["A", "x", "y"]))
    , testCase "variables in abstraction are bound correctly" $
        testParse "fun (a:A) (A:Set) (b:A) (f:C a b) => Set"
            (mkLam [("a", mkVar "A"), ("A", Set0), ("b", mkVar "A"),
                    ("f", App (mkVar "C") (mkVar <$> ["a", "b"]))]
                   Set0)
    ]
    where testParse input expected = sequence_ $
            liftA2 assertAeq (parseExpr input) (Right expected)

assertAeq :: HasCallStack => Expr -> Expr -> Assertion
assertAeq actual expected = assertBool message condition
  where condition = actual `aeq` expected
        message = "Terms are not α-equivalent.\n" <>
                  "Expected\n  " <> prettyPrint actual <>
                  "\nto be equivalent to\n  " <> prettyPrint expected

