import Control.Applicative (liftA2)
import Control.Monad       (sequence_)
import Test.Tasty
import Test.Tasty.HUnit

import Simurgh.Syntax
import Simurgh.Parser
import Simurgh.Pretty

main :: IO ()
main = defaultMain tests
  where tests = testGroup "Parser" [parserTests]

parserTests = testGroup "Parsing expressions"
    [ testCase "parse Set literal" $
        testParse "Set" Set0
    , testCase "ignore white space" $
        testParse "   \n  Set\t\n  " Set0
    , testCase "parse lambda function" $
        testParse "  fun (A : Set) (x : A) (y : A) => eq A x y\n"
            (mkLam [("A", Set0), ("x", mkVar "A"),
                   ("y", mkVar "A")] $
                  App (mkVar "eq") (mkVar <$> ["A", "x", "y"]))
    ]
    where testParse input expected = sequence_ $
            liftA2 assertAeq (parseExpr input) (Right expected)

assertAeq :: HasCallStack => Expr -> Expr -> Assertion
assertAeq actual expected = assertBool message condition
  where condition = actual `aeq` expected
        message = "Terms are not α-equivalent.\n" <>
                  "Expected\n  " <> prettyPrint actual <>
                  "\nto be equivalent to\n  " <> prettyPrint expected

