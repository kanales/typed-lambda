import Test.HUnit
import Syntax
import Parser
import Eval

main = runTestTT tests

tests = TestList 
    [ TestLabel "testParseVar" testParseVar
    , TestLabel "testParseApp" testParseApp
    , TestLabel "testParseAbstraction" testParseAbstraction
    , TestLabel "testSubstitution" testSubstitution
    ]

testParseVar = TestCase 
    ( assertEqual "parsing var:" 
        (parse variable "x")
        (Right $ Var "x")
    )

testParseApp = TestCase
    ( assertEqual "parsing app:"
        (parse expr "(x y)")
        (Right $ App (Var "x") (Var "y"))
    )

testParseAbstraction = TestCase
    ( assertEqual "parsing abstraction:"
        (parse expr "\\x. y")
        (Right $ Lam "x" (Var "y"))
    )

testSubstitution = TestCase
    ( assertEqual "substituting [x\\y](x (\\x. x))"
        (subs "x" (Var "y") input)
        (App (Var "y") (Lam "x" (Var "x")))
    )
    where input = App (Var "x") (Lam "x" (Var "x"))