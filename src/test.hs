{-# LANGUAGE BangPatterns #-}

import Prelude hiding (writeFile)
import System.IO hiding (writeFile)
import Control.Monad(when)

import Cmm_alex
import Cmm_happy
import Checker
import Astbuilder

{-import Data.Yaml(encode)
import Data.ByteString (writeFile)-}
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.ByteString.Lazy (writeFile)
encode = encodePretty


main :: IO ()
main = do
    program <- readFile "cmm_program.cmm"
    let tokens = alexScanTokens program
    --putStrLn $ concat $ map (\t -> show t ++ "\n") $ tokens
    let tree = happyParseToTree tokens
    print tree
    let errors = check tree
    print errors
    when (errors == []) $ do
        let ast = mkAST tree
        writeFile "cmm_program.ast" $ encode ast
    -- синтаксический, лексический и типовый анализ
    -- построить AST
    
{-

type Reference = (Posed String, Maybe (TExpression))
data TExpression =
    ComplEx [Reference] TExpression
    | CallEx (Posed String) [TExpression]
    | Retrieval Reference
    | StringLiteral (Posed [Int])
    | NumLiteral (Posed Int)
    deriving (Show, Eq)
data TStatement =
    CompSta [TDeclaration] [TStatement]
    | SelSta TExpression TStatement (Maybe TStatement)
    | IterSta TExpression TStatement
    | RetSta (Maybe TExpression)
    | ReadSta Reference
    | ExpSta TExpression
    | EmpSta
    deriving (Show, Eq)
data TDeclaration =
    Intdecl (Posed String)
    | Arrdecl (Posed String) (Posed Int)
    | Fundecl (Posed String) [TDeclaration] TStatement
    | Procdecl (Posed String) [TDeclaration] TStatement
    deriving (Show, Eq)
}
-}