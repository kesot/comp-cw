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