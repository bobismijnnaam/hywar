{-# LANGUAGE RecordWildCards #-}
module Graphviz where

import Data.List
import Debug.Trace
import System.Process

import Types
import PrPrClass
import Parse
import Testing

-- data Expr  = Idf     String                     -- for variables
--            | Num     Int                        -- for numerical constants
--            | Bln     Bool                       -- for boolean constants
--            | Empty                              -- for completeness
-- 
--            | Numeric String Expr Expr           -- for numerical expressions such as "3+5"
--            | Boolean String Expr Expr           -- for boolean expressions
--            | Compose String Expr Expr           -- for composition expressions
--            | Pair    Expr   Expr                -- for pairs: "(3,5)"
--            | Triple  Expr   Expr Expr           -- for triples: "(3,4,5)"
--            | Null                               -- for empty list
--            | Cons    Expr   Expr                -- for list-constructor: 3:[4,5]
--            | Sel     Expr   Expr                -- for selection expressions such as "xs!!i"
--            | IfE     Expr   Expr Expr           -- for if-then-else expressions
-- 
--            | App     Expr   Expr                -- for function application
-- 
--            | Def     Expr   Expr                -- Only pattern as defined expr
--            | Let     [Expr] Expr                -- Only Def-expressions in list
--            | Lambda  Expr   Expr                -- Only pattern as formal parameter

edge :: Int -> Int -> String
edge a b = (show a) ++ " -> " ++ (show b) ++ ";"

name :: Int -> String -> String
name id text = (show id) ++ "[label=\"" ++ text ++ "\"];"

oneExpr c title expr1 = (text, myId)
    where
        (exprText, exprId) = toGraphviz c expr1
        myId = exprId + 1
        text = foldl1 (++) [
            name myId title,
            edge myId exprId,
            exprText
            ]

doubleExpr c title expr1 expr2 = (text, myId)
    where
        (text1, id1) = toGraphviz c expr1
        (text2, id2) = toGraphviz id1 expr2
        myId = id2 + 1
        text = foldl1 (++) [
            name myId title,
            edge myId id1,
            edge myId id2,
            text1,
            text2
            ]

tripleExpr c title expr1 expr2 expr3 = (text, myId)
    where
        (text1, id1) = toGraphviz c expr1
        (text2, id2) = toGraphviz id1 expr2
        (text3, id3) = toGraphviz id2 expr3
        myId = id3 + 1
        text = foldl1 (++) [
            name myId title,
            edge myId id1,
            edge myId id2,
            edge myId id3,
            text1,
            text2,
            text3
            ]

toGraphviz :: Int -> Expr -> (String, Int)
toGraphviz c (Idf idName) = (name (c + 1) ("Idf " ++ idName), c + 1)
toGraphviz c (Num value) = (name (c + 1) ("Num " ++ (show value)), c + 1)
toGraphviz c (Bln value) = (name (c + 1) ("Bool " ++ (show value)), c + 1)

toGraphviz c (Numeric op left right) = doubleExpr c ("Numeric " ++ op) left right
toGraphviz c (Boolean op left right) = doubleExpr c ("Boolean " ++ op) left right
toGraphviz c (Compose op left right) = doubleExpr c ("Compose " ++ op) left right
toGraphviz c (Cons head tail) = doubleExpr c "Cons" head tail
toGraphviz c Null = (name (c + 1) "Null", c + 1)
toGraphviz c (IfE cond true false) = tripleExpr c "IfE" cond true false

toGraphviz c (App f arg) = doubleExpr c "App" f arg

toGraphviz c (Def name body) = doubleExpr c "Def" name body
toGraphviz c (Let exprs body) = (text, myId)
    where
        (bodyText, bodyId) = toGraphviz c body
        allExprs = exprsToGraphviz bodyId exprs 
        allEdges = foldl1 (++) $ map (\(_, exprId) -> edge myId exprId) allExprs 
        allExprTexts = foldl1 (++) $ map fst allExprs
        myId = (snd $ last allExprs) + 1
        text = foldl1 (++) [
            name myId "Let",
            allEdges,
            allExprTexts,
            edge myId bodyId,
            bodyText
            ]
toGraphviz c (Lambda arg body) = doubleExpr c "Lambda" arg body

toGraphviz c (TpDef varName varType) = doubleExpr c "TpDef" varName varType

toGraphviz c (FuncCall funcName args) = (text, myId)
    where
        allExprs = exprsToGraphviz c args 
        myId = (snd $ last allExprs) + 1
        allEdges = foldl1 (++) $ map (\(_, exprId) -> edge myId exprId) allExprs 
        allExprTexts = foldl1 (++) $ map fst allExprs
        text = foldl1 (++) [
            name myId ("FuncCall " ++ funcName),
            allEdges,
            allExprTexts
            ]
toGraphviz c (FuncDef funcName args body) = (text, myId)
    where
        (bodyText, bodyId) = toGraphviz c body
        myId = bodyId + 1
        text = foldl1 (++) [
            name myId ("FuncDef " ++ funcName ++ " (" ++ (foldl1 (\l r -> l ++ ", " ++ r) args) ++ ")"),
            edge myId bodyId,
            bodyText
            ]

toGraphviz c (Type l r) = doubleExpr c "Type" l r
toGraphviz c (List expr) = oneExpr c "List" expr

toGraphviz c (TIdf varName varType) = (name (c + 1) ("TIdf " ++ varName ++ " (" ++ (removeDoubleQuotes $ show varType) ++ ")"), c + 1)

removeDoubleQuotes [] = []
removeDoubleQuotes ('\"':xs) = removeDoubleQuotes xs
removeDoubleQuotes (x:xs) = x : removeDoubleQuotes xs

exprsToGraphviz :: Int -> [Expr] -> [(String, Int)]
exprsToGraphviz c [] = []
exprsToGraphviz c (expr:exprs) = me : others
    where
        me@(myText, myId) = toGraphviz c expr
        others = exprsToGraphviz myId exprs

printGraphviz tree = foldl1 (++) [
    "digraph hcl {",
    fst $ toGraphviz 0 tree,
    "}"
    ]

saveTree expr = createProcess (shell ("echo " ++ (show graph) ++ " | dot -Tpng > img.png"))
    where
        graph = printGraphviz expr -- $ pt2expr $ parse $ ex id

showTree expr = createProcess (shell ("echo " ++ (show graph) ++ " | dot -Tx11"))
    where
        graph = printGraphviz expr -- $ pt2expr $ parse $ ex id

showEx id = showTree $ pt2expr $ parse $ ex id

showParse id = parse $ ex id

showExpr id = pt2expr $ parse $ ex id
