{-# LANGUAGE RecordWildCards #-}
module Graphviz where

import Control.Monad.State

import Data.List
import Debug.Trace
import System.Process

import Types
import PrPrClass
import Parse
import Testing

edge :: Int -> Int -> String
edge a b = (show a) ++ " -> " ++ (show b) ++ ";"

name :: Int -> String -> String
name id text = (show id) ++ "[label=\"" ++ text ++ "\"];"

type CounterMonad a = State Int a

next :: CounterMonad Int
next = state $ \x -> (x, x + 1)

runCount f = runState f 0
runCountFrom f s = runState f s

toGraphviz :: Expr -> CounterMonad (Int, String)
toGraphviz t = case t of
    Idf idName -> one ("Idf " ++ idName)
    Num num -> one ("Num " ++ (show num))
    Bln bool -> one ("Bln " ++ (show bool))

    Empty -> one "Empty"
    Numeric op l r -> two l r ("Numeric " ++ op)
    Boolean op l r -> two l r ("Boolean " ++ op)
    Compose op l r -> two l r ("Compose " ++ op)
    Pair l r -> two l r "Pair"
    Triple l m r -> three l m r "Triple"
    Null -> one "Null"
    Cons l r -> two l r "Cons"
    Sel l r -> two l r "Sel"
    IfE l m r -> three l m r "IfE"
    
    App l r -> two l r "Sel"
    
    Def l r -> two l r "Def"
    Let defs body -> do
        myId <- next
        (_, defsText) <- all (defs ++ [body]) myId    
        return (myId, name myId "Let" ++ defsText)
    Lambda l r -> two l r "Lamdba"

    TpDef l r -> two l r "TpDef"

    FuncCall funcName args -> do
        myId <- next
        (_, defsText) <- all args myId
        return (myId, name myId ("FuncCall " ++ funcName) ++ defsText)
    FuncDef ret args body -> do
        myId <- next
        (_, bodyText) <- all [body] myId
        return (myId, name myId label ++ bodyText)
        where
            label = "FuncDef " ++ ret ++ " (" ++ foldl1 (\x y -> x ++ ", " ++ y) args ++ ")"
    FuncSig args -> one ("FuncSig " ++ show args)
    Type l r -> two l r "Type"
    List m -> do
        myId <- next
        (_, childText) <- all [m] myId
        return (myId, name myId "List" ++ childText)

    TIdf varName varType -> do
        myId <- next
        (_, childText) <- all [trace (show varType) varType] myId
        return (myId, name myId ("TIdf " ++ varName) ++ childText)

    where
        all :: [Expr] -> Int -> CounterMonad ([Int], String)
        all ts root = do
            tsInfo <- mapM toGraphviz ts
            
            let tsIds = map fst tsInfo
            let tsText = concat $ map snd tsInfo
            let edges = concat $ map (edge root) tsIds

            return (tsIds, tsText ++ edges)

        one :: String -> CounterMonad (Int, String)
        one label = do
            myId <- next
            return (myId, name myId label)

        two :: Expr -> Expr -> String -> CounterMonad (Int, String)
        two l r label = do
            myId <- next
            (_, lrText) <- all [l, r] myId
            return (myId, name myId label ++ lrText)

        three :: Expr -> Expr -> Expr -> String -> CounterMonad (Int, String)
        three l m r label = do
            myId <- next
            (_, lmrText) <- all [l, m, r] myId
            return (myId, name myId label ++ lmrText)

removeDoubleQuotes [] = []
removeDoubleQuotes ('\"':xs) = removeDoubleQuotes xs
removeDoubleQuotes (x:xs) = x : removeDoubleQuotes xs

printGraphviz tree = foldl1 (++) [
    "digraph hcl {",
    -- fst $ toGraphviz 0 tree,
    snd $ fst $ runCount (toGraphviz tree),
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
