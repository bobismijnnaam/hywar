{-# LANGUAGE RecordWildCards, QuasiQuotes#-} -- , OverloadedStrings #-}
module Compiler where

import Text.RawString.QQ
import Prelude hiding (lookup)
import Numeric (showIntAtBase)
import Data.Map (Map, fromList, lookup)
import Debug.Trace
import Data.Maybe
import Data.Hashable
import Data.Char
import Types
import Testing
import Parse
import Graphviz

encodeArray :: [(Int, Char)]
encodeArray = [ (0,'A'),  (1,'B'),  (2,'C'),  (3,'D'),  (4,'E'),  (5,'F')                    
              , (6,'G'),  (7,'H'),  (8,'I'),  (9,'J'),  (10,'K'), (11,'L')                    
              , (12,'M'), (13,'N'), (14,'O'), (15,'P'), (16,'Q'), (17,'R')
              , (18,'S'), (19,'T'), (20,'U'), (21,'V'), (22,'W'), (23,'X')
              , (24,'Y'), (25,'Z'), (26,'a'), (27,'b'), (28,'c'), (29,'d')
              , (30,'e'), (31,'f'), (32,'g'), (33,'h'), (34,'i'), (35,'j')
              , (36,'k'), (37,'l'), (38,'m'), (39,'n'), (40,'o'), (41,'p')
              , (42,'q'), (43,'r'), (44,'s'), (45,'t'), (46,'u'), (47,'v')
              , (48,'w'), (49,'x'), (50,'y'), (51,'z'), (52,'0'), (53,'1')
              , (54,'2'), (55,'3'), (56,'4'), (57,'5'), (58,'6'), (59,'7')
              , (60,'8'), (61,'9'), (62,'_'), (63,'/') ]

selectBase64Char :: Int -> Char
selectBase64Char ch = snd (encodeArray!!ch)

----------------
postVisitor :: (Expr -> Expr) -> Expr -> Expr
postVisitor f (Numeric op l r) = Numeric op (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (Boolean op l r) = Boolean op (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (Compose op l r) = Compose op (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (Pair l r) = Pair (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (Triple first s t) = Triple (f (postVisitor f first)) (f (postVisitor f s)) (f (postVisitor f t))
postVisitor f (Cons l r) = Cons (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (Sel l r) = Sel (f (postVisitor f l)) (f (postVisitor f r))
postVisitor f (IfE c t false) = IfE (f (postVisitor f c)) (f (postVisitor f t)) (f (postVisitor f false))

postVisitor f (App name arg) = App (f (postVisitor f name)) (f (postVisitor f arg))

postVisitor f (Def n b) = Def (f (postVisitor f n)) (f (postVisitor f b))
postVisitor f (Let exprs body) = Let (map f (map (postVisitor f) exprs)) (f (postVisitor f body))
postVisitor f (Lambda p b) = Lambda (f (postVisitor f p)) (f (postVisitor f b))

postVisitor f e = case e of
    TpDef n v -> TpDef (f (postVisitor f n)) (f (postVisitor f v))

    FuncCall funcName args -> FuncCall funcName (map f (map (postVisitor f) args))
    FuncDef funcName args body -> FuncDef funcName args (f (postVisitor f body))
    Type varName typeName -> Type (f (postVisitor f varName)) (f (postVisitor f typeName))
    List typeName -> List $ f $ postVisitor f typeName
    
    TIdf varName varType -> TIdf varName $ f $ postVisitor f varType
    _ -> e

----------------

-- Convert App's to FuncCall's (And therewith flatten the App's).
appToFuncCall' :: Expr -> Expr
appToFuncCall' (App (Idf name) arg) = FuncCall name [arg]
appToFuncCall' (App (FuncCall name args) arg) = FuncCall name (args ++ [arg])
appToFuncCall' t = t

appToFuncCall = postVisitor appToFuncCall'

-- Convert definitions of lambda's into funcdefs. Used in combination with the lambdas to
-- def.
defToFuncDef e = case e of
    (Def (Idf name) l@(Lambda arg body)) -> FuncDef name args expr
                                         where
                                            FuncDef "" args expr = defToFuncDef l
    (Lambda (Idf arg) l@(Lambda _ _)) -> FuncDef "" (arg:args) body
                                      where
                                        FuncDef "" args body = defToFuncDef l
    (Lambda (Idf arg) body) -> FuncDef "" [arg] body
    Let exprs body -> Let (map defToFuncDef exprs) body
    _ -> e
    

-- Extract lambdas to Def
gatherLambdas :: Expr -> [Expr]
gatherLambdas t = case t of
    Numeric _ l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Boolean _ l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Compose _ l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Pair l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Triple l m r -> (gatherLambdas l) ++ (gatherLambdas m) ++ (gatherLambdas r)
    Cons l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Sel l r -> (gatherLambdas l) ++ (gatherLambdas r)
    IfE l m r -> (gatherLambdas l) ++ (gatherLambdas m) ++ (gatherLambdas r)

    App l r -> (gatherLambdas l) ++ (gatherLambdas r)

    Def l r -> (gatherLambdas l) ++ (gatherLambdas r)
    Let exprs body -> (concat $ map gatherLambdas exprs) ++ (gatherLambdas body)
    t@(Lambda l r) -> [t]

    FuncCall _ exprs -> concat $ map gatherLambdas exprs
    FuncDef _ _ body -> gatherLambdas body

    _ -> []

lambdaToHash :: Expr -> Expr
lambdaToHash t = case t of
    Numeric op l r -> Numeric op (lambdaToHash l) (lambdaToHash r)
    Boolean op l r -> Boolean op (lambdaToHash l) (lambdaToHash r)
    Compose op l r -> Compose op (lambdaToHash l) (lambdaToHash r)
    Pair l r -> Pair (lambdaToHash l) (lambdaToHash r)
    Triple l m r -> Triple (lambdaToHash l) (lambdaToHash m) (lambdaToHash r)
    Cons l r -> Cons (lambdaToHash l) (lambdaToHash r)
    Sel l r -> Sel (lambdaToHash l) (lambdaToHash r)
    IfE l m r -> IfE (lambdaToHash l) (lambdaToHash m) (lambdaToHash r)

    App l r -> App (lambdaToHash l) (lambdaToHash r)

    Def l r -> Def (lambdaToHash l) (lambdaToHash r)
    Let exprs body -> Let (map lambdaToHash exprs) (lambdaToHash body)
    t@(Lambda l r) -> Idf (getLambdaName t)

    FuncCall funcName exprs -> FuncCall funcName (map lambdaToHash exprs)
    FuncDef funcName args body -> FuncDef funcName args (lambdaToHash body)

    t -> t

hasLambdas :: Expr -> Bool
hasLambdas t = case t of
    Numeric op l r -> (hasLambdas l) || (hasLambdas r)
    Boolean op l r -> (hasLambdas l) || (hasLambdas r)
    Compose op l r -> (hasLambdas l) || (hasLambdas r)
    Pair l r -> (hasLambdas l) || (hasLambdas r)
    Triple l m r -> (hasLambdas l) || (hasLambdas m) || (hasLambdas r)
    Cons l r -> (hasLambdas l) || (hasLambdas r)
    Sel l r -> (hasLambdas l) || (hasLambdas r)
    IfE l m r -> (hasLambdas l) || (hasLambdas m) || (hasLambdas r)

    App l r -> (hasLambdas l) || (hasLambdas r)

    Def l r -> (hasLambdas l) || (hasLambdas r)
    Let exprs body -> (foldl1 (||) (map hasLambdas exprs)) || (hasLambdas body)
    Lambda l r -> True

    FuncCall funcName exprs -> foldl1 (||) (map hasLambdas exprs)
    FuncDef funcName args body -> hasLambdas body
    
    _ -> False

removeSlash [] = []
removeSlash ('/':xs) = "__" ++ removeSlash xs
removeSlash (x:xs) = x : removeSlash xs

getLambdaName t@(Lambda arg body) 
    | val < 0 = prefix ('_' : posVal)
    | otherwise = prefix posVal
    where
        val = hash t
        posVal = removeSlash $ showIntAtBase 64 selectBase64Char (abs val) ""
        prefix = ("lf" ++)

lambdaToDef t@(Lambda arg body) = Def (Idf (getLambdaName t)) t

moveLambdas t@(Let _ _) = Let (exprs ++ allLambdas) nudeTree
    where
        allLambdas = map lambdaToDef $ gatherLambdas t
        Let exprs nudeTree = lambdaToHash t

removeLambdas tree
    | hasLambdas trans = removeLambdas trans
    | otherwise = trans
    where
        trans = defToFuncDef $ moveLambdas $ tree

isTypename :: String -> Bool
isTypename = isUpper . head

doTyping' u@(Cons t@(Idf typename) Null)
    | isTypename typename = List t
    | otherwise = u
doTyping' (Cons t@(List typeExpr) Null) = List t
doTyping' (TpDef n@(Idf _) t@(List _)) = Type n t
doTyping' r@(TpDef n@(Idf _) t@(Idf typename)) = Type n t

doTyping' t = t

doTyping = postVisitor doTyping'

-- Get types of arguments of global let
getGlobalTypes :: Expr -> [(String, Expr)]
getGlobalTypes e = case e of
    (Type (Idf varName) varType) -> [(varName, varType)]
    (Def (Idf varName) (Num _)) -> [(varName, Idf "Int")]
    (Let exprs body) -> concat $ map getGlobalTypes exprs
    (FuncDef name args _) -> [(name, FuncSig $ length args)]
    (Def (TIdf varName varType) _) -> [(varName, varType)]
    _ -> []

applyTypes' :: Map String Expr -> Expr -> Expr
applyTypes' types e = case e of
    Idf varName -> case lookup varName types of
        Just varType -> TIdf varName varType
        Nothing -> Idf varName
    Def (Idf name) varType@(TIdf otherName otherType) -> Def (TIdf name otherType) varType

    _ -> e

applyTypes types = postVisitor (applyTypes' types)

getNames :: Expr -> [String]
getNames (Idf varName) = [varName]
getNames _ = []

getLocalTypes :: Expr -> [(String, Expr)]
getLocalTypes e = case e of
    Numeric _ l r -> (map (\x -> (x, Idf "Int")) ((getNames l) ++ (getNames r))) ++ getLocalTypes l ++ getLocalTypes r
    Boolean _ l r -> (map (\x -> (x, Idf "Bool")) ((getNames l) ++ (getNames r))) ++ getLocalTypes l ++ getLocalTypes r
    Compose _ l r -> (map (\x -> (x, Idf "Compose")) ((getNames l) ++ (getNames r))) ++ getLocalTypes l ++ getLocalTypes r
    Pair l r -> getLocalTypes l ++ getLocalTypes r
    Triple l m r -> getLocalTypes l ++ getLocalTypes m ++ getLocalTypes r
    Null -> []
    Cons l r -> getLocalTypes l ++ getLocalTypes r
    Sel l r -> getLocalTypes l ++ getLocalTypes r
    IfE c t f -> getLocalTypes c ++ getLocalTypes t ++ getLocalTypes f
    
    App f arg -> getLocalTypes f ++ getLocalTypes arg
    Def name body -> getLocalTypes name ++ getLocalTypes body
    Let exprs body -> (concat $ map getLocalTypes exprs) ++ getLocalTypes body

    FuncCall _ args -> concat $ map getLocalTypes args
    FuncDef _ _ body -> getLocalTypes body
    Type _ _ -> [] -- Type should not have Num in it
    List _ -> [] -- Neither
    
    TIdf _ _ -> [] -- Neither

    _ -> []

hasUntypedLeft :: Expr -> Bool
hasUntypedLeft e = case e of
    Idf _ -> True
    Num _ -> False
    Bln _ -> False
    
    Numeric _ l r -> hasUntypedLeft l || hasUntypedLeft r
    Boolean _ l r -> hasUntypedLeft l || hasUntypedLeft r
    Compose _ l r -> hasUntypedLeft l || hasUntypedLeft r
    Pair l r -> hasUntypedLeft l || hasUntypedLeft r
    Triple l m r -> hasUntypedLeft l || hasUntypedLeft m || hasUntypedLeft r
    Cons l r -> hasUntypedLeft l || hasUntypedLeft r
    Sel l r -> hasUntypedLeft l || hasUntypedLeft r
    IfE c t f -> hasUntypedLeft c || hasUntypedLeft t || hasUntypedLeft f
    App l r -> hasUntypedLeft l || hasUntypedLeft r
    Def l r -> hasUntypedLeft l || hasUntypedLeft r
    Let ls r -> (foldl1 (||) (map hasUntypedLeft ls)) || hasUntypedLeft r
    Lambda arg body -> hasUntypedLeft arg || hasUntypedLeft body

    FuncCall _ args -> foldl1 (||) $ map hasUntypedLeft args
    FuncDef _ _ body -> hasUntypedLeft body
    FuncSig args -> False
    Type name typeExpr -> hasUntypedLeft name 
    List _ -> False

    TIdf _ _ -> False

getTypeOfExpr :: Expr -> Expr
getTypeOfExpr e = case e of
    Num _ -> Idf "Int"
    Bln _ -> Idf "Bool"

    Numeric _ _ _ -> Idf "Int"
    Boolean _ _ _ -> Idf "Bool"

    TIdf _ varType -> varType    
    
    _ -> error ((show e) ++ " not supported")

typeLocalVars' (FuncDef name args body) = FuncDef name args typedBody
    where
        localTypes = fromList $ (map (\x -> (x, Idf "Int")) args)
        typedBody = applyTypes localTypes body

typeLocalVars' t = t

getSig (FuncDef name args _) = (name, FuncSig $ length args)
getSig _ = ("", Empty)

typeLocalVars (Let exprs body) = Let partiallyTypedExprs body
    where
        partiallyTypedExprs = map typeLocalVars' exprs

showTaal id = showTree $ doTaal id

saveTaal id = saveTree $ doTaal id

doTypes t
    | hasUntypedLeft t' = doTypes t'
    | otherwise = t'
    where
        t' = typeLocalVars $ applyTypes globalVarTypes t
        globalVarTypes = fromList $ getGlobalTypes t

doTaal id = doTypes first
    where
        first = doTyping $ removeLambdas $ appToFuncCall $ showExpr id
