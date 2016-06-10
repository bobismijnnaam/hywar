{-# LANGUAGE RecordWildCards, QuasiQuotes#-} -- , OverloadedStrings #-}
module Compiler where

import System.Process
import System.Directory
import System.FilePath
import Text.RawString.QQ
import Prelude hiding (lookup)
import Numeric (showIntAtBase)
import Data.Char (intToDigit, isUpper)
import Data.Map (Map, fromList, lookup)
import Debug.Trace
-- import Data.List
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

isValidProgram :: Expr -> Bool
isValidProgram (Let exprs body) = True
isValidProgram _ = False

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
    FuncCall funcName args -> FuncCall funcName (map f (map (postVisitor f) args))
    FuncDef funcName args body -> FuncDef funcName args (f (postVisitor f body))
    Type varName typeName -> Type (f (postVisitor f varName)) (f (postVisitor f typeName))
    List typeName -> List $ f $ postVisitor f typeName
    
    TIdf varName varType -> TIdf varName $ f $ postVisitor f varType
    _ -> e

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
gatherLambdas (Numeric _ l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Boolean _ l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Compose _ l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Pair l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Triple l m r) = (gatherLambdas l) ++ (gatherLambdas m) ++ (gatherLambdas r)
gatherLambdas (Cons l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Sel l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (IfE l m r) = (gatherLambdas l) ++ (gatherLambdas m) ++ (gatherLambdas r)

gatherLambdas (App l r) = (gatherLambdas l) ++ (gatherLambdas r)

gatherLambdas (Def l r) = (gatherLambdas l) ++ (gatherLambdas r)
gatherLambdas (Let exprs body) = (concat $ map gatherLambdas exprs) ++ (gatherLambdas body)
gatherLambdas t@(Lambda l r) = [t]

gatherLambdas (FuncCall _ exprs) = concat $ map gatherLambdas exprs
gatherLambdas (FuncDef _ _ body) = gatherLambdas body

gatherLambdas _ = []

lambdaToHash :: Expr -> Expr
lambdaToHash (Numeric op l r) = Numeric op (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Boolean op l r) = Boolean op (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Compose op l r) = Compose op (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Pair l r) = Pair (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Triple l m r) = Triple (lambdaToHash l) (lambdaToHash m) (lambdaToHash r)
lambdaToHash (Cons l r) = Cons (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Sel l r) = Sel (lambdaToHash l) (lambdaToHash r)
lambdaToHash (IfE l m r) = IfE (lambdaToHash l) (lambdaToHash m) (lambdaToHash r)

lambdaToHash (App l r) = App (lambdaToHash l) (lambdaToHash r)

lambdaToHash (Def l r) = Def (lambdaToHash l) (lambdaToHash r)
lambdaToHash (Let exprs body) = Let (map lambdaToHash exprs) (lambdaToHash body)
lambdaToHash t@(Lambda l r) = Idf (getLambdaName t)

lambdaToHash (FuncCall funcName exprs) = FuncCall funcName (map lambdaToHash exprs)
lambdaToHash (FuncDef funcName args body) = FuncDef funcName args (lambdaToHash body)

lambdaToHash t = t

hasLambdas :: Expr -> Bool
hasLambdas (Numeric op l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Boolean op l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Compose op l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Pair l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Triple l m r) = (hasLambdas l) || (hasLambdas m) || (hasLambdas r)
hasLambdas (Cons l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Sel l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (IfE l m r) = (hasLambdas l) || (hasLambdas m) || (hasLambdas r)

hasLambdas (App l r) = (hasLambdas l) || (hasLambdas r)

hasLambdas (Def l r) = (hasLambdas l) || (hasLambdas r)
hasLambdas (Let exprs body) = (foldl1 (||) (map hasLambdas exprs)) || (hasLambdas body)
hasLambdas t@(Lambda l r) = True

hasLambdas (FuncCall funcName exprs) = foldl1 (||) (map hasLambdas exprs)
hasLambdas (FuncDef funcName args body) = hasLambdas body

hasLambdas t = False

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
-- Do typing! omg :o

isTypename :: String -> Bool
isTypename = isUpper . head

doTyping' u@(Cons t@(Idf typename) Null)
    | isTypename typename = List t
    | otherwise = u
doTyping' (Cons t@(List typeExpr) Null) = List t
doTyping' (Def n@(Idf _) t@(List _)) = Type n t
doTyping' r@(Def n@(Idf _) t@(Idf typename))
    | isTypename typename = Type n t
    | otherwise = r

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

-- TODO: Add types for map fold etc.
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
        -- sigs = fromList (filter (\(name, sig) -> sig /= Empty) (map getSig partiallyTypedExprs))
        -- typedExprs = map (applyTypes sigs) partiallyTypedExprs
        -- typedBody = applyTypes sigs body
        

showTaal id = showTree $ doTaal id

saveTaal id = saveTree $ doTaal id

-- FuncDef signature and type finding is also happening in globalVarTypes, and must happen more than once
-- (like the removeLambda stuff)

doTypes t
    | hasUntypedLeft t' = doTypes t'
    | otherwise = t'
    where
        t' = typeLocalVars $ applyTypes globalVarTypes t
        globalVarTypes = fromList $ getGlobalTypes t

doTaal id = doTypes first
    where
        first = doTyping $ removeLambdas $ appToFuncCall $ showExpr id

-- Code generation begins here
spaced :: String -> String
spaced t = " " ++ t ++ " "
parens :: String -> String
parens t = "(" ++ t ++ ")"
brackets :: String -> String
brackets t = "[" ++ t ++ "]"
separate :: String -> [String] -> String
separate x ys = foldl1 (\l r -> l ++ x ++ r) ys

kernelBody :: Expr -> String
kernelBody (FuncCall f args) = "kernel void GPUMonad" ++ (parens kernelArgs) ++ " {\n" ++ body ++ "\n}"
    where
        kernelArgs = case f of
            "filter" -> "__global int *out, __global int *count"
            "reduce" -> "__global int *gOut, __global int *gCountWhere, __global int *gCountDone, __local int *lOut"
            "map" -> "__global int *out"
            "plow" -> "__global int *out, __global int *count"
            "zipWith" -> "__global int *out"
            
        body = case f of
            "filter" -> "if "
                ++ parens (funcName
                        ++ parens (listArg ++ brackets "get_global_id(0)")
                        )
                ++ "{\n"
                ++ "out[atomic_inc(count)] = "
                ++ listArg ++ brackets "get_global_id(0)"
                ++ ";\n"
                ++ "}"
                where
                    [TIdf funcName _, TIdf listArg _] = args

            "reduce" -> "lOut[get_local_id(0)] = " ++ listArg ++ "[get_global_id(0)];\n"
                ++ "barrier(CLK_LOCAL_MEM_FENCE);\n"
                ++ "if (!get_local_id(0)) {\n"
                ++ "for (int i = 1; i < get_local_size(0); ++i) {\n"
                ++ " lOut[0] = " ++ funcName ++ "(lOut[0], lOut[i]);\n"
                ++ "}\n"
                ++ "gOut[atomic_inc(gCountWhere)] = lOut[0];\n"
                ++ "barrier(CLK_GLOBAL_MEM_FENCE);\n"
                ++ "int myTurn = atomic_inc(gCountDone);\n"
                ++ "if (myTurn == get_num_groups(0) - 1) {\n"
                ++ "for (int i = 1; i < get_num_groups(0); ++i) {\n"
                ++ "gOut[0] = " ++ funcName ++ "(gOut[0], gOut[i]);\n"
                ++ "}\n"
                ++ "}\n"
                ++ "}"
                where
                    [TIdf funcName _, TIdf listArg _] = args

            "map" -> "out[get_global_id(0)] = "
                ++ funcName
                ++ parens (listArg ++ brackets "get_global_id(0)")
                ++ ";"
                where
                    [TIdf funcName _, TIdf listArg _] = args

            "plow" -> "out[get_global_id(0)] = " ++ listArg ++ "[get_global_id(0)];\n"
                ++ "barrier(CLK_GLOBAL_MEM_FENCE);\n"
                ++ "if (!get_local_id(0)) {\n"
                ++ "int offset = get_local_size(0) * get_group_id(0);\n"
                ++ "for (int i = 1; i < get_local_size(0); ++i) {\n"
                ++ "out[offset + i] = " ++ funcName ++ "(out[offset + i - 1], out[offset + i]);\n"
                ++ "}\n"
                ++ "int myTurn = atomic_inc(count);\n"
                ++ "if (myTurn == get_num_groups(0) - 1) {\n"
                ++ "    for (int group = 1; group < get_num_groups(0); ++group) {\n"
                ++ "        offset = group * get_local_size(0);\n"
                ++ "        for (int workItem = 0; workItem < get_local_size(0); ++workItem) {\n"
                ++ "            out[offset + workItem] = " ++ funcName ++ "(out[offset - 1], out[offset + workItem]);\n"
                ++ "        }\n"
                ++ "    }\n"
                ++ "}\n"
                ++ "}\n"
                where
                    [TIdf funcName _, TIdf listArg _] = args
                
            "zipWith" -> "out[get_global_id(0)] = "
                ++ funcName
                ++ parens (separate ", " (map (++(brackets "get_global_id(0)")) [listArg1, listArg2]))
                ++ ";"
                where
                    [TIdf funcName _, TIdf listArg1 _, TIdf listArg2 _] = args

generateDef :: Expr -> String
generateDef t = case t of
    Type (TIdf varName _) (Idf "Int") -> "__constant int " ++ varName ++ " = {!" ++ varName ++ "_contents}"
    Type (TIdf varName _) (List (Idf "Int")) -> "__constant int " ++ varName ++ "[{!" ++ varName ++ "_length}] " ++ " = {!" ++ varName ++ "_contents}"
    Def (TIdf varName _) (Num num) -> "__constant int " ++ varName ++ " = " ++ show num
    Def (TIdf varName _) (TIdf funcName (FuncSig n)) -> 
            "int " 
            ++ varName
            ++ (parens argList)
            ++ " {\n    return "
            ++ funcName ++ "(" ++ callList ++ ");\n"
            ++ "}"
        where
            argList = separate ", " (map (\x -> "int v" ++ show x) [1..n])
            callList = separate ", " (map (\x -> "v" ++ show x) [1..n])
    FuncDef funcName args body -> "int "
        ++ funcName
        ++ (parens argList)
        ++ " {\n    return "
        ++ bodyText
        ++ ";\n}"
        where
            argList = separate ", " (map (\x -> "int " ++ x) args)
            bodyText = toOpenCLC body
    _ -> ""

generateSig :: Expr -> String
generateSig t = case t of
    FuncDef funcName args _ -> "int "
        ++ funcName
        ++ "("
        ++ (separate ", " (replicate (length args) "int"))
        ++ ")"
    Def (TIdf funcName (FuncSig n)) _ -> "int " 
        ++ funcName
        ++ "("
        ++ (separate ", " (replicate n "int"))
        ++ ")"
    _ -> ""

base :: String
base = [r|

int mod(int a, int b) {
    return a % b;
}

|]

toOpenCLC :: Expr -> String
toOpenCLC t = case t of
    Let exprs body -> unlines [base, sigs, globals, kernel]
        where
            sigs = (separate ";\n" (filter (/="") (map generateSig exprs))) ++ ";\n"
            globals = (foldl1 (\x y -> x ++ ";\n" ++ y) $ map generateDef exprs)
            kernel = kernelBody body


    TIdf varName _ -> varName
    Num num -> show num
    Bln True -> show 1
    Bln False -> show 0

    Numeric op l r -> (parens $ toOpenCLC l) ++ spaced op ++ (parens $ toOpenCLC r)
    Boolean op l r -> (parens $ toOpenCLC l) ++ spaced op ++ (parens $ toOpenCLC r)
    Compose _ _ _ -> error "compose not implemented"
    IfE c t f -> (parens $ toOpenCLC c) ++ " ? " ++ (parens $ toOpenCLC t) ++ " : " ++ (parens $ toOpenCLC f)

    FuncCall funcName args -> funcName ++ (parens $ separate ", " $ map parens $ map toOpenCLC args)

    _ -> error $ "Unsupported: " ++ show t

-- Library generation

getInput (Type (TIdf varName varType) _) = [(varName, varType)]
getInput _ = []
getInputs = concat . map getInput

generateCppSignature :: Expr -> String -> String
generateCppSignature (Let exprs body) kernelName = returnType ++ " " ++ kernelName ++ parens argList
    where
        inputs = getInputs exprs

        convertInput (List (Idf "Int")) = "std::vector<int>"
        convertInput (Idf "Int") = "int"
        convertedInputs = map (\(varName, varType) -> (varName, convertInput varType)) inputs
        
        argList = separate ", " parts
            where
                parts = map (\(varName, varType) -> varType ++ " " ++ varName) convertedInputs
        
        FuncCall primitive _ = body
        returnType = case primitive of
            "filter" -> "std::vector<int>"
            "reduce" -> "int"
            "map" -> "std::vector<int>"
            "plow" -> "std::vector<int>"
            "zipWith" -> "std::vector<int>"
        
generateKernelReplacementCode :: Expr -> String
generateKernelReplacementCode (Let exprs body) = unlines $ map makeReplaceCode inputs
    where
        inputs = getInputs exprs

        makeReplaceCode (varName, Idf "Int") =
            "kernelSource = replace(kernelSource, \"{!"
            ++ varName
            ++ "_contents}\", std::to_string("
            ++ varName
            ++ "));"

        makeReplaceCode (varName, List (Idf "Int")) =
            "auto "
            ++ varName
            ++ "_strings = map([](int i){return std::to_string(i);}, "
            ++ varName
            ++ ");\n"
            ++ "kernelSource = replace(kernelSource, \"{!"
            ++ varName
            ++ "_length}\", std::to_string("
            ++ varName
            ++ ".size()));\n"
            ++ "kernelSource = replace(kernelSource, \"{!"
            ++ varName
            ++ "_contents}\", braces(params("
            ++ varName
            ++ "_strings)));"

makeInclude :: String -> String
makeInclude name@('<':_) = "#include " ++ name
makeInclude name = "#include \"" ++ name ++ "\""
            
includes :: String
includes = unlines $ map makeInclude [
    "<iostream>",
    "test6003.hpp",
    "utils.hpp",
    "CL/cl.hpp"
    ]

generateWorkerSizeDiscoveryCode :: Expr -> String    
generateWorkerSizeDiscoveryCode (Let exprs body) = case primitive of
    "filter" -> 
        "int n = " ++ arg ++ ".size();"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body
    
    "reduce" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check (arg ++ ".size()")
        where
            FuncCall _ [_, TIdf arg _] = body

    "map" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body
    
    "plow" ->
        "int n = " ++ arg ++ ".size();\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf arg _] = body

    "zipWith" ->
        "int n = std::min(" ++ l ++ ".size(), " ++ r ++ ".size());\n"
        ++ mod64Check "n"
        where
            FuncCall _ [_, TIdf l _, TIdf r _] = body

    where
        FuncCall primitive _ = body

        mod64Check count = 
            "if ((" ++ count ++ ") % workGroupSize != 0) {\n"
            ++ "throw std::invalid_argument(\"Job size should be multiple of \" + std::to_string(workGroupSize));\n"
            ++ "}"

              -- Name, Flags (read/write), size (integers)
data Buffer = Buffer String String String
              -- Or just size (in integers)
            | Local String
            deriving (Show)

generateBuffers :: Expr -> [Buffer]
generateBuffers (Let exprs body) = case primitive of
    --"filter" -> "__global int *out, __global int *count"
    --"reduce" -> "__global int *gOut, __global int *gCountWhere, __global int *gCountDone, __local int *lOut"
    --"map" -> "__global int *out"
    --"plow" -> "__global int *out"
    --"zipWith" -> "__global int *out"
    "filter" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n",
        Buffer "count" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1"
        ]
    
    "reduce" -> [
        Buffer "gOut" "CL_MEM_WRITE_ONLY" "xs.size()/workGroupSize",
        Buffer "gCountWhere" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1", 
        Buffer "gCountDone" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1",
        Local "workGroupSize"
        ]

    "map" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n"
        ]

    "plow" -> [
        Buffer "out" "CL_MEM_READ_WRITE" "n",
        Buffer "count" "CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR" "1"
        ]

    "zipWith" -> [
        Buffer "out" "CL_MEM_WRITE_ONLY" "n"
        ]

    where
        FuncCall primitive _ = body

generateOutputArray :: Expr -> String
generateOutputArray (Let exprs body) = case primitive of
    "filter" -> withCountVar
    "plow" -> withCountVar
    "reduce" -> 
        "auto h_out = std::make_unique<int[]>(n / workGroupSize);\n"
        ++ "std::fill_n(h_out.get(), n / workGroupSize, 0);\n"
        ++ "auto h_count = std::make_unique<int[]>(1);\n"
        ++ "h_count[0] = 0;"
    _ ->
        "auto h_out = std::make_unique<int[]>(n);\n"
        ++ "std::fill_n(h_out.get(), n, 0);"
    where
        FuncCall primitive _ = body
        
        withCountVar = 
            "auto h_out = std::make_unique<int[]>(n);\n"
            ++ "std::fill_n(h_out.get(), n, 0);\n"
            ++ "auto h_count = std::make_unique<int[]>(1);\n"
            ++ "h_count[0] = 0;"

generateKernelLiteral :: Expr -> String
generateKernelLiteral program = 
    "std::string kernelSource = R\"Fancy0penCL(\n"
    ++ kernelProgram ++ "\n"
    ++ ")Fancy0penCL\";"
    where
        kernelProgram = toOpenCLC program

generateExtractionCode :: Expr -> String
generateExtractionCode (Let exprs body) = case primitive of
    --"filter" -> "__global int *out, __global int *count"
    --"reduce" -> "__global int *gOut, __global int *gCountWhere, __global int *gCountDone, __local int *lOut"
    --"map" -> "__global int *out"
    --"plow" -> "__global int *out"
    --"zipWith" -> "__global int *out"

    "filter" -> fewInts
    "reduce" -> singleInt
    _ -> allInts
    
    where
        FuncCall primitive _ = body
        
        singleInt = "queue.enqueueReadBuffer(d_gOut, CL_TRUE, 0, sizeof(int), h_out.get());"
        allInts = "queue.enqueueReadBuffer(d_out, CL_TRUE, 0, n * sizeof(int), h_out.get());"
        fewInts =
            "queue.enqueueReadBuffer(d_out, CL_TRUE, 0, n * sizeof(int), h_out.get());\n"
            ++ "queue.enqueueReadBuffer(d_count, CL_TRUE, 0, sizeof(int), h_count.get());\n"

generateReturnCode :: Expr -> String
generateReturnCode (Let exprs body) = case primitive of
    "filter" -> fewInts
    "reduce" -> singleInt
    _ -> allInts
    where
        FuncCall primitive _ = body
        
        singleInt = "return h_out[0];"
        allInts = "return std::vector<int>(h_out.get(), h_out.get() + n);"
        fewInts = "return std::vector<int>(h_out.get(), h_out.get() + h_count[0]);"

deviceDiscoveryCode = [r|
    // Query platforms
    std::vector<cl::Platform> platforms;
    cl::Platform::get(&platforms);
    if (platforms.size() == 0) {
        std::cout << "Platform size 0\n";
        throw std::domain_error("Platform size is zero, cannot run.");
        }

    // Get list of devices on default platform and create context
    cl_context_properties properties[] = { CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0};
    cl::Context context(CL_DEVICE_TYPE_GPU, properties);
    std::vector<cl::Device> devices = context.getInfo<CL_CONTEXT_DEVICES>();

    // Create command queue for first device
    cl::CommandQueue queue(context, devices[0], 0, &err);
|]

programBuildCode = [r|
    //Build kernel from source string
    cl::Program::Sources source(1, std::make_pair(kernelSource.c_str(),kernelSource.size()));
    cl::Program program = cl::Program(context, source);

    std::cout << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices[0]);

    try {
        program.build(devices);
    } catch (cl::Error& e) {
        if (e.err() == CL_BUILD_PROGRAM_FAILURE) {
            // Check the build status
            cl_build_status status = program.getBuildInfo<CL_PROGRAM_BUILD_STATUS>(devices[0]);

            // Get the build log
            std::string name     = devices[0].getInfo<CL_DEVICE_NAME>();
            std::string buildlog = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices[0]);
            std::cout << "Build log for " << name << ":" << "\n" << buildlog << "\n";
            std::cout << "Program options: " << program.getBuildInfo<CL_PROGRAM_BUILD_OPTIONS>(devices[0]) << "\n";
        } else {
            throw e;
        }
    }
|]

kernelRunCode = [r|
    // Number of work items in each local work group
    cl::NDRange localSize(workGroupSize);
    // Number of total work items - localSize must be devisor
    cl::NDRange globalSize((int) (ceil(n / (float)workGroupSize) * workGroupSize));

    // Enqueue kernel
    cl::Event event;
    queue.enqueueNDRangeKernel(
        kernel,
        cl::NullRange,
        globalSize,
        localSize,
        NULL,
        &event);

    // Block until kernel completion
    event.wait();
|]

errorPrintCode = [r|
    std::cerr << "ERROR: "<<err.what()<<"("<<err.err()<<")"<< "\n";
    return {};
|]

generateLibrary :: Expr -> String -> String
generateLibrary program name = unlines [
    "#define __CL_ENABLE_EXCEPTIONS\n"
    , "#include <iostream>"
    , "#include <memory>"
    , "#include <cmath>"
    , "#include \"" ++ name ++ ".hpp\""
    , "#include \"utils.hpp\""
    , "#include \"CL/cl.hpp\""
    , generateCppSignature program name ++ " {"
    , "int workGroupSize = 64;"
    , generateKernelLiteral program
    , generateKernelReplacementCode program
    , generateWorkerSizeDiscoveryCode program
    , unlines (map generateBufferDefinition buffers)
    , generateOutputArray program
    , "cl_int err = CL_SUCCESS;"
    , "try {"
    , deviceDiscoveryCode
    , unlines (map generateBufferInitialization buffers)
    , programBuildCode
    , "cl::Kernel kernel(program, \"GPUMonad\", &err);"
    , unlines (zipWith generateKernelBufferArgSet [0..length buffers - 1] buffers)
    , kernelRunCode
    , generateExtractionCode program
    , "} catch (cl::Error err) {"
    , errorPrintCode
    , "}"
    , generateReturnCode program
    , "}"
    ]
    where
        buffers = generateBuffers program

        generateBufferDefinition (Buffer name  _ _) = "cl::Buffer d_" ++ name ++ ";\n"
        generateBufferDefinition (Local _) = ""
        
        -- Count buffers need to be initialized to zero for proper counting
        generateBufferInitialization (Buffer "gCountWhere" flags size) =
            "d_gCountWhere = cl::Buffer(context, " ++ flags ++ ", (" ++ size ++ ") * sizeof(int), h_count.get());\n"
        generateBufferInitialization (Buffer "gCountDone" flags size) =
            "d_gCountDone = cl::Buffer(context, " ++ flags ++ ", (" ++ size ++ ") * sizeof(int), h_count.get());\n"
        generateBufferInitialization (Buffer "count" flags size) =
            "d_count = cl::Buffer(context, " ++ flags ++ ", (" ++ size ++ ") * sizeof(int), h_count.get());\n"
        generateBufferInitialization (Buffer name flags size) =
            "d_" ++ name ++ " = cl::Buffer(context, " ++ flags ++ ", (" ++ size ++ ") * sizeof(int));"
        generateBufferInitialization (Local _) = ""

        generateKernelBufferArgSet i (Buffer name _ size) = "kernel.setArg(" ++ show i ++ ", d_" ++ name ++ ");"
        generateKernelBufferArgSet i (Local size) = "kernel.setArg(" ++ show i ++ ", (" ++ size ++ ") * sizeof(int), nullptr);"

generateHeader :: Expr -> String -> String
generateHeader program name = unlines [
    "#ifndef " ++ (map toUpper name) ++ "_H", 
    "#define " ++ (map toUpper name) ++ "_H",
    "#include <vector>",
    generateCppSignature program name ++ ";",
    "#endif"
    ]
    where

generateAndSaveLibrary :: Expr -> String -> IO()
generateAndSaveLibrary program name = do
    createDirectoryIfMissing False name
    copyFile "utils.cpp" (name ++ [pathSeparator] ++ "utils.cpp")
    copyFile "utils.hpp" (name ++ [pathSeparator] ++ "utils.hpp")
    writeFile (name ++ [pathSeparator] ++ name ++ ".cpp") $ generateLibrary program name
    writeFile (name ++ [pathSeparator] ++ name ++ ".hpp") $ generateHeader program name

generateAllExamples = do
    sequence_ $ map (\x -> generateAndSaveLibrary (doTaal x) ("test" ++ show x)) [6003..6007]

doTest = do
    generateAllExamples
    -- createProcess (shell "make")
    -- createProcess (shell "kernel_test")
    callCommand "make"
    callCommand "./kernel_test"
