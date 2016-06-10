{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module PrPrClass where

import Types

-- =================================================================================================
-- == Testing ======================================================================================
-- =================================================================================================

space n = map ((replicate n ' ') ++)

-- ============================================================================================


addSpace n = map ((replicate n ' ') ++)

addListNotation []                 =   [["["]]

addListNotation ([]:strss)         =   ["["]
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addListNotation ((str:strs):strss) =   (("["++str):strs)
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addEndBrack [strs]       = [ strs ++ ["]"] ]
addEndBrack (strs:strss) = strs : addEndBrack strss


instance PrPr ParseTree where

  toStrings tree = case tree of
    PLeaf t                 -> ["PLeaf " ++ show t]
    PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 2 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)

    PError tr rule nt str k -> [ "==========="
                               , "Parse Error"
                               , "==========="
                               , "Recognized:"
                               , "-----------"
                               ]
                               ++ toStrings tr ++
                               [ "-----------"
                               , "Still to go:   " ++ show rule
                               , "Expected:      " ++ show nt
                               , "Found:         " ++ str
                               , "At position:   " ++ show k
                               , "==========="
                               ]

  prpr  t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t

-- ============================================================================================

instance PrPr Expr where

  toStrings e = case e of
    Idf str         -> ["Idf "++str]
    Num n           -> ["Num "++show n]
    Bln b           -> ["Bln "++show b]
    Numeric o e0 e1 -> ["Numeric "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
    Boolean o e0 e1 -> ["Boolean "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
    Compose o e0 e1 -> ["Compose "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
    Pair e0 e1      -> ["Pair"]        ++ space 2 (toStrings e0 ++ toStrings e1)
    Triple e0 e1 e2 -> ["Triple"]      ++ space 2 (toStrings e0 ++ toStrings e1 ++ toStrings e2)
    Null            -> ["Null"]
    Cons e0 es      -> ["Cons "]       ++ space 2 (toStrings e0 ++ toStrings es)
    Lambda x e0     -> ["\\"           ++ head (toStrings x) ++ " -> "] ++ space 2 (toStrings e0)
    App e0 e1       -> ["App"]         ++ space 2 (toStrings e0 ++ toStrings e1)
    Def x e'        -> ["Def"]         ++ space 2 (toStrings x ++ toStrings e')
    Let [] e'       -> ["Let","in"]    ++ space 2 (toStrings e')
    Let ds e'       -> ["Let"]         ++ space 2 (concat $ map toStrings ds) ++ ["in"] ++ (space 2 $ toStrings e')
    Sel e0 i        -> ["Sel"]         ++ space 2 (toStrings e0 ++ toStrings i)
    IfE e0 e1 e2    -> ["If"]          ++ space 2 (toStrings e0) ++ ["then"] ++ space 2 (toStrings e1) ++ ["else"] ++ space 2 (toStrings e2)

    Empty           -> ["Empty"]

        -- ====================================================================================

  prpr  t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t


