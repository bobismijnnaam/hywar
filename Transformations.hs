module Transformations where

import Types
import PrPrClass
import Data.List
import Debug.Trace

-- ====================================================================================

-- inline x (Let ds e) ... WRONG ... !!

inline x e = case e of

        Let ds e        | null dsx      -> simplify $ Let (map (inline x) ds) (inline x e)
                        | otherwise     -> simplify $ Let (map (<<=(x',t)) ds') (e<<=(x',t))
                                        where
                                          isDef x (Def y t) = x==y
                                          (dsx,ds')         = partition (isDef x) ds
                                          Def x' t          = head dsx

        Def y e'                        -> simplify $ Def y (inline x e')
        Lambda y e'                     -> simplify $ Lambda y (inline x $ topLet e')
        App f y                         -> simplify $ App (inline x f) (inline x y)             -- topLet ??
        _                               -> e

inlines [] e = e
inlines (x:xs) e = inlines xs $ inline x e

-- ====================================================================================
-- add Pairs in beta-reduction
-- remove map, zipWith

simplify e = case e of

        Idf x                   -> Idf x
        Num x                   -> Num x
        Bln x                   -> Bln x

        Numeric o e e'          -> Numeric o (simplify e) (simplify e')
        Boolean o e e'          -> Boolean o (simplify e) (simplify e')

        App (Pair f g) x        -> simplify $ Pair (f#x) (g#x)
        App (Triple f g h) x    -> simplify $ Triple (f#x) (g#x) (h#x)

        App (Compose  "." e e') x               -> simplify $ e # (e' # x)
        App (App (Compose  "<." e e') x) y      -> simplify $ e # x # (e' # y)
        App (App (Compose  ".>" e e') x) y      -> simplify $ e' # (e # x) # y

        Compose  o e e'         -> Compose  o (simplify e) (simplify e')

        Pair e0 e1              -> Pair   (simplify e0) (simplify e1)
        Triple e0 e1 e2         -> Triple (simplify e0) (simplify e1) (simplify e2)

        Null                    -> Null                                                         -- Extra
        Cons x xs               -> Cons (simplify x) (simplify xs)                              --

        Lambda x e              -> Lambda x (simplify e)                                        -- ?? whnf ??

        App (Idf "zip") (Pair Null ys)                  -> Null                                         -- Extra
        App (Idf "zip") (Pair xs Null)                  -> Null                                         -- Extra
        App (Idf "zip") (Pair (Cons x xs) (Cons y ys))  -> simplify $ Cons (Pair x y) $ Idf "zip" # Pair xs ys
        App (Idf "zip") x               | x'/=x         -> simplify $ Idf "zip" # x'
                                        | otherwise     -> Idf "zip" # x
                                                        where
                                                          x' = simplify x

        App (Idf "zip3") (Triple Null ys zs)            -> Null                                         -- Extra
        App (Idf "zip3") (Triple xs Null zs)            -> Null                                         -- Extra
        App (Idf "zip3") (Triple xs ys Null)            -> Null                                         -- Extra
        App (Idf "zip3") (Triple (Cons x xs) (Cons y ys) (Cons z zs))
                                                        -> simplify $ Cons (Triple x y z) $ (Idf "zip3")#(Triple xs ys zs)
        App (Idf "zip3") x              | x'/=x         -> simplify $ Idf "zip3" # x'
                                        | otherwise     -> Idf "zip3" # x
                                                        where
                                                          x' = simplify x

        App (Idf "fst") (Pair x y)                      -> simplify x
        App (Idf "fst") x               | x'/=x         -> simplify $ Idf "fst" # x'
                                        | otherwise     -> Idf "fst" # x
                                                        where
                                                          x' = simplify x
        App (Idf "snd") (Pair x y)                      -> simplify y
        App (Idf "snd") x               | x'/=x         -> simplify $ Idf "snd" # x'
                                        | otherwise     -> Idf "snd" # x
                                                        where
                                                          x' = simplify x

        App (Idf "fst3") (Triple x y z)                 -> simplify x
        App (Idf "fst3") x              | x'/=x         -> simplify $ Idf "fst3" # x'
                                        | otherwise     -> Idf "fst3" # x
                                                        where
                                                          x' = simplify x
        App (Idf "snd3") (Triple x y z)                 -> simplify y
        App (Idf "snd3") x              | x'/=x         -> simplify $ Idf "snd3" # x'
                                        | otherwise     -> Idf "snd3" # x
                                                        where
                                                          x' = simplify x
        App (Idf "thd3") (Triple x y z)                 -> simplify z
        App (Idf "thd3") x              | x'/=x         -> simplify $ Idf "thd3" # x'
                                        | otherwise     -> Idf "thd3" # x
                                                        where
                                                          x' = simplify x



        App f a                 -> case f' of
                                        Lambda x t      -> simplify $ t <<= (x,a') 
                                        _               | a == a'       -> App f' a
                                                        | otherwise     -> simplify $ App f' a'
                                where
                                  f' = simplify f
                                  a' = simplify a

        Def x t                 -> Def x $ simplify t

        Let []   t              -> simplify t
        Let defs t              -> Let (map simplify defs) (simplify t)

        Sel e' i                -> case e' of
                                        Let defs t                              -> simplify $ Let defs (t!i)
                                        -- App (App (Idf "map") f) xs           -> simplify $ App f (xs!i)
                                        -- App (App (App (Idf "zipWith") f) xs) ys      -> simplify $ App (App f (xs!i)) (ys!i)

                                        App (Idf "tail") xs                     -> simplify $ xs!(calc $ Numeric "+" i (Num 1))
                                        App (Idf "init") xs                     -> simplify $ xs!i

                                        App (Idf "zip") xy                      -> simplify $ Pair ((Idf "fst"#xy)!i) ((Idf "snd"#xy)!i)
                                        App (Idf "zip3") xyz                    -> simplify $ Triple ((Idf "fst3"#xyz)!i) ((Idf "snd3"#xyz)!i) ((Idf "thd3"#xyz)!i)

                                        App (App (Idf "take") n) xs             -> simplify $ xs!(Numeric "+" i n)
                                        App (App (Idf "drop") n) xs             -> simplify $ xs!i

                                        Sel (App (Idf "transpose") m) j         -> simplify $ (m!i)!j

                                        Null                                    -> error "simplify: Select from empty list"
                                        Cons x xs       | eqNum i 0             -> x
                                                        | otherwise             -> simplify $ Sel xs $ calc $ Numeric "-" i (Num 1)
                                                                                where
                                                                                  eqNum i n = simplify i == Num n

                                        otherwise                               -> (simplify e')!(simplify i)

        IfE e0 e1 e2            -> IfE (simplify e0) (simplify e1) (simplify e2)

        _                       -> error ("simplify -- unexpected expression: " ++ show e)

-- ====================================================================================
-- Lambda lifting

lamLift e = case e of

        Idf "concat"                    -> Lambda (Idf "xss") $ (Idf "concat")#(Idf "xss")

        App (Idf "replicate") n         -> Lambda (Idf "a") $ (Idf "replicate")#n#(Idf "a")
        Idf "replicate"                 -> Lambda (Idf "n" ) $ Lambda (Idf "a") $ (Idf "replicate")#(Idf "n")#(Idf "a")

        App (Idf "split") m             -> Lambda (Idf "a") $ (Idf "split")#m#(Idf "a")
        Idf "split"                     -> Lambda (Idf "n" ) $ Lambda (Idf "a") $ (Idf "split")#(Idf "n")#(Idf "a")

        App (Idf "map") f               -> Lambda (Idf "xs") $ (Idf "map")#f#(Idf "xs")
        Idf "map"                       -> Lambda (Idf "f" ) $ Lambda (Idf "xs") $ (Idf "map")#(Idf "f")#(Idf "xs")

        App (App (Idf "zipWith") f) xs  -> Lambda (Idf "ys") $ (Idf "zipWith")#f#xs#(Idf "ys")
        App (Idf "zipWith") f           -> Lambda (Idf "xs") $ Lambda (Idf "ys") $ (Idf "zipWith")#f#(Idf "xs")#(Idf "ys")
        Idf "zipWith"                   -> Lambda (Idf "f" ) $ Lambda (Idf "xs") $ Lambda (Idf "ys") $ (Idf "zipWith")#(Idf "f")#(Idf "xs")#(Idf "ys")

        App (App (Idf "foldl") f) a     -> Lambda (Idf "xs") $ (Idf "foldl")#f#a#(Idf "xs")
        App (Idf "foldl") f             -> Lambda (Idf "a" ) $ Lambda (Idf "xs") $ (Idf "foldl")#f#(Idf "a")#(Idf "xs")
        Idf "foldl"                     -> Lambda (Idf "f" ) $ Lambda (Idf "a") $ Lambda (Idf "xs") $ (Idf "foldl")#(Idf "f")#(Idf "a")#(Idf "xs")

        Pair e0 e1                      -> Pair (lamLift e0) (lamLift e1)

        Triple e0 e1 e2                 -> Triple (lamLift e0) (lamLift e1) (lamLift e2)

        Cons e0 e1                      -> Cons (lamLift e0) (lamLift e1)

        IfE e0 e1 e2                    -> IfE e0 (lamLift e1) (lamLift e2)

        Def x e0                        -> Def x (lamLift e0)

        Let ds e0                       -> Let (map lamLift ds) (lamLift e0)

        Lambda x e0                     -> Lambda x (lamLift e0)

        _                               -> e

-- ====================================================================================
-- Lift let-bindings to top-level

topLet e = case e of
        Idf e0          -> Let [] e
        Num e0          -> Let [] e
        Bln e0          -> Let [] e
        Null            -> Let [] Null
        Empty           -> Let [] Empty

        Numeric o e0 e1 -> Let (ds0++ds1) $ Numeric o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Boolean o e0 e1 -> Let (ds0++ds1) $ Boolean o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Compose o e0 e1 -> Let (ds0++ds1) $ Compose o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Pair e0 e1      -> Let (ds0++ds1) $ Pair e0' e1'                where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Triple e0 e1 e2 -> Let (ds0++ds1++ds2) $ Triple e0' e1' e2'     where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1
                                                                                Let ds2 e2'     = topLet e2

        Cons e0 e1      -> Let (ds0++ds1) $ Cons e0' e1'                where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Sel e0 i        -> Let ds $ Sel e0' i                           where   Let ds e0'      = topLet e0

        IfE e0 e1 e2    -> Let ds0 $ IfE e0' (Let ds1 e1')
                                             (Let ds2 e2')              where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1
                                                                                Let ds2 e2'     = topLet e2

        App f e0        -> Let (dsf++ds0) $ App f' e0'                  where   Let dsf f'      = topLet f
                                                                                Let ds0 e0'     = topLet e0

        Def x e0        -> Let (ds++[d]) Empty                          where   Let ds e0'      = topLet e0
                                                                                d               = Def x e0'

        Let ds e0       -> Let (ds'++ds0) e0'                           where   ds'             = concat [ dsi | Let dsi Empty <- map topLet ds ]
                                                                                Let ds0 e0'     = topLet e0

        Lambda x es     -> Let [] $ Lambda x $ topLet es

        _               -> error ("topLet: " ++ show e)

-- ====================================================================================
-- Translation from Haskell to C

rename (x,y) e = case e of              -- TOO RADICAL

        Def z e'        | z == x        -> Def y $ rename (x,y) e'
                        | otherwise     -> Def z $ rename (x,y) e'

        Idf z           | x == Idf z    -> y
                        | otherwise     -> Idf z

        Num n                           -> Num n
        Bln n                           -> Bln n

        Numeric o e0 e1                 -> Numeric o (rename (x,y) e0) (rename (x,y) e1)
        Boolean o e0 e1                 -> Boolean o (rename (x,y) e0) (rename (x,y) e1)
        Compose o e0 e1                 -> Compose o (rename (x,y) e0) (rename (x,y) e1)

        Pair e0 e1                      -> Pair (rename (x,y) e0) (rename (x,y) e1)
        Triple e0 e1 e2                 -> Triple (rename (x,y) e0) (rename (x,y) e1) (rename (x,y) e2)

        Null                            -> Null
        Cons t ts                       -> Cons (rename (x,y) t) (rename (x,y) ts)

        Sel e0 i                        -> Sel (rename (x,y) e0) (rename (x,y) i)
        IfE t e0 e1                     -> IfE (rename (x,y) t) (rename (x,y) e0) (rename (x,y) e1)
        App e0 e1                       -> App (rename (x,y) e0) (rename (x,y) e1)

        Lambda z t      | z == x        -> Lambda z t
                        | otherwise     -> Lambda z (rename (x,y) t)

        Func [] stmnts e0               -> Func [] stmnts (rename (x,y) e0)             -- <== rename statments

        Func (z:zs) stmnts e0 | z == x    -> Func (z:zs) stmnts e0
                              | otherwise -> Func (z:zs) stmnts e0'
                                          where
                                            Func _ _ e0' = rename (x,y) (Func zs stmnts e0)             -- <== rename statments

{-
        Func z stmnts e0 | z == x       -> Func z stmnts e0
                         | otherwise    -> Func z stmnts (rename (x,y) e0)              -- <== rename statments
-}

        Let ds t                        -> Let (map (rename (x,y)) ds) (rename (x,y) t)

        _                               -> error ("rename: " ++ show e)

-- ====================================================================================
-- calculate expressions

calc e = case e of

        Idf x                                           -> Idf x
        Num x                                           -> Num x
        Bln x                                           -> Bln x

        -- ======================================================================

        Numeric "+" x (Num 0)                           -> calc x
        Numeric "+" (Num 0) x                           -> calc x

        Numeric "-" x (Num 0)                           -> calc x
        Numeric "-" (Num 0) (Num x)                     -> Num (-x)

        Numeric "*" x (Num 0)                           -> Num 0
        Numeric "*" (Num 0) x                           -> Num 0
        Numeric "*" x (Num 1)                           -> calc x
        Numeric "*" (Num 1) x                           -> calc x

        Numeric "/" x (Num 1)                           -> calc x

        Numeric "^" (Num 0) x                           -> Num 0
        Numeric "^" (Num 1) x                           -> Num 1
        Numeric "^" x (Num 0)                           -> Num 1
        Numeric "^" x (Num 1)                           -> calc x

        App (Idf "neg") (Num x)                         -> Num (-x)
        App (Idf "neg") x                               -> calc $ Numeric "-" (Num 0) x

        Numeric o (Num x) (Num y)                       -> Num (n_oper o x y)

        -- ======================================================================

        -- a*(x+y) -> a*x + a*y
        Numeric ("*") a (Numeric "+" x y)       -> calc $ Numeric "+" (Numeric "*" a' x') (Numeric "*" a' y')
                                                        where
                                                          x' = calc x
                                                          y' = calc y
                                                          a' = calc a

        -- (x+y)*a -> x*a + y*a
        Numeric ("*") (Numeric "+" x y) a       -> calc $ Numeric "+" (Numeric "*" x' a') (Numeric "*" y' a')
                                                        where
                                                          x' = calc x
                                                          y' = calc y
                                                          a' = calc a

        -- (x*y)^n -> x^n * y^n
        Numeric ("^") (Numeric "*" x y) n               -> calc $ Numeric "*" (Numeric "^" x' n') (Numeric "^" y' n')
                                                        where
                                                          x' = calc x
                                                          y' = calc y
                                                          n' = calc n

        Numeric o x y           | x'/=x || y'/=y        -> calc $ Numeric o x' y'
                                | otherwise             -> Numeric o x y
                                                        where
                                                          x' = calc x
                                                          y' = calc y

{-
        N ^ (N + t0 t1) (Num n) -> N * (N + t0 t1) (N ^ (N + t0 t1) (Num (n-1)))

        N + 

        Numeric "+" (Numeric "*" a0 b0) (Numeric "*" a1 b1) | b0==b1       -> Numeric "*" (Numeric "+" a0 a1) b0

        Numeric o e0 e1         | (e0',e1') == (e0,e1)  -> Numeric o e0 e1
                                | otherwise             -> calc $ Numeric o e0' e1'
                                                        where
                                                          (e0',e1') = (calc e0, calc e1)
-}

        -- ======================================================================

        Boolean o (Num x) (Num y)                       -> Bln (r_oper o x y)

        Boolean "&&" x (Bln True)                       -> calc x
        Boolean "&&" (Bln True) x                       -> calc x
        Boolean "&&" x (Bln False)                      -> Bln False
        Boolean "&&" (Bln False) x                      -> Bln False

        Boolean "||" x (Bln True)                       -> Bln True
        Boolean "||" (Bln True) x                       -> Bln True
        Boolean "||" x (Bln False)                      -> calc x
        Boolean "||" (Bln False) x                      -> calc x

        Boolean "=>" x (Bln True)                       -> Bln True
        Boolean "=>" (Bln True) x                       -> calc x
        Boolean "=>" x (Bln False)                      -> calc x
        Boolean "=>" (Bln False) x                      -> Bln True

        Boolean "##" (Bln False) (Bln False)            -> Bln True
        Boolean "##" x (Bln True)                       -> calc x
        Boolean "##" (Bln True) x                       -> calc x

        Boolean o (Bln x) (Bln y)                       -> Bln (b_oper o x y)

        Boolean o e0 e1         | (e0',e1') == (e0,e1)  -> Boolean o e0 e1
                                | otherwise             -> calc $ Boolean o e0' e1'
                                                        where
                                                          (e0',e1') = (calc e0, calc e1)

        -- ======================================================================

        IfE (Bln True)  e0 e1                           -> calc e0
        IfE (Bln False) e0 e1                           -> calc e1
        IfE t e0 e1             | t' == t               -> IfE t (calc e0) (calc e1)
                                | otherwise             -> calc $ IfE t' e0 e1
                                                        where
                                                          t'        = calc t

        Null                                            -> Null
        Cons x xs                                       -> Cons (calc x) (calc xs)

        -- ======================================================================

        App (Idf "not") (Bln True)                      -> Bln False
        App (Idf "not") (Bln False)                     -> Bln True
        App (Idf "not") x       | x' == x               -> App (Idf "not") x
                                | otherwise             -> calc $ App (Idf "not") x'
                                                        where
                                                          x' = calc x

        _                                               -> e

        -- ======================================================================

--App (App map g) (App (App map h) (Cons 1 (Cons 2 (Cons 3 Null))))

expandHOF e = case e of

        Def x y                                                 -> Def x $ expandHOF y

        -- ======================================================================

        App (Idf "head") Null                                   -> error "expandHOF: head applied to Null"
        App (Idf "head") (Cons x xs)                            -> x
        App (Idf "head") xs             | xs'/=xs               -> expandHOF $ Idf "head" # xs'
                                        | otherwise             -> Idf "head" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "tail") Null                                   -> error "expandHOF: tail applied to Null"
        App (Idf "tail") (Cons x xs)                            -> xs
        App (Idf "tail") xs             | xs'/=xs               -> expandHOF $ Idf "tail" # xs'
                                        | otherwise             -> Idf "tail" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "init") Null                                   -> error "expandHOF: init applied to Null"
        App (Idf "init") (Cons x Null)                          -> Null
        App (Idf "init") (Cons x xs)                            -> Cons x $ expandHOF $ Idf "init" # xs
        App (Idf "init") xs             | xs'/=xs               -> expandHOF $ Idf "init" # xs'
                                        | otherwise             -> Idf "init" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "last") Null                                   -> error "expandHOF: last applied to Null"
        App (Idf "last") (Cons x Null)                          -> x
        App (Idf "last") (Cons x xs)                            -> expandHOF $ Idf "last" # xs
        App (Idf "last") xs             | xs'/=xs               -> expandHOF $ Idf "last" # xs'
                                        | otherwise             -> Idf "last" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "take") (Num 0)) xs                       -> Null
        App (App (Idf "take") (Num n)) Null                     -> Null
        App (App (Idf "take") (Num n)) (Cons x xs)              -> Cons x $ expandHOF $ Idf "take" # Num (n-1) # xs
        App (App (Idf "take") (Num n)) xs       | xs'/=xs       -> expandHOF $ Idf "take" # Num n # xs'
                                                | otherwise     -> Idf "take" # Num n # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "drop") (Num 0)) xs                       -> xs
        App (App (Idf "drop") (Num n)) Null                     -> Null
        App (App (Idf "drop") (Num n)) (Cons x xs)              -> expandHOF $ Idf "drop" # Num (n-1) # xs
        App (App (Idf "drop") (Num n)) xs       | xs'/=xs       -> expandHOF $ Idf "drop" # Num n # xs'
                                                | otherwise     -> Idf "drop" # Num n # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "split") (Num n)) Null                    -> Null
        App (App (Idf "split") (Num n)) xs                      -> Cons (expandHOF $ Idf "take" # Num n # xs')
                                                                        (expandHOF $ Idf "split" # Num n # (expandHOF $ Idf "drop" # Num n # xs'))
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "replicate") (Num 0)) x                   -> Null
        App (App (Idf "replicate") (Num n)) x                   -> Cons x (expandHOF $ Idf "replicate" # Num (n-1) # x)

        App (Idf "zip") (Pair Null Null)                        -> Null
        App (Idf "zip") (Pair (Cons x xs) (Cons y ys))          -> Cons (Pair x y) $ expandHOF $ Idf "zip" # Pair xs ys
        App (Idf "zip") p               | p'/=p                 -> expandHOF $ Idf "zip" # p'
                                        | otherwise             -> Idf "zip" # p
                                                                where
                                                                  p' = expandHOF p

        App (Idf "unzip") Null                                  -> Pair Null Null
        App (Idf "unzip") (Cons (Pair x y) xys)                 -> Pair (Cons x xs) (Cons y ys)
                                                                where
                                                                  Pair xs ys = expandHOF $ Idf "unzip" # xys
        App (Idf "unzip") xs            | xs'/=xs               -> expandHOF $ Idf "unzip" # xs'
                                        | otherwise             -> Idf "unzip" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "zip3") (Triple Null Null Null)                        -> Null
        App (Idf "zip3") (Triple (Cons x xs) (Cons y ys) (Cons z zs))   -> Cons (Triple x y z) $ expandHOF $ Idf "zip3" # Triple xs ys zs
        App (Idf "zip3") xs             | xs'/=xs               -> expandHOF $ Idf "zip3" # xs'
                                        | otherwise             -> Idf "zip3" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "unzip3") Null                                 -> Triple Null Null Null
        App (Idf "unzip3") (Cons (Triple x y z) xyzs)           -> Triple (Cons x xs) (Cons y ys) (Cons z zs)
                                                                where
                                                                  Triple xs ys zs = expandHOF $ Idf "unzip3" # xyzs
        App (Idf "unzip3") xs           | xs'/=xs               -> expandHOF $ Idf "unzip3" # xs'
                                        | otherwise             -> Idf "unzip3" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "transpose") (Cons Null xss)                   -> Null
        App (Idf "transpose") xss                               -> Cons (expandHOF $ Idf "map" # Idf "head" # xss) $ expandHOF $ Idf "transpose" # (expandHOF $ Idf "map" # Idf "tail" # xss)

        -- ======================================================================

        App (App (Idf "map") f) Null                            -> Null
        App (App (Idf "map") f) (Cons x xs)                     -> Cons (expandHOF $ f#x) $ expandHOF $ Idf "map" # f # xs'
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "map") f) xs              | xs'/=xs       -> expandHOF $ (Idf "map") # f # xs'
                                                | otherwise     -> Idf "map" # expandHOF f # xs
                                                                where
                                                                  xs' = expandHOF xs

        -- ======================================================================

        App (App (App (Idf "itn") f) a) (Num 0) -> expandHOF $ a
        App (App (App (Idf "itn") f) a) (Num n) -> expandHOF $ Idf "itn" # f # (f#a) # Num (n-1)

        App (App (App (Idf "itn") f) a) n       | a'/=a || n'/=n        -> expandHOF $ Idf "itn" # f # a' # n'
                                                | otherwise             -> Idf "itn" # expandHOF f # a # n
                                                                        where
                                                                          a' = expandHOF a
                                                                          n' = expandHOF n

        -- ======================================================================

        App (App (App (Idf "itnscan") f) a) (Num 0)                     -> Cons a Null
        App (App (App (Idf "itnscan") f) a) (Num n)                     -> topLet $ Let [Def (Idf ax) (f#a)]
                                                                                (Cons a $ expandHOF $ Idf "itnscan" # f # Idf ax # Num (n-1))
                                                                        where
                                                                          ax = toString a ++ "`"

        App (App (App (Idf "itnscan") f) a) n   | n'/=n                 -> expandHOF $ Idf "itnscan" # f # a # n'
                                                | otherwise             -> Idf "itnscan" # expandHOF f # a # n
                                                                        where
                                                                          n' = expandHOF n

        -- ======================================================================

        App (App (App (Idf "zipWith") f) Null) xs       -> Null
        App (App (App (Idf "zipWith") f) xs) Null       -> Null
        App (App (App (Idf "zipWith") f) (Cons x xs)) (Cons y ys)
                                                        -> Cons ((f#x#y)) $ expandHOF $ (Idf "zipWith")#f#xs#ys

        App (App (App (Idf "zipWith") f) xs) ys | xs'/=xs || ys'/=ys    -> expandHOF $ (Idf "zipWith") # f # xs' # ys'
                                                | otherwise             -> (Idf "zipWith") # expandHOF f # xs # ys
                                                                        where
                                                                          xs' = expandHOF xs
                                                                          ys' = expandHOF ys

        -- ======================================================================

        App (App (App (Idf "foldl") f) a) Null          -> a
        App (App (App (Idf "foldl") f) a) (Cons x xs)   -> expandHOF $ (Idf "foldl") # f # (expandHOF $ f#a#x) # xs'
                                                                        where
                                                                          xs' = expandHOF xs

        App (App (App (Idf "foldl") f) a) xs    | xs'/=xs               -> expandHOF $ (Idf "foldl") # f # a # xs'
                                                | otherwise             -> (Idf "foldl") # expandHOF f # a # xs
                                                                        where
                                                                          xs' = expandHOF xs

        -- ======================================================================

        Pair x y                                        -> Pair (expandHOF x) (expandHOF y)
        Triple x y z                                    -> Triple (expandHOF x) (expandHOF y) (expandHOF z)

        Cons x xs                                       -> Cons (expandHOF x) (expandHOF xs)

        IfE e e1 e2                                     -> IfE (expandHOF e) (expandHOF e1) (expandHOF e2)

        _                                               -> e

-- ====================================================================================


fI       = Idf "f"
gI       = Idf "g"
mI       = Idf "m"
splitI   = Idf "split"
concatI  = Idf "concat"
zipI     = Idf "zip"
mapI     = Idf "map"
foldlI   = Idf "foldl"
zipWithI = Idf "zipWith"


law (p,i) e = case (p,i) of

        (_,0)   -> e

        -- ======================================================================
        -- LHS: map f xs

        ("m",i)         -> case e of
                                App (App (Idf "map") f) xs

                                 -> case i of
                                        -- RHS: concat $ map (map f) $ split m xs
                                     10 -> concatI # (mapI # (mapI#f) # xssm)
                                     12 -> concatI # (mapI # (mapI#f) # xss2)
                                 where
                                   xssm = splitI # mI # xs
                                   xss2 = Idf "split" # Num 2 # xs

        -- ======================================================================
        -- LHS: foldl f a xs

        ("f",i)         -> case e of
                                App (App (App (Idf "foldl") f) a) xs

                                 -> case i of
                                     10 -> foldlI # (foldlI#f) # a # xssm                               -- foldl f a xs ==>>  foldl (foldl f) a (split m xs)
                                     12 -> foldlI # (foldlI#f) # a # xss2
                                     13 -> foldlI # (foldlI#f) # a # xss3
                                     14 -> foldlI # (foldlI#f) # a # xss4

                                     20 -> foldlI # f # a # (mapI # (foldlI#f#a) # xssm)                --              ==>>  foldl f a $ map (foldl f a) (split m xs)
                                     22 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss2)
                                     23 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss3)
                                     24 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss4)

                                     30 -> foldlI # f # a # (foldlI# (zipWithI#f) # asm # xssm)         --              ==>>  foldl f a $ foldl (zipWith f) (replicate m a) (split m xs)
                                     32 -> foldlI # f # a # (foldlI# (zipWithI#f) # as2 # xss2)
                                     33 -> foldlI # f # a # (foldlI# (zipWithI#f) # as3 # xss3)
                                     34 -> foldlI # f # a # (foldlI# (zipWithI#f) # as4 # xss4)

                                 where
                                   xssm = Idf "split" # mI # xs
                                   xss2 = Idf "split" # Num 2 # xs
                                   xss3 = Idf "split" # Num 3 # xs
                                   xss4 = Idf "split" # Num 4 # xs

                                   asm  = Idf "replicate" # mI # a
                                   as2  = Idf "replicate" # Num 2 # a
                                   as3  = Idf "replicate" # Num 3 # a
                                   as4  = Idf "replicate" # Num 4 # a

                                _ -> error ("law: " ++ (show $ toString e))

        -- ======================================================================
        -- LHS: map f (map g xs)

        ("mm",1)        -> case e of
                                App (App (Idf "map") f) (App (App (Idf "map") g) xs)
                                        -- RHS: map (f.g) xs
                                        -> mapI # (Compose "." f g) # xs

        -- ======================================================================
        -- LHS: zipWith o (map f xs) (map g ys)

        ("zWmm",i)      -> case e of
                                App (App (App (Idf "zipWith") o) (App (App (Idf "map") f) xs)) (App (App (Idf "map") g) ys)             -- assumption: xs and ys equally long? Not necessarily ...

                                  -> case i of
                                        -- RHS: zipWith (\v y -> o v (g y)) (map f xs) ys
                                      1 -> zipWithI # (Lambda v $ Lambda y $ o#v#(g#y)) # (mapI#f#xs) # ys

                                        -- RHS: zipWith (\x v -> o (f x) v) xs (map g ys)
                                      2 -> zipWithI # (Lambda x $ Lambda v $ o#(f#x)#v) # xs # (mapI#g#ys)

                                        -- RHS: map (\x -> o (f x) (g x)) xs                                            -- assumption: xs == ys
                                      3 -> mapI # (Lambda x $ o#(f#x)#(g#x)) # xs
                        where
                          (v,x,y) = (Idf"v", Idf "x", Idf "y")

        -- ======================================================================
        -- LHS: foldl f a $ map g xs

        ("fm",i)        -> case e of
                                App (App (App (Idf "foldl") f) a) (App (App (Idf "map") g) xs)

                                 -> case i of
                                        -- RHS: foldl (f <. g x) a xs
                                      1 -> foldlI # (Compose "<." f g) # a # xs

                                      2 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Idf "x") $
                                                                f#(Idf "a")#(g#(Idf "x"))
                                                  ) # a # xs

                                        -- RHS: foldl (\a x -> let y = g x in f a y) a xs
                                      3 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Idf "x") $
                                                                Let [Def (Idf "y") (g#(Idf"x"))]
                                                                    f#(Idf "a")#(Idf "y")
                                                  ) # a # xs

        -- ======================================================================
        -- LHS: foldl f a $ zipWith g xs ys

        ("fzW",i)       -> case e of
                                App (App (App (Idf "foldl") f) a) (App (App (App (Idf "zipWith") g) xs) ys)

                                 -> case i of
                                        -- RHS: foldl ((f<.g) a (x,y)         f a (g x y)) a (zip xs ys)
                                      1 -> foldlI # (Compose "<." f g) # a # (zipI#xs#ys)

                                        -- RHS: foldl (\a (x,y) -> f a (g x y)) a (zip xs ys)
                                      2 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Pair (Idf "x") (Idf "y")) $
                                                                f#(Idf "a")#(g#(Idf "x")#(Idf "y"))
                                                  ) # a # (zipI#xs#ys)

                                        -- RHS: foldl (\a (x,y) -> let u = g x y in f a u) a (zip xs ys)
                                      3 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Pair (Idf "x") (Idf "y")) $
                                                                Let [Def (Idf "u") (g#(Idf "x")#(Idf "y"))]
                                                                    f#(Idf "a")#(Idf "u")
                                                  ) # a # (zipI#xs#ys)

        -- ======================================================================



