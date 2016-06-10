{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}
module Testing where

import Text.RawString.QQ

import Data.List
import Types
import PrPrClass
import Parse

-- ===============================================================================================

ex i = case i of

        1       -> "map f xs"
        11      -> "map f (map g xs)"
        12      -> "map (map (map f)) xsss"
        15      -> "let t = foldl f a xs in t"

        160     -> "let f = (*2) in foldl (+) 0 (map f xs)"                                             -- C

        170     -> "foldl f a xs + foldl g b ys"

        210     -> "foldl (+) a xs"
        -- ------------------------------------------------------------------------------------
        220     -> unlines [ "let"
                           , "  xss = split m xs,"
                           , "  isum = \\a xs -> foldl (+) a xs"
                           , "in"
                           , "  foldl isum 0 xss"
                           ]
        -- ------------------------------------------------------------------------------------
        230     -> unlines [ "let"
                           , "  xss = split m xs,"
                           , "  dsum = foldl (+) 0"
                           , "in"
                           , "  foldl (+) 0 (map dsum xss)"
                           ]
        -- ------------------------------------------------------------------------------------
        240     -> unlines [ "let"
                           , "  xss = split m xs,"
                           , "  zeroes = replicate m 0,"
                           , "  psum = zipWith (+)"
                           , "in"
                           , "  foldl (+) 0 (foldl psum zeroes xss)"
                           ]

        250     -> " foldl (foldl (+)) 0 xss"
        251     -> " foldl (+) 0 (map (foldl (+) 0) xss)"
        252     -> " foldl (+) 0 (foldl (zipWith (+)) zs xss)"


        -- ====================================================================================
        -- Dotproduct, matrix multiplication

        410     -> "foldl (+) 0 (zipWith (*) xs ys)"
        412     -> "foldl (+) 0 (map (*2) xs)"

        -- ====================================================================================
        420     -> unlines [ "let "     -- NOTE: uncurried dotprod !!
                           , "  dpr = (foldl (\\c (a,b) -> c+a*b) 0) . zip"
                           , "in "
                           , "  map (\\as -> map (\\bs -> dpr (as,bs)) (transpose qss)) pss"
                           ]

        -- ====================================================================================

        4001    -> unlines [ "let"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys),"
                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss,"
                           , "  mxmA = \\xss yss -> map (mxv xss) (transpose yss)"
                           , "in"
                           , "  mxmA xss yss"
                           ]

        4002    -> unlines [ "let"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys),"
                           , "  vxm = \\xs yss -> map (vxv xs) (transpose yss),"
                           , "  mxmB = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxmB xss yss"
                           ]

        4003    -> unlines [ "let"
                           , "  vxv = \\xs  ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys)),"

                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss"

                           , "in"
                           , "  mxv xss ys"
                           ]


        4004    -> unlines [ "let"      -- mxm: variant 1
                           , "  vxv = \\xs  ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys)),"
                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss,"
                           , "  mxm = \\xss yss -> transpose (map (mxv xss) (transpose yss))"
                           , "in"
                           , "  mxm xss yss"
                           ]

        4005    -> unlines [ "let"      -- mxm: variant 2
                           , "  vxv = \\xs  ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys)),"
                           , "  vxm = \\xs yss -> map (vxv xs) (transpose yss),"
                           , "  mxm = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxm xss yss"
                           ]
        
        -- =================================================================================
        -- Testing the compiler and finding the boundaries of the syntax

        5000 ->  [r|
let 
    muller = \x y -> x * y,
    muls = zipWith muller xs ys
in
    fold (+) muls
|]

        5001    ->  [r|
let 
    a = let
            b = 3,
            c = 4
        in
            (b + c) * 4,
    d = 3
in
    mod a d
|]

        5002 -> [r|
let
    xs :: [Int]
    ys :: [Int]
in
    fold (+) xs ys
|]

        5003 -> [r|
let
    xs = [1, 2, 3],
    yss = [[1, 2], [3, 4]],
    zsss = [[[1, 2], [3, 4]], [[5, 6], [7, 8]], [[9, 10], [11, 12]]]
in
    main a b c
|]

        -- =================================================================================
        -- Actual example programs
        -- Later on there should be a "kernel-let" à la harlan
        -- primitives: filter/reduce/map/plow/zipWith
        -- Arbitrary constraints:
        --      - List can only be global inputs
        --      - List cannot be nested
        --      - Global inputs can also be normal ints
        --      - Functions can only have ints as input and int as output
        --      - Top level expression has to be one of the primitives
        --      - No lets except for the top-level let :-(

        6003 -> [r|
let
    ps = [Int],
    isEven = \x -> (mod x 2) == 0
in
    filter isEven ps
|]

        6004 -> [r|
let
    xs = [Int],
    myMax = \x y -> if x > y then x else y
in
    reduce myMax xs
|]
                    
        6005 -> [r|
let
    xs = [Int],
    add = Int,
    sum = \x -> x + add + 5
in
    map sum xs
|]

-- plow is a reduce-like name for scan?
        6006 -> [r|
let
    xs = [Int],
    a = 5
in
    plow (\x z -> x + z) xs
|]

        6007 -> [r|
let
    xs = [Int],
    ys = [Int],
    myMin = \x y -> if x > y then y else x
in
    zipWith myMin xs ys
|]
