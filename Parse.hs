{-# LANGUAGE FlexibleInstances #-}

module Parse where

import Types
import Data.List

-- =================================================================================================
-- == Basic definitions ============================================================================
-- =================================================================================================

syntcat x       | x ∈ letters           = P_Idf
                | x ∈ digits            = P_Num
                | x ∈ abstractors       = P_Abstr
                | x ∈ opChars           = P_Op
                | x ∈ separators        = Sep
                | x ∈ delimiters        = Delim
                | x ∈ spaces            = Space
                | otherwise             = error (show x)

-- =================================================================================================
-- == Tokenizer ====================================================================================
-- =================================================================================================

fsa P_Idf       = \s x -> case s of
                           A | x ∈ letters      -> B
                           B | x ∈ idfChars     -> B
                             | otherwise        -> S

fsa P_Num       = \s x -> case s of
                           A | x ∈ digits       -> A
                             | x == '.'         -> B
                             | otherwise        -> S
                           B | x ∈ digits       -> C
                             | otherwise        -> X
                           C | x ∈ digits       -> C
                             | otherwise        -> S

fsa P_Op        = \s x -> case s of
                           A | x ∈ opChars      -> A
                             | otherwise        -> S

fsa P_Abstr     = \s x -> case s of
                           A | x ∈ abstractors  -> A
                             | otherwise        -> S

fsa Delim       = \s x -> case s of
                           A | x ∈ delimiters   -> B
                           B                    -> S

fsa Sep         = \s x -> case s of
                           A | x ∈ separators   -> B
                           B                    -> S

fsa Space       = \s x -> case s of
                           A | x ∈ spaces       -> A
                             | otherwise        -> S

-- =============================================

foldlr fsa c (s,a) [] = [a]

foldlr fsa c (s,a) (x:xs)       | s' == X       = error ("incorrect token: " ++ a)
                                | s' /= S       = foldlr fsa c (s',a') xs
                                | otherwise     = a : foldlr fsa c' (A,"") (x:xs)
                                where
                                  s' = fsa c s x
                                  a' = a ++ [x]
                                  c' = syntcat x

tokenizer (x:xs) = foldlr fsa (syntcat x) (A,"") (x:xs)

lexer str = (cat,str)
        where
          cat | str ∈ booleans  = P_Bln
              | str ∈ resWords  = ResWord
              | otherwise       = syntcat (head str)

remSpaces = filter ((/=Space).fst)

-- =================================================================================================
-- == Grammar ======================================================================================
-- =================================================================================================

gramm nt = case nt of

        P_Expr          -> [[ P_BlnTerm, (*:)[blnOp,P_BlnTerm]                                          ]
                           ,[ P_AppTerm, (*:)[cmpOp,P_AppTerm]                                          ]]

        P_BlnTerm       -> [[ P_RelTerm, (?:)[relOp,P_RelTerm]                                          ]]

        P_RelTerm       -> [[ P_AddTerm, (*:)[addOp,P_AddTerm]                                          ]]

        P_AddTerm       -> [[ P_MulTerm, (*:)[mulOp,P_MulTerm]                                          ]]

        P_MulTerm       -> [[ P_SelTerm, (*:)[selOp,P_SelTerm]                                          ]]

        P_SelTerm       -> [[ P_ExpTerm, (*:)[expOp,P_ExpTerm]                                          ]]

        P_ExpTerm       -> [[ P_AppTerm, (*:)[P_AppTerm]                                                ]]

        P_AppTerm       -> [[ num                                                                       ]
                           ,[ bln                                                                       ]
                           ,[ idf                                                                       ]
                           ,[ empty                                                                     ]

                           ,[ lParenToken, op, rParenToken                                              ]       -- Sectioning
                           ,[ lParenToken, op, P_Expr, rParenToken                                      ]
                           ,[ lParenToken, P_Expr, op, rParenToken                                      ]

                           ,[ lParenToken, P_Expr, comma, P_Expr, rParenToken                           ]       -- Tuples
                           ,[ lParenToken, P_Expr, comma, P_Expr, comma, P_Expr, rParenToken            ]

                           ,[ lSqBracket, rSqBracket                                                    ]       -- Lists
                           ,[ lSqBracket, P_Expr, (*:)[comma,P_Expr], rSqBracket                        ]

                           ,[ lambda, (+:)[P_Pattern], arrow, P_Expr                                    ]       -- Lambda expressions

                           ,[ lett, P_Def, (*:)[comma,P_Def], inn, P_Expr                               ]       -- Let expressions

                           ,[ ifE, P_Expr, thenE, P_Expr, elseE, P_Expr                                 ]       -- if-then-else expressions

                           ,[ lParenSymb, P_Expr, rParenSymb                                            ]]      -- Bracketed expressions

        P_Def           -> [[ P_Pattern, eq, P_Expr                                                     ]]

        P_Pattern       -> [[ idf                                                                       ]
                           ,[ num                                                                       ]
                           ,[ lParenToken, P_Pattern, comma, P_Pattern, rParenToken                     ]
                           ,[ lParenToken, P_Pattern, comma, P_Pattern, comma, P_Pattern, rParenToken   ]
                           ,[ lSqBracket, rSqBracket                                                    ]
                           ,[ lSqBracket, P_Pattern, (*:)[comma, P_Pattern], rSqBracket                 ]]


lParenSymb      = Symbol "("
rParenSymb      = Symbol ")"
comma           = Symbol ","

lambda          = CheckChar (=='\\')
lParenToken     = CheckChar (=='(')
rParenToken     = CheckChar (==')')

lett            = TermSymb "let"
inn             = TermSymb "in"
ifE             = TermSymb "if"
thenE           = TermSymb "then"
elseE           = TermSymb "else"
eq              = TermSymb "="
empty           = TermSymb "empty"

idf             = SyntCat P_Idf
num             = SyntCat P_Num
bln             = SyntCat P_Bln
op              = SyntCat P_Op

cmpOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ [".",";",";>","<;",";>>","<<;","<<<",">>>","<.",".>"])
blnOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["&&","||"])
relOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["<","<=","==","/=",">=",">"])
addOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["+","-"])
mulOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["*","/"])
selOp           = CheckToken (\(nt,s) -> nt==P_Op && s == "!")
expOp           = CheckToken (\(nt,s) -> nt==P_Op && s == "^")
arrow           = CheckToken (\(nt,s) -> nt==P_Op && s == "->")
lSqBracket      = CheckToken (\(nt,s) -> nt==Delim && s == "[")
rSqBracket      = CheckToken (\(nt,s) -> nt==Delim && s == "]")


-- =================================================================================================
-- == Parser Generator =============================================================================
-- =================================================================================================

endSkip nt = case nt of
                Opt  _          -> True
                Rep0 _          -> True
                Alt  nts mts    -> all endSkip nts || all endSkip mts
                Try  nts mts    -> all endSkip nts || all endSkip mts
                Rep1 nts        -> all endSkip nts
                _               -> False


-- ==========================================================================================================
-- Parser Generator
-- ----------------
--      NOTE:
--      - Grammar gr is *function*
--      - nt is non-terminal; nt:rule is the rule under consideration
--      - nt0 is the father node
--      - ts is the list of subtrees under nt0 produced so far
--      - tokens here is the list of *indexed* input tokens
--      - recCheck is used for checking left-recursiveness of the grammar
-- ==========================================================================================================

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[(Int,Token)])]

parserGen gr []        (nt0,ts,tokens,recCheck)  = [(PNode nt0 ts, tokens)]

parserGen gr (nt:rule) (nt0,ts,[],recCheck)      | endSkip nt    = parserGen gr rule (nt0,ts,[],recCheck)
                                                 | otherwise     = [(PError (PNode nt0 ts) (nt:rule) nt "end of input" 0, [])]

parserGen gr (nt:rule) (nt0,ts, allTokens@((k,(cat,str)):remTokens), recCheck)

    | nt ∈ recCheck         = error ("grammar is left-recursive. Chain: " ++ show (recCheck ++ [nt]))
    | otherwise             = case nt of

        -- ============================================================================================================
        -- Backus-Naur constructions

        Alt nts mts     ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr (mts++rule)                (nt0,ts,allTokens,recCheck)

        Opt  nts        ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,recCheck)

        Rep0 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,recCheck)

        Rep1 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,recCheck)

        -- ============================================================================================================
        -- Terminal Symbols

        Symbol str'     | str==str'     -> parserGen gr rule (nt0,ts,remTokens,[])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        TermSymb str'   | str==str'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        SyntCat cat'    | cat==cat'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        CheckToken p    | p (cat,str)   -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        CheckChar p     | p (head str)  -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        -- ============================================================================================================
        -- Non-terminals

        _  ->  concat [ nextParses
                        | r <- gr nt
                        , let parses        = parserGen gr r (nt,[],allTokens, recCheck++[nt])
                        , let correctParses = filter (not.isPError.fst) parses

                        , let nextParses | null correctParses = [ (finalPError (nt0,ts) $ maximum $ map fst parses , []) ]

                                         | otherwise          = concat $ map (parserGen gr rule) nextParseStates
                                                              where
                                                                nextParseStates = [ (nt0,ts++[t],remTokens,[])
                                                                                    | (t,remTokens) <- correctParses ]
                      ]

-- ==================================================
-- Additional functions

isPError (PError _ _ _ _ _) = True
isPError _                  = False

finalPError (nt0,ts) (PError t rule nt str k) = PError (PNode nt0 (ts++[t])) rule nt str k

-- ==================================================
-- Top-level parse function

parseTinadic :: Grammar -> Alphabet -> [Token] -> ParseTree

parseTinadic gr s tokens | null correctParses = maximum $ map fst parses
                  | not $ null rest    = error ("tokenList not fully parsed. Still left: " ++ (show $ map snd rest))
                  | otherwise          = final
          where
            parses = [ (t,rem) | r <- gr s
                               , (t,rem) <- parserGen gr r (s,[],tokens',[])
                               ]

            tokens' = zip [0..] tokens                        -- indexed tokens

            correctParses = filter (not.isPError.fst) parses

            (final,rest)  = head correctParses


parse = parseTinadic gramm P_Expr . remSpaces . map lexer . tokenizer

-- =================================================================================================
-- == ParseTree -> AST =============================================================================
-- =================================================================================================

op2str (PLeaf (P_Op ,str)) = str
op2str (PLeaf (Delim,str)) = str
op2str x                   = error ("up2str:   " ++ show x)

prefixLam es  = PNode P_AppTerm $ PLeaf (P_Abstr,"\\")  : es
prefixLet es  = PNode P_AppTerm $ PLeaf (ResWord,"let") : es

-- ============================

pt2expr (PLeaf (nt,str)) = case nt of
  P_Idf                                                                         -> Idf str
  P_Num                                                                         -> Num (read str)
  P_Bln                                                                         -> Bln (read str)

pt2expr (PNode nt subtrees) = case nt of

  P_Expr        -> case subtrees of
                [e]                                                             -> pt2expr e

                [f,o,g]                 | op2str o == ".>"                      -> Lambda (Idf "_x_") $ Lambda (Idf "_y_") $ pt2expr g # (pt2expr f # (Idf "_x_")) # (Idf "_y_")

                                        | op2str o == "<."                      -> Lambda (Idf "_x_") $ Lambda (Idf "_y_") $ pt2expr f # (Idf "_x_") # (pt2expr g # (Idf "_y_"))


                _                       | op2str o' ∈ cmpOps                    -> Lambda (Idf "x") $ App (Compose (op2str o')
                                                                                                          (pt2expr $ PNode P_Expr $ init $ init subtrees)
                                                                                                          (pt2expr $ last subtrees)
                                                                                                          ) (Idf "x")

                                        | otherwise                             -> Boolean (op2str o')
                                                                                           (pt2expr $ PNode P_Expr $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_BlnTerm     -> case subtrees of
                [e]                                                             -> pt2expr e
                [e0,o,e1]                                                       -> Boolean (op2str o) (pt2expr e0) (pt2expr e1)

  P_RelTerm    -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_RelTerm $ init $ init subtrees)   -- to get left-associativity
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_AddTerm    -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_AddTerm $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_MulTerm -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Sel (pt2expr $ PNode P_MulTerm $ init $ init subtrees)
                                                                                       (pt2expr $ last subtrees)

  P_SelTerm -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_SelTerm $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_ExpTerm  -> foldl (#) (pt2expr e) (map pt2expr es)
            where
              (e:es) = subtrees

  P_AppTerm -> case subtrees of
                [e]                                                             -> pt2expr e

                [PLeaf (Delim,"("), PLeaf (P_Op,o),     PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Numeric o (Idf "_u_") (Idf "_v_")
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Boolean o (Idf "_u_") (Idf "_v_")
                        | o ∈ cmpOps                                            -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Compose o (Idf "_u_") (Idf "_v_")

                [PLeaf (Delim,"("), e, PLeaf (P_Op,o), PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_v_") $ Numeric o (pt2expr e) (Idf "_v_")
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_v_") $ Boolean o (pt2expr e) (Idf "_v_")
                        | o ∈ cmpOps                                            -> Lambda (Idf "_v_") $ Compose o (pt2expr e) (Idf "_v_")

                [PLeaf (Delim,"("), PLeaf (P_Op,o), e, PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_u_") $ Numeric o (Idf "_u_") (pt2expr e)
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_u_") $ Boolean o (Idf "_u_") (pt2expr e)
                        | o ∈ cmpOps                                            -> Lambda (Idf "_u_") $ Compose o (Idf "_u_") (pt2expr e)

                [PLeaf (Delim,"("), e0, e1,             PLeaf (Delim,")")]      -> Pair (pt2expr e0) (pt2expr e1)
                [PLeaf (Delim,"("), e0, e1, e2,         PLeaf (Delim,")")]      -> Triple (pt2expr e0) (pt2expr e1) (pt2expr e2)

                [PLeaf (Delim,"["), PLeaf(Delim,"]")]                           -> Null
                (PLeaf (Delim,"[") : e : es)                                    -> Cons (pt2expr e) $ pt2expr $ PNode P_AppTerm $ PLeaf(Delim,"[") : es

                [PLeaf (P_Abstr,"\\") , PLeaf (P_Op,"->") , body]               -> pt2expr body
                (PLeaf (P_Abstr,"\\") : pattern : es)                           -> Lambda (pt2expr pattern) (pt2expr $ prefixLam es)

                [PLeaf (ResWord,"let") , PLeaf (ResWord,"in") , body]           -> Let [] $ pt2expr body
                (PLeaf (ResWord,"let") : PNode P_Def def : es)                  -> Let (Def (pt2expr pattern) (pt2expr e) : defs) e'
                                                                                where
                                                                                  [pattern, PLeaf (P_Op,"="), e] = def
                                                                                  Let defs e' =  pt2expr $ prefixLet es

                [PLeaf (ResWord,"if")  , e0,
                 PLeaf (ResWord,"then"), e1,
                 PLeaf (ResWord,"else"), e2]                                    -> IfE (pt2expr e0) (pt2expr e1) (pt2expr e2)

                _                                                               -> error ("pt2expr: " ++ show subtrees)

  P_Pattern -> case subtrees of
                [PLeaf (P_Idf,str)]                                             -> Idf str
                [PLeaf (Delim,"("), e0, e1,     PLeaf (Delim,")")]              -> Pair (pt2expr e0) (pt2expr e1)
                [PLeaf (Delim,"("), e0, e1, e2, PLeaf (Delim,")")]              -> Triple (pt2expr e0) (pt2expr e1) (pt2expr e2)
                [PLeaf (Delim,"["), PLeaf (Delim,"]")]                          -> Null
                (PLeaf (Delim,"[") : e : es)                                    -> Cons (pt2expr e) $ pt2expr $ PNode P_Pattern $ PLeaf(Delim,"[") : es

  _        -> error ("pt2expr -- Found: " ++ show nt ++ " --- Should not occur")

