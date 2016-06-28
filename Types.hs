{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

module Types where

import Data.List
import Data.Hashable
import GHC.Generics (Generic)

-- =================================================================================================
-- == Types ========================================================================================
-- =================================================================================================

type Grammar    = Alphabet -> [[Alphabet]]

data Alphabet   = Symbol     String             -- Token given ("char" specific for this example)
                | TermSymb   String             -- A given string, but included in the parsetree
                | SyntCat    Alphabet           -- A given string, but included in the parsetree
                | CheckChar  (Char->Bool)       -- Character should have some property (for tokenizer)
                | CheckToken (Token->Bool)      -- Token should have some property (for parser)

                | Alt   [Alphabet] [Alphabet]   -- Try both
                | Try   [Alphabet] [Alphabet]   -- If first doesn't work, try second
                | Opt   [Alphabet]              -- Optional
                | Rep0  [Alphabet]              -- Zero or more repetitions
                | Rep1  [Alphabet]              -- One or more repetitions

                | P_Idf                         -- Identifier
                | P_Expr                        -- Expression
                | P_BlnTerm                     -- Boolean term
                | P_RelTerm                     -- Relational term
                | P_AddTerm                     -- Additive term
                | P_MulTerm                     -- Multiplicative term
                | P_SelTerm                     -- Selection term
                | P_ExpTerm                     -- Exponent term
                | P_AppTerm                     -- Applicative term
                | P_Def                         -- Let-definition
                | P_Num                         -- Number
                | P_Bln                         -- Boolean
                | P_Op                          -- Operator
                | P_Abstr                       -- Abstraction (lambda)
                | P_Pattern                     -- Formal parameters in lambda expressions

                | ResWord                       -- Reserved Word
                | Sep                           -- Separator (comma, semicolon)
                | Delim                         -- Delimiter (brackets)
                | Space                         -- Spaces (spacebar,tab, newline)
                deriving (Eq,Show)


ps <>  qs       = Alt  ps qs
ps <<> qs       = Try  ps qs
(?:) ps         = Opt  ps
(*:) ps         = Rep0 ps
(+:) ps         = Rep1 ps

type Token      = (Alphabet,String)             -- Alphabet: indicates the "syntactic category" to which
                                                --      the String belongs (to distinguish, a.o., between
                                                --      reserved words and identifiers in general),
                                                -- String: the token itself,

data FsaState   = A | B | C                     -- interne FSA-states
                | S | X                         -- Stop state, Error state
                deriving (Eq,Show)

data ParseTree  = PLeaf Token
                | PNode Alphabet [ParseTree]
                | PError ParseTree [Alphabet] Alphabet String Int
                deriving (Eq,Show)

instance Ord ParseTree where
  PError _ _ _ _ k <  PError _ _ _ _ k' = k <  k'
  _                <  _                 = error "ordering only in case of parse-errors"

  PError _ _ _ _ k <= PError _ _ _ _ k' = k <= k'
  _                <= _                 = error "ordering only in case of parse-errors"


type ParseState = ( Alphabet                    -- Non-terminal indicating the present subexpression
                  , [ParseTree]                 -- The already produced trees within the present subexpression
                  , [(Int,Token)]               -- The remaining list of *indexed* input tokens
                  , [Alphabet]                  -- List of non-terminals to check for left-recursiveness
                  )

-- Formally necessary, never really used (6x)
instance Eq (Char->Bool)        where f == g = True
instance Eq (Token->Bool)       where f == g = True
instance Eq (String->Bool)      where f == g = True
instance Show (Char->Bool)      where show f = "Char->Bool"
instance Show (Token->Bool)     where show f = "Token->Bool"
instance Show (String->Bool)    where show f = "String->Bool"

data Expr  = Idf     String                     -- for variables
           | Num     Int                        -- for numerical constants
           | Bln     Bool                       -- for boolean constants
           | Empty                              -- for completeness

           | Numeric String Expr Expr           -- for numerical expressions such as "3+5"
           | Boolean String Expr Expr           -- for boolean expressions
           | Compose String Expr Expr           -- for composition expressions
           | Pair    Expr   Expr                -- for pairs: "(3,5)"
           | Triple  Expr   Expr Expr           -- for triples: "(3,4,5)"
           | Null                               -- for empty list
           | Cons    Expr   Expr                -- for list-constructor: 3:[4,5]
           | Sel     Expr   Expr                -- for selection expressions such as "xs!!i"
           | IfE     Expr   Expr Expr           -- for if-then-else expressions

           | App     Expr   Expr                -- for function application

           | Def     Expr   Expr                -- Only pattern as defined expr
           | Let     [Expr] Expr                -- Only Def-expressions in list
           | Lambda  Expr   Expr                -- Only pattern as formal parameter

           | TpDef Expr Expr
        
           -- Converted forms start here
           | FuncCall String [Expr]
           | FuncDef String [String] Expr
           | FuncSig Int -- Only amount of args; return type is always Int
           | Type Expr Expr
           | List Expr

           -- Typed
           | TIdf String Expr -- Expr at the end is the type
           deriving (Eq, Show, Generic)

instance Hashable Expr

-- for Completeness (not used)
data Stmnt = Skip
           | Break
           | Assign  Expr Expr
           | IfS     Expr [Stmnt] [Stmnt]
           | For     Expr (Int,String,Int) [Stmnt]
           | While   Expr [Stmnt]
           deriving (Eq,Show)


class PrPr a where
        toStrings ::  a  -> [String]
        prpr      ::  a  -> IO()


-- =================================================================================================

x ∈ xs          = x `elem` xs

xs!i            = Sel xs i

f#x             = App f x

upLetters       = ['A' .. 'Z']
lowLetters      = ['a' .. 'z']
letters         = upLetters ++ lowLetters
digits          = ['0' .. '9']
idfChars        = letters ++ digits ++ "_'"
abstractors     = "\\"
opChars         = "+*-/%^:!#=><.~&|;"
separators      = ","
delimiters      = "(){}[]"
spaces          = " \t\n"

resWords        = [ "let", "in", "if", "then", "else", "Skip" ]
booleans        = [ "True", "False" ]

numOps          = ["+","-","*","/","^"]
relOps          = [">",">=","==","/=","<=","<"]
blnOps          = ["&&","||","##","=>"]
cmpOps          = [".",";",";>","<;",";>>","<<;","<<<",">>>","<.",".>"]
