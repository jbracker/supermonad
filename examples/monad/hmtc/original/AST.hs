{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         AST                                                  *
*       Purpose:        MiniTriangle Abstract Syntax Tree                    *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2014                  *
*                                                                            *
******************************************************************************
-}

-- | MiniTriangle Abstract Syntax Tree. Representation of MiniTriangle programs
-- after parsing but prior to type-checking.

module AST (
    AST (..),           -- Not abstract. Instances: HasSrcPos.
    Command (..),       -- Not abstract. Instances: HasSrcPos.
    Expression (..),    -- Not abstract. Instances: HasSrcPos.
    Declaration (..),   -- Not abstract. Instances: HasSrcPos.
    TypeDenoter (..),   -- Not abstract. Instances: HasSrcPos.
    ArgDecl (..),       -- Not abstract. Instances: HasSrcPos.
    ArgMode (..)        -- Not abstract. Instances: Eq, Show.
) where

-- HMTC module imports
import Name
import SrcPos

-- Note on Naming Conventions for Constructors and Field Labels
--
-- In Haskell, two (or more) datatypes that are in scope simultaneoulsy
-- must not have any constructors or field labels in common. However,
-- different constructors of the same type may have common field names,
-- provided the fields all have the same type. This is very different
-- from records in languages like Pascal or C, and from objects in OO
-- languages like Java, where sharing names across different records or
-- objects are both possible and common.
--
-- To avoid name clashes, while still making it possible to use similar
-- names for similar things in different type declarations, some simple
-- naming conventins have been adopted:
--
--   * Constructors get prefix which is an abbreviation of the name of
--     the data type. E.g. for 'Command', the prefix is 'Cmd', and a
--     typical constructor name is 'CmdAssign', and for 'TypeDenoter',
--     te prefix is 'TD'.
--
--   * Field names that are common to one or more constructors, get the
--     same prefix as the constructor, but in lower-case.
--
--   * Field names that are specific to a contructor get a lower-case
--     prefix that is an abbreviation of the constructor. E.g. the
--     prefix for 'CmdAssign' is 'ca', and one of its fields is 'caVar'.

-- | Abstract syntax for the syntactic category Program
data AST = AST { astCmd :: Command }


instance HasSrcPos AST where
    srcPos = cmdSrcPos . astCmd


-- | Abstract syntax for the syntactic category Command

-- For generality, the variable being assigned to, the procedure being
-- called, and the function being applied (currently only operators) are
-- represented by expressions as opposed to just an identifier (for
-- variables, procedures, and functions) or an operator. Consider
-- assignment to an array element, for example, where the RHS (e.g. x[i])
-- really is an expression that gets evaluated to a memory reference
-- (sink). Also, this arrangement facilitates error reporting, as a
-- variable expression has an associated source position, whereas names,
-- currently represented by strings, have not.

data Command
    -- | Assignment
    = CmdAssign {
          caVar     :: Expression,      -- ^ Assigned variable
          caVal     :: Expression,      -- ^ Right-hand-side expression
          cmdSrcPos :: SrcPos
      }
    -- | Procedure call
    | CmdCall {
          ccProc    :: Expression,      -- ^ Called procedure
          ccArgs    :: [Expression],    -- ^ Arguments
          cmdSrcPos :: SrcPos
      }
    -- | Command sequence (block)
    | CmdSeq {
          csCmds    :: [Command],       -- ^ Commands
          cmdSrcPos :: SrcPos
      }
    -- | Conditional command
    | CmdIf {
          ciCondThens :: [(Expression,
                           Command)],   -- ^ Conditional branches
          ciMbElse    :: Maybe Command, -- ^ Optional else-branch
          cmdSrcPos   :: SrcPos
      }
    -- | While-loop
    | CmdWhile {
          cwCond    :: Expression,      -- ^ Loop-condition
          cwBody    :: Command,         -- ^ Loop-body
          cmdSrcPos :: SrcPos
      }
    -- | Repeat-loop
    | CmdRepeat {
          crBody    :: Command,         -- ^ Loop-body
          crCond    :: Expression,      -- ^ Loop-condition
          cmdSrcPos :: SrcPos
      }
    -- | Let-command
    | CmdLet {
          clDecls   :: [Declaration],   -- ^ Declarations
          clBody    :: Command,         -- ^ Let-body
          cmdSrcPos :: SrcPos
      }


instance HasSrcPos Command where
    srcPos = cmdSrcPos


-- | Abstract syntax for the syntactic category Expression
data Expression
    -- | Literal integer
    = ExpLitInt {
          eliVal    :: Integer,         -- ^ Integer value
          expSrcPos :: SrcPos
      }
    -- | Literal character
    | ExpLitChr {
          elcVal    :: Char,            -- ^ Character value
          expSrcPos :: SrcPos
      }
    -- | Variable reference
    | ExpVar {
          evVar     :: Name,            -- ^ Name of referenced variable
          expSrcPos :: SrcPos
      }
    -- | Function or n-ary operator application
    | ExpApp {
          eaFun     :: Expression,      -- ^ Applied function or operator
          eaArgs    :: [Expression],    -- ^ Arguments
          expSrcPos :: SrcPos
      }
    -- | Array expression
    | ExpAry {
          eaElts    :: [Expression],    -- ^ Array elements
          expSrcPos :: SrcPos
      }
    -- | Array indexing
    | ExpIx {
          eiAry     :: Expression,      -- ^ Array expression
          eiIx      :: Expression,      -- ^ Indexing expression
          expSrcPos :: SrcPos
      }
    -- | Record expression
    | ExpRcd {
          erFldDefs :: [(Name,Expression)],     -- ^ Record field definitions
          expSrcPos :: SrcPos
      }
    -- | Record projection
    | ExpPrj {
          epRcd     :: Expression,      -- ^ Record expression
          epFld     :: Name,            -- ^ Field to project out
          expSrcPos :: SrcPos
      }
    -- | Conditional expression
    | ExpCond {
          ecCond    :: Expression,      -- ^ Condition
          ecTrue    :: Expression,      -- ^ Value if condition true
          ecFalse   :: Expression,      -- ^ Value if condition false
          expSrcPos :: SrcPos
      }


instance HasSrcPos Expression where
    srcPos = expSrcPos


-- | Abstract syntax for the syntactic category Declaration
-- (Most of these are also definitions.)
data Declaration
    -- | Constant declaration
    = DeclConst {
          dcConst    :: Name,           -- ^ Name of declared constant
          dcType     :: TypeDenoter,    -- ^ Type of declared constant
          dcVal      :: Expression,     -- ^ Value of declared constant
          declSrcPos :: SrcPos
      }
    -- | Variable declaration
    | DeclVar {
          dvVar      :: Name,           -- ^ Name of declared variable
          dvType     :: TypeDenoter,    -- ^ Type of declared variable
          dvMbVal    :: Maybe Expression, -- ^ Initial value of declared
                                          -- varible, if any
          declSrcPos :: SrcPos
      }
    -- | Function declaration
    | DeclFun {
          dfFun      :: Name,           -- ^ Name of declared function
          dfOvrld    :: Bool,           -- ^ Whether decl. of overloaded fun.
          dfArgDecls :: [ArgDecl],      -- ^ Declarations of formal arguments
          dfType     :: TypeDenoter,    -- ^ Return type of declared function
          dfBody     :: Expression,     -- ^ Function boody
          declSrcPos :: SrcPos
      }
    -- | Procedure declaration
    | DeclProc {
          dpProc     :: Name,           -- ^ Name of declared procedure
          dpOvrld    :: Bool,           -- ^ Whether decl. of overloaded proc.
          dpArgDecls :: [ArgDecl],      -- ^ Declarations of formal arguments
          dpBody     :: Command,        -- ^ Procedure boody
          declSrcPos :: SrcPos
      }


instance HasSrcPos Declaration where
    srcPos = declSrcPos


-- | Abstract syntax for the syntactic category ArgDecl
data ArgDecl
    -- | Declaration of formal argument
    = ArgDecl {
          adArg      :: Name,           -- ^ Name of declared argument
          adArgMode  :: ArgMode,        -- ^ Argument passing mode
          adType     :: TypeDenoter,    -- ^ Type of defined constant
          adSrcPos   :: SrcPos
      }


instance HasSrcPos ArgDecl where
    srcPos = adSrcPos


-- | Argument passing modes
data ArgMode
    = ByValue   -- ^ Call-by-value
    | ByRefIn   -- ^ Call-by-reference in argument (ref. to source)
    | ByRefOut  -- ^ Call-by-reference out argument (ref. to sink)
    | ByRefVar  -- ^ Call-by-reference var (in/out) argument (ref. to variable)
    deriving (Eq, Show)


-- | Abstract syntax for the syntactic category TypeDenoter

-- Right now, the only types are simple base types, like Integer and Bool,
-- and fixed-size arrays and records of such types.
-- If MiniTriangle were extended to allow users to express e.g. function
-- types, then this data declaration would have to be extended.
data TypeDenoter
    -- | Base Type
    = TDBaseType {
          tdbtName :: Name,                     -- ^ Name of the base type
          tdSrcPos :: SrcPos
      }
    | TDArray {
          tdaEltType :: TypeDenoter,            -- ^ Type of array elements
          tdaSize    :: Integer,                -- ^ Size of the array
          tdSrcPos   :: SrcPos
      }
    | TDRecord {
          tdrFldTypes :: [(Name, TypeDenoter)], -- ^ Name and type of fields
          tdSrcPos    :: SrcPos
      }


instance HasSrcPos TypeDenoter where
    srcPos = tdSrcPos
