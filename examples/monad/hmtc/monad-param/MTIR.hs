{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         MTIR                                                 *
*       Purpose:        MiniTriangle Internal Representation                 *
*                       (typechecked AST with semantical annotations)        *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2014                  *
*                                                                            *
******************************************************************************
-}


-- | MiniTriangle Internal Representation. The definitions mirror the
-- AST, but the program should be type correct at this stage and semantical
-- annotations have been added. In particular, variables are represented
-- by symbols (with name, type info, and source position) as opposed to just
-- their names.

module MTIR (
    -- MTIR types
    MTIR (..),          -- Not abstract. Instances: HasSrcPos.
    Command (..),       -- Not abstract. Instances: HasSrcPos.
    Expression (..),    -- Not abstract. Instances: HasSrcPos.
    Declaration (..),   -- Not abstract. Instances: HasSrcPos.
    MTInt,              -- Representation type for MiniTriangle integers.
    MTChr               -- Representation type for MiniTriangle characters.
) where

-- HMTC module imports
import SrcPos
import Name
import Type
import Symbol (IntTermSym)

-- | Internal representation of MiniTriangle Programs
data MTIR = MTIR { mtirCmd :: Command }


instance HasSrcPos MTIR where
    srcPos = cmdSrcPos . mtirCmd


-- | Internal representation of syntactic category Command
data Command
    -- | Assignment
    = CmdAssign {
          caVar     :: Expression,      -- ^ Assigned variable
          caVal     :: Expression,      -- ^ Right-hand side expression
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
    | CmdRepeat {
          crBody    :: Command,           -- ^ Loop-body
          crCond    :: Expression,        -- ^ Loop-condition
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


-- | Internal representation of syntactic category Expression
data Expression
    -- | Literal Boolean
    = ExpLitBool {
          elbVal    :: Bool,            -- ^ Literal Boolean.
          expType   :: Type,            -- ^ Type
          expSrcPos :: SrcPos
      }
    -- | Literal integer
    | ExpLitInt {
          eliVal    :: MTInt,           -- ^ Literal integer.
          expType   :: Type,            -- ^ Type
          expSrcPos :: SrcPos
      }
    -- | Literal character
    | ExpLitChr {
          elcVal    :: MTChr,           -- ^ Literal character.
          expType   :: Type,            -- ^ Type
          expSrcPos :: SrcPos
      }
    -- | External reference (procedure/function)
    | ExpExtRef {
          eerVal    :: Name,            -- ^ Name of external entity.
          expType   :: Type,            -- ^ Type
          expSrcPos :: SrcPos
      }
    -- | Variable reference
    | ExpVar {
          evVar     :: IntTermSym,      -- ^ Referenced variable (symbol!)
          expType   :: Type,            -- ^ Type
          expSrcPos :: SrcPos
      }
    -- | Dereferencing of reference variable
    | ExpDeref {
          edArg     :: Expression,      -- ^ Argument
          expType   :: Type,            -- ^ Type (after dereferencing)
          expSrcPos :: SrcPos
      }
    -- | Function or n-ary operator application
    | ExpApp {
          eaFun     :: Expression,      -- ^ Applied function or operator
          eaArgs    :: [Expression],    -- ^ Arguments
          expType   :: Type,            -- ^ Type (of application)
          expSrcPos :: SrcPos
      }
    -- | Conditional expression
    | ExpCond {
          ecCond    :: Expression,      -- ^ Condition
          ecTrue    :: Expression,      -- ^ Value if condition is true
          ecFalse   :: Expression,      -- ^ Value if condition is false
          expType   :: Type,            -- ^ Type (of conditional expression)
          expSrcPos :: SrcPos
      }
    -- | Array expression
    | ExpAry {
          eaElts    :: [Expression],    -- ^ Array elements
          expType   :: Type,
          expSrcPos :: SrcPos
      }
    -- | Array indexing
    | ExpIx {
          eiAry     :: Expression,      -- ^ Array expression
          eiIx      :: Expression,      -- ^ Indexing expression
          expType   :: Type,
          expSrcPos :: SrcPos
      }
    -- | Record expression
    | ExpRcd {
          erFldDefs :: [(Name,Expression)],     -- ^ Record field definitions;
                                                -- assumed to be sorted by name
          expType   :: Type,
          expSrcPos :: SrcPos
      }
    -- | Record projection
    | ExpPrj {
          epRcd     :: Expression,      -- ^ Record expression
          epFld     :: Name,            -- ^ Field to project out
          expType   :: Type,
          expSrcPos :: SrcPos
      }


instance HasSrcPos Expression where
    srcPos = expSrcPos


-- | Internal representation of syntactic category Declaration
-- (Most of these are also definitions. Note that symbols carry the types.)
data Declaration
    -- | Constant declaration
    = DeclConst {
          dcConst    :: IntTermSym,     -- ^ Declared constant (symbol!)
          dcVal      :: Expression      -- ^ Value of defined constant
      }
    -- | Variable declaration
    | DeclVar {
          dvVar      :: IntTermSym,      -- ^ Declared variable (symbol!)
          dvMbVal    :: Maybe Expression -- ^ Initial value of declared
                                         -- variable, if any
      }
    -- | Function declaration
    | DeclFun {
          dfFun      :: IntTermSym,     -- ^ Declared function (symbol!)
          dfArgs     :: [IntTermSym],   -- ^ Formal arguments (with types)
          dfBody     :: Expression      -- ^ Function boody
      }
    -- | Procedure declaration
    | DeclProc {
          dpProc     :: IntTermSym,     -- ^ Declared procedure (symbol!)
          dpArgs     :: [IntTermSym],   -- ^ Formal arguments (with types)
          dpBody     :: Command         -- ^ Procedure boody
      }


instance HasSrcPos Declaration where
    srcPos (DeclConst {dcConst = s}) = srcPos s
    srcPos (DeclVar   {dvVar = s})   = srcPos s
    srcPos (DeclFun   {dfFun = s})   = srcPos s
    srcPos (DeclProc  {dpProc = s})  = srcPos s
