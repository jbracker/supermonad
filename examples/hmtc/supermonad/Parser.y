-- ***************************************************************************
-- *                               H M T C                                   *
-- *                                                                         *
-- *    Module:         Parser                                               *
-- *    Purpose:        MiniTriangle parser                                  *
-- *    Authors:        Henrik Nilsson                                       *
-- *                                                                         *
-- *               Copyright (c) Henrik Nilsson, 2006 - 2015                 *
-- *                                                                         *
-- ***************************************************************************

{
-- | MiniTriangle parser

module Parser (
    parse,              -- :: String -> DF AST
    testParser          -- :: String -> IO ()
) where

-- Standard library imports
import Data.Maybe (isJust)

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import Token
import AST
import PPAST
import ParseMonad
import Scanner

}

----------------------------------------------------------------
-- Parser
----------------------------------------------------------------

-- Happy grammar with semantic actions for building an AST.
-- Convention: Terminals are either written in upper case or within
-- single quotes. Non-terminals are written in lower case.

%name parseAux

%monad { P } { >>= } { return }

%lexer { scanner } { (EOF, _) }

-- The terminal symbols are tokens paired with a source code position.
%tokentype { (Token, SrcPos) }

-- The semantic values of a constant terminal symbol is the associated
-- source code position. The semantic value of a terminal symbol that
-- carry additional information (like identifiers) is the token and
-- source code position pair itself.
%token
    '('         { (LPar, $$) }
    ')'         { (RPar, $$) }
    '['         { (LBrk, $$) }
    ']'         { (RBrk, $$) }
    '{'         { (LBrc, $$) }
    '}'         { (RBrc, $$) }
    ','         { (Comma, $$) }
    '.'         { (Period, $$) }
    ';'         { (Semicol, $$) }
    ':'         { (Colon, $$) }
    ':='        { (ColEq, $$) }
    '='         { (Equals, $$) }
    '?'         { (Cond, $$) }
    BEGIN       { (Begin, $$) }
    CONST       { (Const, $$) }
    DO          { (Do, $$) }
    ELSE        { (Else, $$) }
    ELSIF       { (Elsif, $$) }
    END         { (End, $$) }
    FUN         { (Fun, $$) }
    IF          { (If, $$) }
    IN          { (In, $$) }
    LET         { (Let, $$) }
    OUT         { (Out, $$) }
    OVERLOADED  { (Overloaded, $$) }
    PROC        { (Proc, $$) }
    REPEAT      { (Repeat, $$) }
    THEN        { (Then, $$) }
    UNTIL       { (Until, $$) }
    VAR         { (Var, $$) }
    WHILE       { (While, $$) }
    LITINT      { (LitInt {}, _) }
    LITCHR      { (LitChr {}, _) }
    ID          { (Id {}, _) }
    '++'        { (Op {opName="++"},  _) }
    '--'        { (Op {opName="--"},  _) }
    '+'         { (Op {opName="+"},   _) }
    '-'         { (Op {opName="-"},   _) }
    '*'         { (Op {opName="*"},   _) }
    '/'         { (Op {opName="/"},   _) }
    '^'         { (Op {opName="^"},   _) }
    '<'         { (Op {opName="<"},   _) }
    '<='        { (Op {opName="<="},  _) }
    '=='        { (Op {opName="=="},  _) }
    '!='        { (Op {opName="!="},  _) }
    '>='        { (Op {opName=">="},  _) }
    '>'         { (Op {opName=">"},   _) }
    '&&'        { (Op {opName="&&"},  _) }
    '||'        { (Op {opName="||"},  _) }
    '!'         { (Op {opName="!"},   _) }

%right '?' ':'
%left '||'
%left '&&'
%nonassoc '<' '<=' '==' '!=' '>=' '>'
%left '+' '-'
%left '*' '/'
%right '^'

%%

program :: { AST }
program : command       { AST $1 }


commands :: { [Command] }
commands : command              { [$1] } 
         | command ';' commands { $1 : $3 }


command :: { Command }
command
    : var_expression ':=' expression
        { CmdAssign {caVar = $1, caVal=$3, cmdSrcPos = srcPos $1} }
    | var_expression '(' expressions ')'
        { CmdCall {ccProc = $1, ccArgs = $3, cmdSrcPos = srcPos $1} }
    | IF expression THEN command elsifs optelse
        { CmdIf {ciCondThens = ($2,$4) : $5, ciMbElse = $6, cmdSrcPos = $1} }
    | WHILE expression DO command
        { CmdWhile {cwCond = $2, cwBody = $4, cmdSrcPos = $1} }
    | REPEAT command UNTIL expression
        { CmdRepeat {crBody = $2, crCond = $4, cmdSrcPos = $1} }
    | LET declarations IN command
        { CmdLet {clDecls = $2, clBody = $4, cmdSrcPos = $1} }
    | BEGIN commands END
        { if length $2 == 1 then
              head $2
          else
              CmdSeq {csCmds = $2, cmdSrcPos = srcPos $2}
        }

optelse :: { Maybe Command }
optelse : {- epsilon -}
           { Nothing }
        | ELSE command
           { Just $2 }


elsifs :: { [(Expression, Command)] }
elsifs : {- epsilon -}
           { [] }
       | ELSIF expression THEN command elsifs
           { ($2,$4) : $5 }


expressions :: { [Expression] }
expressions : {- epsilon -} { [] }
            | expressions1 { $1 } 


expressions1 :: { [Expression] }
expressions1 : expression { [$1] }
             | expression ',' expressions { $1 : $3 }


-- The terminal associated with a precedence declaration has to occur
-- *literally* in a rule if precedence declarations are to be taken into
-- account. That means a lot of repetitive productions. To simplify a bit,
-- non-terminals for *classes* of operators having the same precedence
-- and associativity are introduced, along with one expression production
-- for each precedence level. The latter production has to be annotated with
-- an explicit precedence declaration.
--
-- (Alternatively, the scanner could classify operators into classes. But it
-- was decided to handle precedence and associativity completely within the
-- parser.)

expression :: { Expression }
expression
    : primary_expression
        { $1 }
    | expression '?' expression ':' expression
        { ExpCond {ecCond    = $1,
                   ecTrue    = $3,
                   ecFalse   = $5,
                   expSrcPos = srcPos $1} } 
    | expression opclass_disjunctive expression %prec '||'
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_conjunctive expression %prec '&&'
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_relational expression %prec '=='
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_additive expression %prec '+'
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_multiplicative expression %prec '*'
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_exponential expression %prec '^'
        { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }


primary_expression :: { Expression }
    : LITINT
        { ExpLitInt {eliVal = tspLIVal $1, expSrcPos = tspSrcPos $1} }
    | LITCHR
        { ExpLitChr {elcVal = tspLCVal $1, expSrcPos = tspSrcPos $1} }
    | var_expression
        { $1 }
    | opclass_prefix_unary primary_expression
        { ExpApp {eaFun = $1, eaArgs = [$2], expSrcPos = srcPos $1}}
    | primary_expression opclass_postfix_unary
        { ExpApp {eaFun = $2, eaArgs = [$1], expSrcPos = srcPos $1}}
    | var_expression '(' expressions ')'
        { ExpApp {eaFun = $1, eaArgs = $3, expSrcPos = srcPos $1} }
    | '[' expressions ']'
        { ExpAry {eaElts = $2, expSrcPos = $1} }
    | '{' field_defs '}'
        { ExpRcd {erFldDefs = $2, expSrcPos = $1} }
    | '(' expression ')'
        { $2 }


-- Syntactically limited form of expressions for variables, procedures,
-- and functions. Facilitates parsing and rules out certain unsupported
-- constructs already syntactically (would otherwise be caught by the type
-- checker; e.g. (f(x))(y) not possible because no support for returning
-- functions). Still, convenient to represent by an expression in the
-- abstract syntax. Also indexing of array expressions or projection of
-- record expressions (e.g. [1,2,3][1] or {a = 1}.a) not permitted as
-- indexing/projecting on references simplifies the type system and as there
-- is little point i supporting this anyway (in particular the latter).

var_expression :: { Expression }
    : ID
        { ExpVar {evVar = tspIdName $1, expSrcPos = tspSrcPos $1} }
    | var_expression '[' expression ']'
        { ExpIx {eiAry = $1, eiIx = $3, expSrcPos = srcPos $1} }
    | var_expression '.' ID
        { ExpPrj {epRcd = $1, epFld = tspIdName $3, expSrcPos = srcPos $1} }


opclass_disjunctive :: { Expression }
    : '||' { mkExpVarInfixOp $1 }

opclass_conjunctive :: { Expression }
    : '&&' { mkExpVarInfixOp $1 }

opclass_relational :: { Expression }
    : relational_op { mkExpVarInfixOp $1 }

relational_op
    : '<'  { $1 }
    | '<='  { $1 }
    | '=='  { $1 }
    | '!='  { $1 }
    | '>='  { $1 }
    | '>'  { $1 }

opclass_additive :: { Expression }
    : additive_op { mkExpVarInfixOp $1 }

additive_op
    : '+'  { $1 }
    | '-'  { $1 }

opclass_multiplicative :: { Expression }
    : multiplicative_op { mkExpVarInfixOp $1 }

multiplicative_op
    : '*'  { $1 }
    | '/'  { $1 }

opclass_exponential :: { Expression }
    : '^' { mkExpVarInfixOp $1 }

opclass_prefix_unary :: { Expression }
    : prefix_unary_op { mkExpVarPrefixOp $1 }

prefix_unary_op
    : '++' { $1 }
    | '--' { $1 }
    | '-' { $1 }
    | '!'  { $1 }

opclass_postfix_unary :: { Expression }
    : prefix_unary_op { mkExpVarPostfixOp $1 }

postfix_unary_op
    : '++' { $1 }
    | '--' { $1 }

field_defs :: { [(Name, Expression)] }
field_defs : {- epsilon -} { [] }
           | field_defs1   { $1 } 


field_defs1 :: { [(Name, Expression)] }
field_defs1 : field_def                { [$1] }
            | field_def ',' field_defs { $1 : $3 }


field_def :: { (Name, Expression) }
field_def
    : ID '=' expression
        { (tspIdName $1, $3) }


declarations :: { [Declaration] }
declarations
    : declaration
        { [$1] } 
    | declaration ';' declarations
        { $1 : $3 }


declaration :: { Declaration }
declaration
    : CONST ID ':' type_denoter '=' expression
        { DeclConst {dcConst = tspIdName $2, dcType = $4, dcVal = $6,
                     declSrcPos = $1} }
    | VAR ID ':' type_denoter
        { DeclVar {dvVar = tspIdName $2, dvType = $4, dvMbVal = Nothing,
          declSrcPos = $1} }
    | VAR ID ':' type_denoter ':=' expression
        { DeclVar {dvVar = tspIdName $2, dvType = $4, dvMbVal = Just $6,
          declSrcPos = $1} }
    | opt_ovrld FUN ID '(' arg_decls ')' ':' type_denoter '=' expression
        { DeclFun {dfFun = tspIdName $3, dfOvrld = isJust $1,
                   dfArgDecls = $5, dfType = $8, dfBody = $10,
                   declSrcPos = maybe $2 id $1} }
    | opt_ovrld PROC ID '(' arg_decls ')' command
        { DeclProc {dpProc = tspIdName $3, dpOvrld = isJust $1,
                    dpArgDecls = $5, dpBody = $7,
                    declSrcPos = maybe $2 id $1} }


-- Rather than the semantic value being simply a Boolean, we keep track of any
-- source code position to enable more accurate source code position tracking
-- in higher-level productions.
opt_ovrld :: { Maybe SrcPos }
opt_ovrld
    : OVERLOADED
        { Just $1 }
    | {- epsilon -}
        { Nothing }


arg_decls :: { [ArgDecl] }
arg_decls
    : {- epsilon -}
        { [] }
    | arg_decls1
        { $1 }


arg_decls1 :: { [ArgDecl] }
arg_decls1
    : arg_decl
        { [$1] }
    | arg_decl ',' arg_decls1
        { $1 : $3 }


arg_decl :: { ArgDecl }
arg_decl
    : ID ':' type_denoter
        { ArgDecl {adArg = tspIdName $1, adArgMode = ByValue, adType = $3,
                   adSrcPos = tspSrcPos $1} }
    | IN ID ':' type_denoter
        { ArgDecl {adArg = tspIdName $2, adArgMode = ByRefIn, adType = $4,
                   adSrcPos = $1} }
    | OUT ID ':' type_denoter
        { ArgDecl {adArg = tspIdName $2, adArgMode = ByRefOut, adType = $4,
                   adSrcPos = $1} }
    | VAR ID ':' type_denoter
        { ArgDecl {adArg = tspIdName $2, adArgMode = ByRefVar, adType = $4,
                   adSrcPos = $1} }


type_denoter :: { TypeDenoter }
type_denoter
    : ID
        { TDBaseType { tdbtName = tspIdName $1, tdSrcPos = tspSrcPos $1 } }
    | type_denoter '[' LITINT ']'
        { TDArray { tdaEltType = $1, tdaSize = tspLIVal $3,
                    tdSrcPos = srcPos $1 } }
    | '{' field_types '}'
        { TDRecord { tdrFldTypes = $2, tdSrcPos = $1 } }


field_types :: { [(Name, TypeDenoter)] }
field_types : {- epsilon -} { [] }
            | field_types1   { $1 } 


field_types1 :: { [(Name, TypeDenoter)] }
field_types1 : field_type                { [$1] }
             | field_type ',' field_types1 { $1 : $3 }


field_type :: { (Name, TypeDenoter) }
field_type
    : ID ':' type_denoter
        { (tspIdName $1, $3) }


{

happyError :: P a
happyError = failP "Parse error"


-- | Parses a MiniTriangle program, building an AST representation of it
-- if successful.

parse :: String -> DF AST
parse = runP parseAux


-- Projection functions for pairs of token and source position.

tspSrcPos :: (Token,SrcPos) -> SrcPos
tspSrcPos = snd


tspLIVal :: (Token,SrcPos) -> Integer
tspLIVal (LitInt {liVal = n}, _) = n
tspLIVal _ = parserErr "tspLIVal" "Not a LitInt"


tspLCVal :: (Token,SrcPos) -> Char
tspLCVal (LitChr {lcVal = c}, _) = c
tspLCVal _ = parserErr "tspLCVal" "Not a LitChr"


tspIdName :: (Token,SrcPos) -> Name
tspIdName (Id {idName = nm}, _) = nm
tspIdName _ = parserErr "tspIdName" "Not an Id"


tspOpName :: (Token,SrcPos) -> Name
tspOpName (Op {opName = nm}, _) = nm
tspOpName _ = parserErr "tspOpName" "Not an Op"


-- Helper functions for building ASTs.

-- Builds ExpVar from pair of infix binary operator Token and SrcPos.
-- Note: The operator naming convention of indicating the argument positions
-- with an underscore means that there is no confusion between infix, prefix,
-- and postfix operators even if the same operator token is used to refer
-- to them.
mkExpVarInfixOp :: (Token,SrcPos) -> Expression
mkExpVarInfixOp otsp =
    ExpVar {evVar = "_" ++ tspOpName otsp ++ "_", expSrcPos = tspSrcPos otsp}


-- Builds ExpVar from pair of prefix unary operator Token and SrcPos.
mkExpVarPrefixOp :: (Token,SrcPos) -> Expression
mkExpVarPrefixOp otsp =
    ExpVar {evVar = tspOpName otsp ++ "_", expSrcPos = tspSrcPos otsp}


-- Builds ExpVar from pair of post unary operator Token and SrcPos.
mkExpVarPostfixOp :: (Token,SrcPos) -> Expression
mkExpVarPostfixOp otsp =
    ExpVar {evVar = "_" ++ tspOpName otsp, expSrcPos = tspSrcPos otsp}


-- | Test utility. Attempts to parse the given string input and,
-- if successful, pretty-prints the resulting AST.

testParser :: String -> IO ()
testParser s = do
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) (snd result)
    putStrLn ""
    case fst result of
        Just ast -> do
                        putStrLn "AST:"
                        putStrLn (ppAST ast)
        Nothing -> putStrLn "Parsing produced no result."
    putStrLn ""
    where
        result :: (Maybe AST, [DMsg])
        result = runDF (parse s)


parserErr :: String -> String -> a
parserErr = internalError "Parser"

}
