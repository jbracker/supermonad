{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         PPTAMCode                                            *
*       Purpose:        Simple pretty printer for TAM code                   *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2013                  *
*                                                                            *
******************************************************************************
-}

-- | Simple pretty printer for TAM Code.

module PPTAMCode (
    ppTAMCode,          -- [TAMInst] -> String
    ppTAMInstState      -- TAMInst -> [MTInt] -> String
) where

-- HMTC module imports
import PPUtilities
import Name
import TAMCode


-- Column widths
cwLbl     = 8
cwOpCode  = 12
cwArgs    = 20
cwState   = 39


------------------------------------------------------------------------------
-- Pretty printing of TAM code (sequence of TAM instructions)
------------------------------------------------------------------------------

-- | Converts TAM code to a nicely laid-out textual representation for
-- display purposes.

ppTAMCode :: [TAMInst] -> String
ppTAMCode is = (foldr (.) id (map (\i -> ppTAMInst i . nl) is)) ""


------------------------------------------------------------------------------
-- Pretty printing of TAM instruction and TAM state (i.e., TAM stack)
------------------------------------------------------------------------------

-- | Converts single TAM instruction and TAM state to a nicely laid-out textual
-- representation for display purposes.

ppTAMInstState :: TAMInst -> [MTInt] -> String
ppTAMInstState i@(Label _) _  = (ppTAMInst i . nl) ""
ppTAMInstState i           ns = (ppTAMInst i . ppState ns . nl) ""


ppState :: [MTInt] -> ShowS
ppState [] = showString "[]"
ppState (n:ns) = showString s . ppsAux rw ns
    where
        s  = "[" ++ show n
        rw = cwState - length s

        ppsAux rw []     = showString "]"
        ppsAux rw (n:ns)
            | rw' >= tsrw = showString s . ppsAux rw' ns
            | otherwise   = showString tsr
            where
                s   = ", " ++ show n
                rw' = rw - length s

                -- Representation of truncated state
                tsr = ", ...]"
                tsrw = length tsr


------------------------------------------------------------------------------
-- Pretty printing of TAM instruction
------------------------------------------------------------------------------

-- | Converts single TAM instruction to a nicely laid-out textual
-- representation for display purposes.

ppTAMInst :: TAMInst -> ShowS
ppTAMInst (Label l)    = showString l . showString ":"
ppTAMInst (LOADL n)    = ppOA "LOADL"    (show n)
ppTAMInst (LOADCA l)   = ppOA "LOADCA"   l
ppTAMInst (LOAD a)     = ppOA "LOAD"     (fmtAddr a)
ppTAMInst (LOADA a)    = ppOA "LOADA"    (fmtAddr a)
ppTAMInst (LOADI d)    = ppOA "LOADI"    (show d)
ppTAMInst (STORE a)    = ppOA "STORE"    (fmtAddr a)
ppTAMInst (STOREI d)   = ppOA "STOREI"   (show d)
ppTAMInst (LOADLB m n) = ppOA "LOADLB"   (show m ++ " " ++ show n)
ppTAMInst (LOADIB n)   = ppOA "LOADIB"   (show n)
ppTAMInst (STOREIB n)  = ppOA "STOREIB"  (show n)
ppTAMInst (POP m n)    = ppOA "POP"      (show m ++ " " ++ show n)
ppTAMInst ADD          = ppOA "ADD"      ""
ppTAMInst SUB          = ppOA "SUB"      ""
ppTAMInst MUL          = ppOA "MUL"      ""
ppTAMInst DIV          = ppOA "DIV"      ""
ppTAMInst NEG          = ppOA "NEG"      ""
ppTAMInst LSS          = ppOA "LSS"      ""
ppTAMInst EQL          = ppOA "EQL"      ""
ppTAMInst GTR          = ppOA "GTR"      ""
ppTAMInst AND          = ppOA "AND"      ""
ppTAMInst OR           = ppOA "OR"       ""
ppTAMInst NOT          = ppOA "NOT"      ""
ppTAMInst (JUMP l)     = ppOA "JUMP"     l
ppTAMInst (JUMPIFZ l)  = ppOA "JUMPIFZ"  l
ppTAMInst (JUMPIFNZ l) = ppOA "JUMPIFNZ" l
ppTAMInst (CALL l)     = ppOA "CALL"     l
ppTAMInst CALLI        = ppOA "CALLI"    ""
ppTAMInst (RETURN m n) = ppOA "RETURN"   (show m ++ " " ++ show n)
ppTAMInst PUTINT       = ppOA "PUTINT"   ""
ppTAMInst PUTCHR       = ppOA "PUTCHR"   ""
ppTAMInst GETINT       = ppOA "GETINT"   ""
ppTAMInst GETCHR       = ppOA "GETCHR"   ""
ppTAMInst HALT         = ppOA "HALT"     ""

ppOA oc args = spcs cwLbl . leftJust cwOpCode oc . leftJust cwArgs args


fmtAddr :: Addr -> String
fmtAddr (SB d) = "[SB "   ++ fmtDisp d ++ "]"
fmtAddr (LB d) = "[LB "   ++ fmtDisp d ++ "]"
fmtAddr (ST d) = "[ST "   ++ fmtDisp d ++ "]"


fmtDisp :: MTInt -> String
-- Note that in twos complement, abs of the most negative number is identity...
fmtDisp d | d >= 0    = "+ " ++ show d
fmtDisp d             = "- " ++ show (abs ((fromIntegral d) :: Integer))
