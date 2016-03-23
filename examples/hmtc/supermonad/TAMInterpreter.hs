{-
******************************************************************************
*                                  H M T C                                   *
*                                                                            *
*       Module:         TAMInterpreter                                       *
*       Purpose:        Triangle Abstract Machine (TAM) Interpreter          *
*       Authors:        Henrik Nilsson                                       *
*                                                                            *
*                 Copyright (c) Henrik Nilsson, 2006 - 2013                  *
*                                                                            *
******************************************************************************
-}

-- | Triangle Abstract Machine (TAM) Interpreter.

module TAMInterpreter (
    runTAM      -- :: Bool -> [TAMInst] -> IO ()
) where

-- Standard library imports
import Prelude hiding (take, drop, (!!))
import qualified Prelude as P (take, drop, (!!))
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Array

-- HMTC module imports
import Name
import TAMCode
import PPTAMCode


-- TAM state (the code is fixed).

data TAMState = TAMState {
    tsCode :: Array MTInt TAMInst,      -- TAM Code
    tsCdSz :: MTInt,    -- Code size
    tsPC   :: MTInt,    -- PC (Program Counter): 0 <= tsPC <= tsCdSz
    tsLB   :: MTInt,    -- LB (Local Base): 0 <= tsLB <= tsST
    tsST   :: MTInt,    -- ST (Stack Top): 0 <= tsST <= stkSz
    tsStk  :: [MTInt]   -- Stack: length tsStk <= stkSz
}


-- TAM stack limit. Naive impl., so a large stack would be very inefficient.
-- Invariant: 0 <= tsST <= stkSz <= maxBound :: Int

-- Note that the maximal size of the stack actually is limited by the size of
-- Haskell Ints (which might be only 30 bits) as the list operations such as
-- "take", "drop", "!!", use an Int count. This is unlikely to be a problem,
-- though: we'd need a better representation of the stack anyway to handle
-- such large stacks reasonably efficiently even assuming enough memory in
-- the first place.

stkSz :: MTInt
stkSz = 10000


-- | Run TAM code (interpreter, naive). Arguments:
--
-- (1) Tracing on (True) or off (False)
--
-- (2) The TAM code to run

runTAM :: Bool -> [TAMInst] -> IO ()
runTAM trace code = rtAux initTS
    where
        initTS = TAMState {
                     tsCode = array (0, cdSz - 1) (zip [0..] code),
                     tsCdSz = cdSz,
                     tsPC   = 0,
                     tsLB   = 0,
                     tsST   = 0,
                     tsStk  = []}

        cdSz = toMTInt (length code)

        rtAux s = do
            let (i, s') = fetchInst s
            ms'' <- execute s' i
            case ms'' of
                Nothing  -> putStrLn "TAM Halted!"
                Just s'' -> do
                    when trace (putStr (ppTAMInstState i (tsStk s'')))
                    rtAux s''


-- Idea: treat the upper stack bound as "soft", only checking if exceeded
-- at the very last, in continue. Right now, not always checked, anyway, 
-- e.g. CALL.
execute :: TAMState -> TAMInst -> IO (Maybe TAMState)
execute s@(TAMState {tsLB = lb, tsST = st, tsStk = stk}) i =
    case i of
        Label _    -> continue s
        LOADL n    -> continue (push n s)
        LOADCA l   -> continue (push (findLabel l s) s)
        LOAD a     -> readStkAddr a
        LOADA a    -> pushAddr a
        LOADI d    -> readStkInd d
        STORE a    -> writeStkAddr a
        STOREI d   -> writeStkInd d
        LOADLB m n -> pushBlk m n
        LOADIB n   -> readStkBlk n
        STOREIB n  -> writeStkBlk n
        POP m n    -> squeeze m n
        ADD        -> binOp (+)
        SUB        -> binOp (-)
        MUL        -> binOp (*)
        DIV        -> binOp (div)
        NEG        -> unOp  negate
        LSS        -> binOp (\x y -> if x < y then 1 else 0)
        EQL        -> binOp (\x y -> if x == y then 1 else 0)
        GTR        -> binOp (\x y -> if x > y then 1 else 0)
        AND        -> binOp (\x y -> if x /= 0 && y /= 0 then 1 else 0)
        OR         -> binOp (\x y -> if x /= 0 || y /= 0 then 1 else 0)
        NOT        -> unOp  (\x -> if x /= 0 then 0 else 1)
        JUMP l     -> jump l s
        JUMPIFZ l  ->
            case pop s of
                Right (x, s') -> if x == 0 then jump l s' else continue s'
                Left msg      -> abort msg
        JUMPIFNZ l ->
            case pop s of
                Right (x, s') -> if x /= 0 then jump l s' else continue s'
                Left msg      -> abort msg
        -- Activation record:
        -- LB:     Static link, not currently used
        -- LB + 1: Dynamic link (old LB)
        -- LB + 2: Return adddress (old PC)
        CALL l -> 
            continue (s {tsPC  = findLabel l s,
                         tsLB  = st,
                         tsST  = st + 3,
                         tsStk = tsPC s : lb : 0 : stk})
        CALLI ->
            case stk of
                (pc' : sl : stk') ->
                    continue (s {tsPC  = pc',
                                 tsLB  = st - 2,
                                 tsST  = st + 1,
                                 tsStk = tsPC s : lb : sl : stk'})
                _ -> abort stkUFlw
        RETURN m n ->
            if st - lb < 3 then
                abort stkARCrrpt
            else if st < m + 3 + n then
                abort stkUFlw
            else -- There is a 3-word act. record and enough elems for return 
                 continue (s {tsPC  = stk !! addrToIx (LB 2),
                              tsLB  = stk !! addrToIx (LB 1),
                              tsST  = st - 3 - n,
                              tsStk = take m stk ++ drop (m + 3 + n) stk})
        PUTINT ->
            case pop s of
                Right (n, s') -> putStrLn (show n) >> continue s' 
                Left msg      -> abort msg
        PUTCHR ->
            case pop s of
                Right (n, s') -> putChar (toEnum (fromMTInt n)) >> continue s' 
                Left msg      -> abort msg
        GETINT -> do
            putStrLn "Enter integer: "
            i <- getLine
            let i' = read ('0' : takeWhile isDigit i)
            continue (push i' s)
        GETCHR -> do
            putStrLn "Enter character: "
            c <- getChar
            continue (push (toMTInt (fromEnum c)) s)
        HALT -> return Nothing
    where
        addrToIx :: Addr -> MTInt
        addrToIx (SB d) = (st - 1) - d
        addrToIx (LB d) = (st - 1) - (lb + d)
        addrToIx (ST d) = -(1 + d)

        pushAddr :: Addr -> IO (Maybe TAMState)
        pushAddr a = continue (push (addr a) s)
            where
                addr (SB d) = d
                addr (LB d) = lb + d
                addr (ST d) = st + d

        readStkAddr :: Addr -> IO (Maybe TAMState)
        readStkAddr a = readStk 0 (addrToIx a) 1

        readStkInd :: MTInt -> IO (Maybe TAMState)
        readStkInd d =
            case stk of
                a : _ -> readStk 1 (addrToIx (SB (a + d))) 1
                _     -> abort stkUFlw

        readStkBlk :: MTInt -> IO (Maybe TAMState)
        readStkBlk n =
            case stk of
                a : _ -> readStk 1 (addrToIx (SB a)) n
                _     -> abort stkUFlw

        -- Read block of size n starting at ix i and push it discarding m elts.
        readStk :: MTInt -> MTInt -> MTInt -> IO (Maybe TAMState)
        readStk m i n
            | start < 0 || i >= st =
                abort stkDisp
            | st' > stkSz = 
                abort stkOFlw
            | otherwise =
                continue (s {tsST = st', tsStk = xs ++ drop m stk})
            where
                start = i - n + 1
                st'   = st + n - m
                xs    = take n (drop start stk)
            
{-
        readStk :: Bool -> MTInt -> IO (Maybe TAMState)
        readStk replc d
            | 0 <= d && d < st =
                if replc then
                    -- Replace top stack element; we know stack not empty!
                    continue (s {tsStk = (stk !! d) : tail stk})
                else
                    -- Push element onto stack
                    continue (push (stk !! d) s)
            | otherwise =
                abort stkDisp
-}

        writeStkAddr :: Addr -> IO (Maybe TAMState)
        writeStkAddr a =
            case stk of
                x : stk' -> writeStk 1 [x] (addrToIx a)
                _        -> abort stkUFlw

        writeStkInd :: MTInt -> IO (Maybe TAMState)
        writeStkInd d =
            case stk of
                a : x : _ -> writeStk 2 [x] (addrToIx (SB (a + d)))
                _         -> abort stkUFlw

        writeStkBlk :: MTInt -> IO (Maybe TAMState)
        writeStkBlk n
            | an <= st =
                writeStk an block (addrToIx (SB a))
            | otherwise =
                abort stkUFlw
            where
                an = 1 + n
                a : block = take an stk

        -- Write block to stack starting at ix i, discard. n top elts. afterw.
        writeStk :: MTInt -> [MTInt] -> MTInt -> IO (Maybe TAMState)
        writeStk n xs i
            | start < 0 || i >= st =
                abort stkDisp
            | st' < 0 =
                abort stkUFlw
            | otherwise =
                continue (s {tsST  = st',
                             tsStk = drop n (take start stk
                                             ++ xs
                                             ++ drop (i+1) stk)})
            where
                start = i - fromIntegral (length xs) + 1
                st'   = st - n

{-
        writeStk :: MTInt -> MTInt -> MTInt -> IO (Maybe TAMState)
        writeStk n x d
            | 0 <= d && d < st =
                continue (s {tsST  = st - n,
                             tsStk = drop n (take d stk
                                             ++ [x]
                                             ++ drop (d+1) stk)})
            | otherwise =
                  abort stkDisp
-}

        pushBlk :: MTInt -> MTInt -> IO (Maybe TAMState)
        pushBlk m n
            | st' <= stkSz =
                continue (s {tsST = st', tsStk = take n' (repeat m) ++ stk})
            | otherwise =
                abort stkOFlw
            where
                n'  = max 0 n
                st' = st + n'

        squeeze :: MTInt -> MTInt -> IO (Maybe TAMState)
        squeeze m n
            | m' + n' <= st =
                continue (s {tsST = st - n',
                             tsStk = take m' stk ++ drop (m' + n') stk})
            | otherwise = 
                abort stkUFlw
            where
                m' = max 0 m
                n' = max 0 n

        unOp :: (MTInt -> MTInt) -> IO (Maybe TAMState)
        unOp f =
            case stk of
                x : stk' -> continue (s {tsStk = f x : stk'})
                _        -> abort stkUFlw

        binOp :: (MTInt -> MTInt -> MTInt) -> IO (Maybe TAMState)
        binOp f =
            case stk of
                y : x : stk' -> continue (s {tsST  = st - 1,
                                             tsStk = f x y : stk'})
                _            -> abort stkUFlw

        jump :: Name -> TAMState -> IO (Maybe TAMState)
        jump l s = continue (s {tsPC = findLabel l s})


        continue :: TAMState -> IO (Maybe TAMState)
        continue s | tsST s <= stkSz = return (Just s)
                   | otherwise       = abort stkOFlw

        abort :: String -> IO (Maybe TAMState)
        abort msg  = putStrLn msg
                     >> putStrLn (ppTAMInstState i (tsStk s))
                     >> return Nothing


-- Push without check on upper bound.
push :: MTInt -> TAMState -> TAMState
push n s@(TAMState {tsST = st, tsStk = stk}) =
    s {tsST  = st + 1, tsStk = n : stk}


pop :: TAMState -> Either String (MTInt, TAMState)
pop s@(TAMState {tsST = st, tsStk = x : stk}) = 
    Right (x, s {tsST = st - 1, tsStk = stk})
pop _ =
    Left stkUFlw


fetchInst :: TAMState -> (TAMInst, TAMState)
fetchInst s@(TAMState {tsCode = code, tsCdSz = cdSz, tsPC = pc})
    | 0 <= pc && pc < cdSz = (code ! pc, s {tsPC = pc + 1})
    | otherwise            = (HALT, s)


-- If label not found, returns PC just beyond end of code. Attempting to
-- fetch an instruction outside the defined code segment yields HALT,
-- so machine will then stop.
findLabel :: Name -> TAMState -> MTInt
findLabel l (TAMState {tsCode = code, tsCdSz = cdSz}) = flAux 0
    where
        flAux pc
            | pc < cdSz =
                case code ! pc of
                    Label l' | l == l' -> pc
                    _                  -> flAux (pc + 1)
            | otherwise = pc


toMTInt :: Int -> MTInt
toMTInt n = fromIntegral n


fromMTInt :: MTInt -> Int
fromMTInt n = fromIntegral n


(!!) :: [a] -> MTInt -> a
xs !! n = xs P.!! (fromMTInt n)


drop :: MTInt -> [a] -> [a]
drop n xs = P.drop (fromMTInt n) xs


take :: MTInt -> [a] -> [a]
take n xs = P.take (fromMTInt n) xs


-- Error messages
stkUFlw       = "Stack underflow!"
stkOFlw       = "Stack overflow!"
stkARCrrpt    = "Activation record corrupted!"
stkDisp       = "Displacement outside stack!"


-- Tests

test1 =
    [LOADL 0,
     GETINT,
     Label "loop",
     LOAD (SB 0),
     LOADL 1,
     ADD,
     STORE (SB 0),
     LOAD (SB 0),
     LOAD (SB 1),
     CALL "geq",
     JUMPIFZ "loop",
     LOAD (SB 0),
     PUTINT,
     HALT,
     Label "geq",
     LOAD (LB (-2)),
     LOAD (LB (-1)),
     LSS,
     NOT,
     RETURN 1 2]
