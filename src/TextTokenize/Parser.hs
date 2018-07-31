{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------
-- Module for parsing
----------------------------------------------------------------

module TextTokenize.Parser
    ( CTokenize         (..) 

    , TokenizeAtom      (..)
    , TokenizeAtomDm    (..)
    , TokenizeBlock     (..)

    , TokenAtom         (..)
    , TokenAtomDm       (..)
    , TokenBlock        (..)

    , space

    , defaultTokenizeAtom
    , defaultTokenizeAtomDm
    , defaultTokenizeForString

    , recAtom
    , recCrumbs, Crumb (..)
    ) where



-- Import of modules
import           Prelude
import qualified Data.Text               as T



class CTokenize p where
    type ResToken p
    tokenize :: p -> T.Text -> ResToken p



-- | Properties for parsing
data TokenizeAtom 
    = TokenizeAtom 
        { ta_splits  :: [T.Text]         -- Array of delimiters for atoms
        , ta_start   :: Maybe [T.Text]   -- Filter for atoms token by array of prefix text (OR)
        , ta_clean   :: Bool             -- Clearing from empty tokens
        }

data TokenizeAtomDm 
    = TokenizeAtomDm 
        { tad_splits  :: [T.Text]        -- Array of delimiters for blocks (open block, close block)
        , tad_start   :: Maybe [T.Text]  -- Filter for blocks token by array of prefix text (OR)
        , tad_clean   :: Bool            -- Clearing from empty tokens
        }

data TokenizeBlock 
    = TokenizeBlock 
        { tb_block   :: (T.Text, T.Text) -- Defining block boundaries
        , tb_splits  :: [T.Text]         -- Array of delimiters for blocks (open block, close block)
        , tb_start   :: Maybe [T.Text]   -- Filter for blocks token by array of prefix text (OR)
        , tb_clean   :: Bool             -- Clearing from empty tokens
        }



-- | Data for token after parsing
data TokenAtom   = TokenEmpty      | TokenAtom      T.Text                        deriving (Show, Eq)
data TokenAtomDm = TokenADMEmpty   | TokenADMBody   T.Text | TokenADMDelm    T.Text deriving (Show, Eq)
data TokenBlock  = TokenBlockEmpty | TokenBlockBody T.Text | TokenBlockOther T.Text deriving (Show, Eq)
--data TokenBlock  = TokenBlockEmpty | TokenBlockBody T.Text | TokenBlockDelm T.Text | TokenBlockOther T.Text deriving (Show, Eq)
data TokenOther  =                   TokenOther     T.Text                        deriving (Show, Eq)



-- | Adapter class. The implementation allows you to create adapters 
-- between different types.
--class Adaptable a b | a -> b where
--    adapter :: a -> b

--instance Adaptable TokenAtomDm TokenBlock where
--    adapter TokenADMEmpty = TokenBlockEmpty
--    adapter (TokenADMBody v) = TokenBlockBody v
--    adapter (TokenADMDelm v) = TokenBlockDelm v

-- | Data of type block for token
--data TypeBlock 
--    = TBBody
--    | TBDelm
--    deriving (Show, Eq)



space :: T.Text
space = " "



-- | Default properties for parsing (TokenizeAtom)
defaultTokenizeAtom :: TokenizeAtom
defaultTokenizeAtom = TokenizeAtom
    { ta_splits = [space]
    , ta_start  = Nothing
    , ta_clean  = True
    }

-- | Default properties for parsing (TokenizeAtomDm)
defaultTokenizeAtomDm :: TokenizeAtomDm
defaultTokenizeAtomDm = TokenizeAtomDm
    { tad_splits = ["{", "}", "/*", "*/"]
    , tad_start  = Nothing
    , tad_clean  = True
    }

-- | Default properties for parsing (TokenizeBlock)
-- for String
defaultTokenizeForString :: TokenizeBlock
defaultTokenizeForString = TokenizeBlock
    { tb_block  = ("\"", "\"")
    , tb_splits = [ "\""  
                  , "\n"  
                  ]
    , tb_start  = Nothing
    , tb_clean  = True
    }



-- | Parsing according properties (TokenizeAtom)
instance CTokenize TokenizeAtom where
    type ResToken TokenizeAtom = [TokenAtom]
    tokenize (TokenizeAtom ss str cln) text
        | cln       = filter (TokenEmpty /=) tokens
        | otherwise = tokens
        where
            -- It is impossible to use пгфквы from error: In a stmt of a pattern guard 
            -- for an equation ‘wrapInToken’: recIsStart str v
            --wrapInToken v | null v             = TokenEmpty
            --              | (recIsStart str v) = TokenAtom v
            --              | otherwise          = TokenEmpty
            wrapInToken :: T.Text -> TokenAtom
            wrapInToken v = 
                if   T.null v || recIsStart str v == False 
                then TokenEmpty 
                else TokenAtom v
            tokens = map wrapInToken $ recAtom ss [text]



-- | Parsing according properties (TokenizeAtomDm)
instance CTokenize TokenizeAtomDm where
    type ResToken TokenizeAtomDm = [TokenAtomDm]
    tokenize (TokenizeAtomDm dlms str cln) text
        | cln       = filter (TokenADMEmpty /=) tokens
        | otherwise = tokens
        where
            wrapInToken :: Crumb -> TokenAtomDm
            wrapInToken (TCrBody v) = if   T.null v || recIsStart str v == False 
                                      then TokenADMEmpty 
                                      else TokenADMBody v
            wrapInToken (TCrDelm v) = TokenADMDelm v
            tokens = map wrapInToken $ recCrumbs dlms [text]



-- | Parsing according properties (TokenizeBlock)
instance CTokenize TokenizeBlock where
    type ResToken TokenizeBlock = Either T.Text [TokenBlock]
    --type ResToken TokenizeBlock = [ABSec]
    tokenize tzb@(TokenizeBlock blc dlms str cln) text =
        --recBlock tzb (tokenize (TokenizeAtomDm dlms str cln) text) BBLeft defABSec []
        case recBlock tzb (tokenize (TokenizeAtomDm dlms str cln) text) BBLeft defABSec [] of
            []  -> Left "Error parsing "
            r   -> if   (isAllOther $ bls r) == True
                   then Left "Error parsing"
                   else Right $ bls r
        where
            bls r = genBlocks tzb r
            isAllOther :: [TokenBlock] -> Bool
            isAllOther = all $ \case (TokenBlockOther _) -> True
                                     _                   -> False



-- | Type for mark for serching left and rite block boundaries
data BBState = BBLeft | BBRight

-- | Intermediate type for grouping
data ABSec 
    = ABSec
        { abs_array :: [TokenAtomDm]
        }
        deriving (Show, Eq)


defABSec :: ABSec
defABSec = 
    ABSec {abs_array = []}

                

recBlock :: TokenizeBlock 
         -> [TokenAtomDm]
         -> BBState
         -> ABSec
         -> [ABSec]
         -> [ABSec]
recBlock _ [] BBLeft cur acc =
    acc ++ [cur]
recBlock _ [] BBRight cur acc =
    []
recBlock tzb@(TokenizeBlock (d,_) _ _ _) (x:xs) BBLeft cur acc =
    case x of
        TokenADMEmpty       -> recBlock tzb xs BBLeft cur acc
        t1@(TokenADMDelm v) -> if   d == v
                               then recBlock tzb 
                                             xs 
                                             BBRight 
                                             ABSec {abs_array = [t1]}
                                             (acc ++ [cur])
                               else recBlock tzb 
                                             xs 
                                             BBLeft 
                                             cur {abs_array = (abs_array cur) ++ [t1]}
                                             acc
        t2@(TokenADMBody v) -> recBlock tzb 
                                        xs 
                                        BBLeft
                                        cur {abs_array = (abs_array cur) ++ [t2]}
                                        acc
recBlock tzb@(TokenizeBlock (_,d) _ _ _) (x:xs) BBRight cur acc =
    case x of
        TokenADMEmpty       -> recBlock tzb xs BBRight cur acc
        t1@(TokenADMDelm v) -> if   d == v
                               then recBlock tzb 
                                             xs 
                                             BBLeft 
                                             ABSec {abs_array = []}
                                             (acc ++ [cur {abs_array = (abs_array cur) ++ [t1]}])
                               else recBlock tzb 
                                             xs 
                                             BBRight 
                                             cur {abs_array = (abs_array cur) ++ [t1]}
                                             acc
        t2@(TokenADMBody v) -> recBlock tzb 
                                        xs 
                                        BBRight
                                        cur {abs_array = (abs_array cur) ++ [t2]}
                                        acc


genBlocks :: TokenizeBlock
          -> [ABSec]
          -> [TokenBlock]
genBlocks _ [] =
    []
genBlocks tzb ((ABSec []):xs) =
    genBlocks tzb xs 
genBlocks tzb@(TokenizeBlock (l,r) _ _ _) ((ABSec a@(a1:a2:a3:[])):xs) =
    case (a1,a2,a3) of
        ((TokenADMDelm l), (TokenADMBody b), (TokenADMDelm r))  -> [TokenBlockBody b] ++ (genBlocks tzb xs)
        _                                                       -> []
genBlocks tzb ((ABSec aa):xs) = 
    (map lM aa) ++ (genBlocks tzb xs)
    where
        lM :: TokenAtomDm -> TokenBlock
        lM TokenADMEmpty    = TokenBlockEmpty
        lM (TokenADMBody v) = TokenBlockOther v 
        lM (TokenADMDelm v) = TokenBlockOther v
    



-----------------------------------------------------------------------------------------------
-- Auxiliary functions   ----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

recAtom :: [T.Text] 
        -> [T.Text] 
        -> [T.Text]
recAtom [] texts    = texts
recAtom tss texts =  
    foldl recAtomN tss texts
    where
        recAtomN :: [T.Text] -> T.Text -> [T.Text]
        recAtomN ss t =
            concatMap (\v -> T.splitOn v t) ss



recIsStart :: Maybe [T.Text] 
           -> T.Text 
           -> Bool
recIsStart Nothing  _    = True
recIsStart (Just p) text = any (`T.isPrefixOf` text) p


data Crumb 
    = TCrBody T.Text
    | TCrDelm T.Text
    deriving (Show, Eq)

type Crumbs = [Crumb]


isEmptyTCr :: Crumb
           -> Bool
isEmptyTCr (TCrBody "") = 
    True
isEmptyTCr (TCrDelm "") = 
    True
isEmptyTCr _ = 
    False

recCrumbs :: [T.Text] 
          -> [T.Text]
          -> Crumbs
recCrumbs ss texts =
    filter (not . isEmptyTCr) $ recCrumbsI ss (map TCrBody texts)


recCrumbsI :: [T.Text] 
           -> Crumbs
           -> Crumbs
recCrumbsI ts texts =  
    foldl recCrumbsN texts ts

recCrumbsN :: Crumbs 
           -> T.Text 
           -> Crumbs
recCrumbsN [] _       = []  
recCrumbsN ((TCrDelm x):xs) ss =
    TCrDelm x : recCrumbsN xs ss
recCrumbsN ((TCrBody x):xs) ss = 
    (recCN ss x) ++ (recCrumbsN xs ss)
    where
        recCN :: T.Text -> T.Text -> Crumbs
        recCN s t =
            if   T.isInfixOf s t
            then let (l,r) = T.breakOn s t
                     ls    = T.length  s
                 in [TCrBody l, TCrDelm s] ++ recCN s (T.drop ls r)
            else [TCrBody t]



