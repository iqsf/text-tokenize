{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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

    , ABSec (..)    -- TEMP
    ) where



-- Import of modules
import           Prelude                 as PRL
import           Data.Text               as T



class CTokenize p where
    type ResToken p
    tokenize :: p -> Text -> ResToken p



-- | Properties for parsing
data TokenizeAtom 
    = TokenizeAtom 
        { ta_splits  :: [Text]           -- Array of delimiters for atoms
        , ta_start   :: Maybe [Text]     -- Filter for atoms token by array of prefix text (OR)
        , ta_clean   :: Bool             -- Clearing from empty tokens
        }

data TokenizeAtomDm 
    = TokenizeAtomDm 
        { tad_splits  :: [Text]          -- Array of delimiters for blocks (open block, close block)
        , tad_start   :: Maybe [Text]    -- Filter for blocks token by array of prefix text (OR)
        , tad_clean   :: Bool            -- Clearing from empty tokens
        }

data TokenizeBlock 
    = TokenizeBlock 
        { tb_block   :: (Text, Text)     -- Defining block boundaries
        , tb_splits  :: [Text]    -- Array of delimiters for blocks (open block, close block)
        , tb_start   :: Maybe [Text]     -- Filter for blocks token by array of prefix text (OR)
        , tb_clean   :: Bool             -- Clearing from empty tokens
        }



-- | Data for token after parsing
data TokenAtom   = TokenEmpty      | TokenAtom      Text                        deriving (Show, Eq)
data TokenAtomDm = TokenADMEmpty   | TokenADMBody   Text | TokenADMDelm    Text deriving (Show, Eq)
data TokenBlock  = TokenBlockEmpty | TokenBlockBody Text | TokenBlockOther Text deriving (Show, Eq)
--data TokenBlock  = TokenBlockEmpty | TokenBlockBody Text | TokenBlockDelm Text | TokenBlockOther Text deriving (Show, Eq)
data TokenOther  =                   TokenOther     Text                        deriving (Show, Eq)



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



space :: Text
space = " "



-- | Default properties for parsing (TokenizeAtom)
defaultTokenizeAtom :: TokenizeAtom
defaultTokenizeAtom = TokenizeAtom
    { ta_splits = [" "]
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
    tokenize (TokenizeAtom ss str cln) text =
        let lM = \v -> if v == "" || recIsStart str v == False then TokenEmpty else TokenAtom v
            lF = \v -> if v == TokenEmpty                      then False      else True
        in
        case cln of
            False -> PRL.map    lM $ recAtom ss [text]
            True  -> PRL.filter lF $ PRL.map lM $ recAtom ss [text]



-- | Parsing according properties (TokenizeAtomDm)
instance CTokenize TokenizeAtomDm where
    type ResToken TokenizeAtomDm = [TokenAtomDm]
    tokenize (TokenizeAtomDm dlms str cln) text =
        PRL.map (lM str) $ recCrumbs dlms [text]
        where
            lM :: Maybe [Text] -> Crumb -> TokenAtomDm
            lM str (TCrBody v) = if v == "" || recIsStart str v == False then TokenADMEmpty else TokenADMBody v
            lM _   (TCrDelm v) = TokenADMDelm v



-- | Parsing according properties (TokenizeBlock)
instance CTokenize TokenizeBlock where
    type ResToken TokenizeBlock = [TokenBlock]
    --type ResToken TokenizeBlock = [ABSec]
    tokenize tzb@(TokenizeBlock blc dlms str cln) text =
        --recBlock tzb (tokenize (TokenizeAtomDm dlms str cln) text) BBLeft defABSec []
        genBlocks tzb $ recBlock tzb (tokenize (TokenizeAtomDm dlms str cln) text) BBLeft defABSec []



data BBState = BBLeft | BBRight

data ABSec 
    = ABSec
        { abs_array :: [TokenAtomDm]
        }
        deriving (Show, Eq)


defABSec :: ABSec
defABSec = 
    ABSec {abs_array = []}

--unionABSec :: ABSec
--           -> Text
--unionABSec (ABSec a) = 
--    recU a
--    where
--        recU :: [TokenAtomDm] -> [Text]
--        recU [] = ""
--        recU (x:xs) = 
--            case x of
--                TokenADMEmpty       -> "" ++ (recU xs)
--                (TokenADMBody v)    -> v  ++ (recU xs)
--                (TokenADMDelm v)    -> v  ++ (recU xs)

                

recBlock :: TokenizeBlock 
         -> [TokenAtomDm]
         -> BBState
         -> ABSec
         -> [ABSec]
         -> [ABSec]
--recBlock _ _ _ _ = []
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
    (PRL.map lM aa) ++ (genBlocks tzb xs)
    where
        lM :: TokenAtomDm -> TokenBlock
        lM TokenADMEmpty    = TokenBlockEmpty
        lM (TokenADMBody v) = TokenBlockOther v 
        lM (TokenADMDelm v) = TokenBlockOther v
    



-----------------------------------------------------------------------------------------------
-- Auxiliary functions   ----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

recAtom :: [Text] 
        -> [Text] 
        -> [Text]
recAtom [] texts    = texts
recAtom (x:xs) texts =  
    recAtom xs (recAtomN x texts)

recAtomN :: Text 
         -> [Text] 
         -> [Text]
recAtomN _  []     = []  
recAtomN ss (x:xs) =  
    (T.splitOn ss x) ++ (recAtomN ss xs)



recIsStart :: Maybe [Text] 
           -> Text 
           -> Bool
recIsStart Nothing  _    = 
    True
recIsStart (Just p) text =
    recIsStartN p text
    where
        recIsStartN []     _    = False
        recIsStartN (x:xs) text =
            if T.isPrefixOf x text == True 
            then True 
            else recIsStartN xs text



data Crumb 
    = TCrBody Text
    | TCrDelm Text
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

recCrumbs :: [Text] 
          -> [Text]
          -> Crumbs
recCrumbs ss texts =
    let textsTCr = PRL.map (\v -> TCrBody v) texts
        lF       = \v -> not $ isEmptyTCr v
    in PRL.filter lF $ recCrumbsI ss textsTCr

recCrumbsI :: [Text] 
           -> Crumbs
           -> Crumbs
recCrumbsI [] texts    = texts
recCrumbsI (x:xs) texts =  
    recCrumbsI xs (recCrumbsN x texts)

recCrumbsN :: Text 
           -> Crumbs 
           -> Crumbs
recCrumbsN _  []     = []  
recCrumbsN ss ((TCrDelm x):xs) =
    [TCrDelm x] ++ recCrumbsN ss xs
recCrumbsN ss ((TCrBody x):xs) = 
    (recCN ss x) ++ (recCrumbsN ss xs)
    where
        recCN :: Text -> Text -> Crumbs
        recCN s t =
            if   T.isInfixOf s t
            then let (l,r) = T.breakOn s t
                     ls    = T.length  s
                 in [TCrBody l, TCrDelm s] ++ recCN s (T.drop ls r)
            else [TCrBody t]





