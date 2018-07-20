{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------
-- Module for parsing
----------------------------------------------------------------

module TextTokenize.Parser
    ( TokenizeProps (..)
    , Token         (..)
    , TypeBlock     (..)
    , Tokens

    , space

    , defaultTokenizeProps
    , tokenize

    , Crumb (..)    -- TEMP: Only for test 
    , Crumbs        -- TEMP: Only for test
    , recCrumbs     -- TEMP: Only for test
    , recCrumbsN    -- TEMP: Only for test
    ) where



-- Import of modules
import           Prelude                 as PRL
import           Data.Text               as T



-- | Properties for parsing
data TokenizeProps 
    = TokenizeAtom 
        { tp_splits  :: [Text]           -- Array of delimiters for atoms
        , tp_start   :: Maybe [Text]     -- Filter for atoms token by array of prefix text (OR)
        , tp_clean   :: Bool             -- Clearing from empty tokens
        }
    | TokenizeBlock 
        { tp_delmits :: [(Text,Text)]    -- Array of delimiters for blocks (open block, close block)
        , tp_start   :: Maybe [Text]     -- Filter for blocks token by array of prefix text (OR)
        , tp_clean   :: Bool             -- Clearing from empty tokens
        }



-- | Data for token after parsing
data Token 
    = TokenEmpty
    | TokenAtom Text
    | TokenBlock TypeBlock Text
    | TokenOther Text
    deriving (Show, Eq)

-- | Data of type block for token
data TypeBlock 
    = TBBody
    | TBDelm
    deriving (Show, Eq)


-- | Many tokens
type Tokens = [Token]



space :: Text
space = " "



-- | Default properties for parsing
defaultTokenizeProps :: TokenizeProps 
defaultTokenizeProps = TokenizeAtom
    { tp_splits = [" "]
    , tp_start  = Nothing
    , tp_clean  = True
    }



-- | Parsing according properties
tokenize :: TokenizeProps 
         -> Text 
         -> Tokens
tokenize (TokenizeAtom ss str cln) text =
    let lM = \v -> if v == "" || recIsStart str v == False then TokenEmpty else TokenAtom v
        lF = \v -> if v == TokenEmpty                      then False      else True
    in
    case cln of
        False -> PRL.map    lM $ recAtom ss [text]
        True  -> PRL.filter lF $ PRL.map lM $ recAtom ss [text]
tokenize (TokenizeBlock dlms str cln) text =
    PRL.map lM $ recCrumbs (masDlms dlms) [text]
    where
        masDlms :: [(Text, Text)] -> [Text]
        masDlms dlms =
            let (p1, p2) = PRL.unzip dlms in p1 ++ p2
        lM :: Crumb -> Token
        lM (TCrBody v) = TokenBlock TBBody v
        lM (TCrDelm v) = TokenBlock TBDelm v



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




