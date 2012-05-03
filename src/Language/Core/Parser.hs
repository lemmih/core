{-# LANGUAGE NoMonomorphismRestriction, PatternGuards #-}
module Language.Core.Parser
    ( parseModule, lexer
    ) where

import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec
import Control.Monad
import Debug.Trace

import Language.Core.Syntax
import Language.Core.Pretty

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

type LParser = GenParser (Pos Token) ()

data Pos a = Pos !Int !Int a deriving Show

getToken (Pos _ _ token) = token
mkPos line col token = Pos (fromIntegral line) (fromIntegral col) token

data Token
    = Keyword String
    | Colon
    | DColon
    | SColon
    | Arrow
    | LParen
    | RParen
    | LBrace
    | RBrace
    | Dot
    | Equal
    | At
    | Star
    | Hash
    | Dash
    | Percent
    | QuestionMark
    | Backslash
    | Uname L.ByteString
    | Lname L.ByteString
    | String String
    | Char Char
    | Number Integer
    deriving (Show, Eq)

lexer :: L.ByteString -> [Pos Token]
lexer = worker 1 0
    where worker line col inp
              = case L.uncons inp of
                  Nothing -> []
                  Just ('\n',cs) -> worker (line+1) 0 cs
                  Just ('"',cs)  -> let (str,len,rest) = lex_string cs
                                    in mkPos line col (String str) : worker line (col+len+2) rest
                  Just ('\'',cs) -> let (char, len, rest) = lex_char cs
                                    in mkPos line col (Char char) : worker line (col+len+2) (L.drop 1 rest)
                  Just (c,cs)
                      | isSpace c -> worker line (col+1) cs
                      | Just (token,len,rest) <- findSymbol inp
                        -> mkPos line col token : worker line (col+len) rest
                      | isLetter c -> let (name, rest) = L.span isNameChar inp
                                          token = if isUpper c then Uname name
                                                               else Lname name
                                      in mkPos line col token : worker line (col+L.length name) rest
                      | isDigit c  -> let (digits, rest) = L.span isDigit inp
                                          token = Number (read (L.unpack digits))
                                      in mkPos line col token : worker line (col+L.length digits) rest
                      | otherwise -> worker line (col+1) cs -- error $ "Unhandled: " ++ take 20 (L.unpack inp)

lex_string = worker 0 ""
    where worker len acc inp
              = case L.uncons inp of
                  Nothing -> error "lexer failure in string: eof"
                  Just ('"',cs) -> (reverse acc, len, cs)
                  _ -> let (c, l, cs) = lex_char inp
                       in worker (len+l) (c:acc) cs

lex_char inp = case L.uncons inp of
                 Just ('\\',cs) -> case L.unpack (L.take 3 cs) of
                                     ['x',ah,al] | all isHexDigit [ah,al]
                                         -> (chr (digitToInt ah*16 + digitToInt al), 4, L.drop 3 cs)
                                     _ -> error "lexer failure in string: bad hex"
                 Just (c,cs) | c >= '\x20' && c <= '\x7E' && c `notElem` ['\x22', '\x27', '\x5C']
                      -> (c, 1, cs)
                 Just (c,cs) -> error $ "lexer failure in string: invalid char: " ++ show c


isNameChar c = isUpper c || isLower c || isDigit c || c == '\''


findSymbol inp = worker symbols
    where worker [] = Nothing
          worker ((str,token):xs) = if str `L.isPrefixOf` inp
                                       then Just (token, L.length str, L.drop (L.length str) inp)
                                       else worker xs

symbols = [ (L.pack "::", DColon)
          , (L.pack ":", Colon)
          , (L.pack ";", SColon)
          , (L.pack "(", LParen)
          , (L.pack ")", RParen)
          , (L.pack "{", LBrace)
          , (L.pack "}", RBrace)
          , (L.pack "->", Arrow)
          , (L.pack ".", Dot)
          , (L.pack "=", Equal)
          , (L.pack "@", At)
          , (L.pack "*", Star)
          , (L.pack "?", QuestionMark)
          , (L.pack "#", Hash)
          , (L.pack "-", Dash)
          , (L.pack "\\", Backslash)
          , (L.pack "%module", Keyword "module")
          , (L.pack "%data", Keyword "data")
          , (L.pack "%newtype", Keyword "newtype")
          , (L.pack "%rec", Keyword "rec")
          , (L.pack "%case", Keyword "case")
          , (L.pack "%forall", Keyword "forall")
          , (L.pack "%let", Keyword "let")
          , (L.pack "%note", Keyword "note")
          , (L.pack "%external", Keyword "external")
          , (L.pack "%dynexternal", Keyword "dynexternal")
          , (L.pack "%label", Keyword "label")
          , (L.pack "%_", Keyword "_")
          , (L.pack "%cast", Keyword "cast")
          , (L.pack "%left", Keyword "left")
          , (L.pack "%right", Keyword "right")
          , (L.pack "%sym", Keyword "sym")
          , (L.pack "%unsafe", Keyword "unsafe")
          , (L.pack "%inst", Keyword "inst")
          , (L.pack "%trans", Keyword "trans")
          , (L.pack "%unsafe", Keyword "unsafe")
          , (L.pack "%of", Keyword "of")
          , (L.pack "%in", Keyword "in")
          , (L.pack "%", Percent)
          ]

parseModule :: SourceName -> L.ByteString -> Either ParseError Module
parseModule src inp = runParser moduleP () src (lexer inp)

moduleP :: LParser Module
moduleP = do keyword "module"
             pkg <- pkgname
             matchToken Colon
             modName <- mident
             --trace (L.unpack modName) $ return ()
             tdefs <- tdef `endBy` (matchToken SColon)
             vdefgs <- vdefg `endBy` (matchToken SColon)
             return $ Module (pkg,modName) tdefs vdefgs


vdefg = choice
        [ do keyword "rec"
             braces $ liftM Rec (vdef `sepBy` (matchToken SColon))
        , do v <- vdef
             return $ Nonrec v
        ] <?> "vdefg"

vdef = do name <- try qvar <|> do name <- lname; return (L.empty,L.empty,name)
          matchToken DColon
          t <- ty
          matchToken Equal
          e <- expP
          return $ Vdef { vdefLocal = False
                        , vdefName  = name
                        , vdefType  = t
                        , vdefExp   = e }
       <?> "vdef"

expP = choice
       [ do fn <- aexp
            args <- many arg
            let app e (Left t)  = Appt e t
                app e (Right a) = App e a
            return $ foldl app fn args
       , do matchToken Backslash
            binds <- many1 binder
            matchToken Arrow
            e <- expP
            let lam (Left t) e = Lamt t e
                lam (Right a) e = Lam a e
            return $ foldr lam e binds
       , do keyword "case"
            t <- aty
            e <- expP
            keyword "of"
            b <- vbind
            alts <- braces $ alt `sepBy1` (matchToken SColon)
            return $ Case e b t alts
       , do keyword "cast"
            e <- aexp
            t <- aty
            return $ Cast e t
       , do keyword "let"
            def <- vdefg
            keyword "in"
            e <- expP
            return $ Let def e
       , do keyword "note"
            note <- stringP
            e <- expP
            return $ Note note e
       , do keyword "external"
            conv <- lname
            target <- stringP
            t <- aty
            return $ External target (L.unpack conv) t
       , do keyword "dynexternal"
            conv <- lname
            t <- aty
            return $ DynExternal (L.unpack conv) t
       , do keyword "label"
            str <- stringP
            return $ Label str
       ]

alt = choice [ do con <- qdcon
                  tbinds <- many (matchToken At >> tbind)
                  vbinds <- many vbind
                  matchToken Arrow
                  e <- expP
                  return $ Acon con tbinds vbinds e
             , do keyword "_"
                  matchToken Arrow
                  e <- expP
                  return $ Adefault e
             , do l <- lit
                  matchToken Arrow
                  e <- expP
                  return $ Alit l e
             ] <?> "alt"

binder = choice
         [ do matchToken At
              liftM Left tbind
         , liftM Right vbind
         ] <?> "binder"

arg = choice
      [ do matchToken At
           liftM Left aty
      , liftM Right aexp
      ] <?> "arg"

aexp = choice
       [ try $ liftM Dcon qdcon
       , try $ liftM Var qvar
       , try $ liftM Var (lname >>= \name -> return (L.empty,L.empty,name))
       , try $ liftM Lit lit
       , parens expP
       ]

lit = choice
      [ try $ parens $
        do cs <- stringP
           matchToken DColon
           t <- ty
           return (Lstring cs t)
      , try $ parens $
        do c <- charP
           matchToken DColon
           t <- ty
           return (Lchar c t)
      , try $ parens $
        do m <- option id (matchToken Dash >> return negate)
           ds <- number
           matchToken DColon
           t <- ty
           return (Lint (m ds) t)
      , try $ parens $
        do n <- number <|> parens (matchToken Dash >> liftM negate number)
           matchToken Percent
           d <- number
           matchToken DColon
           t <- ty
           return $ Lrational (n % d) t
      ]



tdef = choice [dataP, newtypeP]

dataP = do keyword "data"
           name <- qtycon
           tbinds <- many tbind
           matchToken Equal
           cdefs <- braces $ cdef `sepBy` matchToken SColon
           return $ Data name tbinds cdefs
        <?> "dataP"

newtypeP = do keyword "newtype"
              name <- qtycon
              coercion <- qtycon
              tbinds <- many tbind
              matchToken Equal
              t <- ty
              return $ Newtype name coercion tbinds t

cdef = do name <- qdcon
          tbinds <- many (matchToken At >> tbind)
          tys <- many aty
          return $ Constr name tbinds tys

aty = choice
      [ try $ liftM Tcon qtycon
      , liftM Tvar tyvar
      , parens ty
      , do keyword "trans"
           liftM2 TransCoercion aty aty
      , do keyword "sym"
           liftM SymCoercion aty
      , do keyword "right"
           liftM RightCoercion aty
      , do keyword "left"
           liftM LeftCoercion aty
      , do keyword "unsafe"
           liftM2 UnsafeCoercion aty aty
      , do keyword "inst"
           liftM2 InstCoercion aty aty
      ]

bty = do ts <- many1 aty
         return $ foldl1 Tapp ts

ty = choice
     [ do keyword "forall"
          binds <- many1 tbind
          matchToken Dot
          t <- ty
          return $ foldr Tforall t binds
     , try $ do a <- bty
                matchToken Arrow
                b <- ty
                return (Tarrow a b)
     , bty
     ]


tbind = choice
        [ do var <-tyvar
             return $ (var, Klifted)
        , parens $
          do var <- tyvar
             matchToken DColon
             k <- kind
             return (var, k)
        ]

vbind = parens $ do v <- lname
                    matchToken DColon
                    t <- ty
                    return ((L.empty,L.empty,v),t)


akind = choice [ matchToken Star >> return Klifted
               , matchToken Hash >> return Kunlifted
               , matchToken QuestionMark >> return Kopen
               , parens $ kind ]

kind = choice [ try $ do atomic <- akind
                         matchToken Arrow
                         k <- kind
                         return (Karrow atomic k)
              , try $ do a <- parens ty
                         matchToken Colon
                         matchToken Equal
                         matchToken Colon
                         b <- parens ty
                         return $ Keq a b
              , akind ]

tyvar = lname

qdcon  = qual uname
qtycon = qual (uname <|> lname)
qvar   = qual lname

qual a = do pkg <- pkgname
            matchToken Colon
            mod <- uname
            matchToken Dot
            t <- a
            return (pkg, mod, t)

tycon = uname

pkgname = uname <|> lname

mident = uname

--namechar = lower <|> upper <|> digit <|> char '\''


movePos p (Pos l c _) _ = setSourceLine (setSourceColumn p c) l
uname = tokenPrim (const "uname") movePos (\t -> case getToken t of Uname n -> Just n; _ -> Nothing)
lname = tokenPrim (const "lname") movePos (\t -> case getToken t of Lname n -> Just n; _ -> Nothing)
number = tokenPrim (const "number") movePos (\t -> case getToken t of Number n -> Just n; _ -> Nothing)
stringP = tokenPrim (const "string") movePos (\t -> case getToken t of String n -> Just n; _ -> Nothing)
charP = tokenPrim (const "char") movePos (\t -> case getToken t of Char n -> Just n; _ -> Nothing)

matchToken token
    = tokenPrim (const $ show token) movePos (\t -> if getToken t == token then Just () else Nothing)


-- Utilities
parens = between (matchToken LParen)
                 (matchToken RParen)

braces = between (matchToken LBrace)
                 (matchToken RBrace)

keyword txt = matchToken (Keyword txt)

