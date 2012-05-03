module Language.Core.Pretty
    ( ppModule
    ) where

import Language.Core.Syntax

import Text.PrettyPrint
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L

ppModule :: Module -> Doc
ppModule (Module (pkg,mod) tdefs vdefgs)
    = keyword "module" <+> text (L.unpack pkg) <> colon <> text (L.unpack mod) $+$
      indent body
    where body = types $$ definitions
          types = endWith (char ';') $ map ppTdef tdefs
          definitions = endWith (char ';') $ map ppVdefg vdefgs

ppVdefg (Rec defs)
    = keyword "rec" $$ braces (indent (vcat (punctuate (char ';') (map ppVdef defs))))
ppVdefg (Nonrec def)
    = ppVdef def

ppVdef vdef -- (_local, name, ty, expr)
    = ppQual (vdefName vdef) <+> text "::" <+> ppTy (vdefType vdef) <+> equals $$ indent (ppExp (vdefExp vdef))

ppAexp, pfexp, ppExp :: Exp -> Doc
ppAexp (Var x) = ppQual x
ppAexp (Dcon x) = ppQual x
ppAexp (Lit l) = plit l
ppAexp e = parens(ppExp e)

plamexp :: [Either Tbind Vbind] -> Exp -> Doc
plamexp bs (Lam b e) = plamexp (bs ++ [Right b]) e
plamexp bs (Lamt b e) = plamexp (bs ++ [Left b]) e
plamexp bs e = sep [sep (map pbind bs) <+> text "->",
                    indent (ppExp e)]

pbind :: Either Tbind Vbind -> Doc
pbind (Left tb) = char '@' <+> ppTbind tb
pbind (Right vb) = pvbind vb

pfexp (App e1 e2) = pappexp e1 [Left e2]
pfexp (Appt e t) = pappexp e [Right t]
pfexp e = ppAexp e

pappexp :: Exp -> [Either Exp Ty] -> Doc
pappexp (App e1 e2) as = pappexp e1 (Left e2:as)
pappexp (Appt e t) as = pappexp e (Right t:as)
pappexp e as = fsep (ppAexp e : map pa as)
           where pa (Left e) = ppAexp e
                 pa (Right t) = char '@' <+> ppAty t

ppExp (Lam b e) = char '\\' <+> plamexp [Right b] e
ppExp (Lamt t e) = char '\\' <+> plamexp [Left t] e
ppExp (Let vd e) = (text "%let" <+> ppVdefg vd) $$ (text "%in" <+> ppExp e)
ppExp (Case e vb ty alts) = sep [text "%case" <+> ppAty ty <+> ppAexp e,
                             text "%of" <+> pvbind vb]
                        $$ (indent (braces (vcat (punctuate (char ';') (map palt alts)))))
ppExp (Cast e co) = (text "%cast" <+> parens (ppExp e)) $$ ppAty co
ppExp (Note s e) = (text "%note" <+> pstring s) $$ ppExp e
ppExp (External n cc t) = (text "%external" <+> text cc <+> pstring n) $$ ppAty t
ppExp (DynExternal cc t) = (text "%dynexternal" <+> text cc) $$ ppAty t
ppExp (Label n) = (text "%label" <+> pstring n)
ppExp e = pfexp e

pvbind :: Vbind -> Doc
pvbind (x,t) = parens(ppQual x <> text "::" <> ppTy t)

palt :: Alt -> Doc
palt (Acon c tbs vbs e) =
        sep [ppQual c, 
             sep (map pattbind tbs),
             sep (map pvbind vbs) <+> text "->"]
        $$ indent (ppExp e)
palt (Alit l e) = 
        (plit l <+>  text "->")
        $$ indent (ppExp e)
palt (Adefault e) = 
        (text "%_ ->")
        $$ indent (ppExp e)

plit :: Lit -> Doc
plit (Lint i t) = parens (integer i <> text "::" <> ppTy t)
-- we use (text (show r)) because "(rational r)" was printing out things
-- like "2.0e-2" (which isn't External Core)
plit (Lrational r t) = parens (text (show r) <>  text "::" <> ppTy t)
plit (Lchar c t) = parens (text ("\'" ++ escape [c] ++ "\'") <> text "::" <> ppTy t)
plit (Lstring s t) = parens (pstring s <> text "::" <> ppTy t)

pstring :: String -> Doc
pstring s = doubleQuotes(text (escape s))

escape :: String -> String
escape s = foldr f [] (map ord s)
    where 
     f cv rest
        | cv > 0xFF = '\\':'x':hs ++ rest
        | (cv < 0x20 || cv > 0x7e || cv == 0x22 || cv == 0x27 || cv == 0x5c) = 
         '\\':'x':h1:h0:rest
           where (q1,r1) = quotRem cv 16
                 h1 = intToDigit q1
                 h0 = intToDigit r1
                 hs = dropWhile (=='0') $ reverse $ mkHex cv
                 mkHex 0 = ""
                 mkHex cv = intToDigit r : mkHex q
                    where (q,r) = quotRem cv 16
     f cv rest = (chr cv):rest



ppTdef (Data qual tbinds cdefs)
    = (keyword "data" <+> ppQual qual <+> hsep (map ppTbind tbinds) <+> equals) $$
      indent (braces (vcat $ punctuate (char ';') $ map ppCdef cdefs))
ppTdef (Newtype name coercion tbinds ty)
    = (keyword "newtype" <+> ppQual name <+> ppQual coercion <+> hsep (map ppTbind tbinds)) $$
      indent (equals <+> ppTy ty)

ppCdef (Constr qual tbinds ty)
    = ppQual qual <+> sep (map (char '@' <+>) (map ppTbind tbinds)) <+> sep (map ppAty ty)

ppTbind (var, Klifted) = text (L.unpack var)
ppTbind (var, kind) = parens $ text (L.unpack var) <+> text "::" <+> ppKind kind

pattbind (t,k) = char '@' <> ppTbind (t,k)

ppKind' Klifted = char '*'
ppKind' Kunlifted = char '#'
ppKind' Kopen = char '?'
ppKind' k = parens (ppKind k)

ppKind (Karrow k1 k2) = parens (ppKind' k1 <> text "->" <> ppKind k2)
--pkind (Keq t1 t2) = parens (parens (pty t1) <+> text ":=:" <+> 
--                            parens (pty t2))
ppKind k = ppKind' k

ppAty, ppBty, ppTy :: Ty -> Doc
ppAty (Tvar n) = text (L.unpack n)
ppAty (Tcon c) = ppQual c
ppAty t = parens (ppTy t)

ppBty (Tarrow t1 t2) = parens(fsep [ppBty t1, text "->",ppTy t2])
ppBty (Tapp t1 t2) = parens $ ppAppty t1 [t2] 
ppBty t = ppAty t

ppTy (Tarrow t1 t2) = fsep [ppBty t1, text "->",ppTy t2]
ppTy (Tforall tb t) = text "%forall" <+> ppForall [tb] t
ppTy (TransCoercion t1 t2) =
  sep [text "%trans", ppAty t1, ppAty t2]
ppTy (SymCoercion t) =
  sep [text "%sym", ppAty t]
ppTy (UnsafeCoercion t1 t2) =
  sep [text "%unsafe", ppAty t1, ppAty t2]
ppTy (LeftCoercion t) =
  sep [text "%left", ppAty t]
ppTy (RightCoercion t) =
  sep [text "%right", ppAty t]
ppTy (InstCoercion t1 t2) =
  sep [text "%inst", ppAty t1, ppAty t2]
ppTy t = ppBty t

ppAppty :: Ty -> [Ty] -> Doc
ppAppty (Tapp t1 t2) ts = ppAppty t1 (t2:ts)
ppAppty t ts = sep (map ppAty (t:ts))

ppForall :: [Tbind] -> Ty -> Doc
ppForall tbs (Tforall tb t) = ppForall (tbs ++ [tb]) t
ppForall tbs t = hsep (map ppTbind tbs) <+> char '.' <+> ppTy t



ppQual (pkg,mod,ident)
    | L.null pkg && L.null mod = text (L.unpack ident)
    | otherwise = text (L.unpack pkg) <> colon <> text (L.unpack mod) <> char '.' <> text (L.unpack ident)


-- Helpers

endWith end docs = vcat (map (<> end) docs)

keyword str = char '%' <> text str

indent = nest 2
