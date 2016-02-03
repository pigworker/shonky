module Syntax where

import Control.Monad
import Control.Applicative
import Data.Char


data Exp
  = EV String
  | EA String
  | Exp :& Exp
  | Exp :$ [Exp]
  | Exp :! Exp
  | Exp :// Exp
  | EF [[String]] [([Pat], Exp)]
  | [Def Exp] :- Exp
  | EX [Either Char Exp]
  deriving Show
infixr 6 :&
infixl 5 :$
infixr 4 :!

data Def v
  = String := v
  | DF String [[String]] [([Pat], Exp)]
  deriving Show

data Pat
  = PV VPat
  | PT String
  | PC String [VPat] String
  deriving Show

data VPat
  = VPV String
  | VPA String
  | VPat :&: VPat
  | VPX [Either Char VPat]
  | VPQ String
  deriving Show

pProg :: P [Def Exp]
pProg = pGap *> many (pDef <* pGap)

pGap :: P ()
pGap = () <$ many (pLike pChar isSpace)

pId :: P String
pId = some (pLike pChar isAlphaNum)

pP :: String -> P ()
pP s = () <$ traverse (pLike pChar . (==)) s

pExp :: P Exp
pExp = ((EV <$> pId
       <|> EA <$ pP "'" <*> pId
       <|> EX <$ pP "[|" <*> pText pExp
       <|> id <$ pP "[" <*> pLisp pExp (EA "") (:&)
       <|> thunk <$ pP "{" <* pGap <*> pExp <* pGap <* pP "}"
       <|> EF <$ pP "{" <* pGap <*>
          (id <$ pP "(" <*> pCSep (many (pId <* pGap)) ")"
              <* pGap <* pP ":" <* pGap
           <|> pure []) <*> pCSep pClause "" <* pGap <* pP "}"
       ) >>= pApp)
     <|> (:-) <$ pP "{|" <*> pProg <* pP "|}"
                 <* pGap <*> pExp
     where thunk e = EF [] [([], e)]

pText :: P x -> P [Either Char x]
pText p = (:) <$ pP "\\" <*> (Left <$> (esc <$> pChar)) <*> pText p
    <|> (:) <$ pP "`" <*> (Right <$> p) <* pP "`" <*> pText p
    <|> [] <$ pP "|]"
    <|> (:) <$> (Left <$> pChar) <*> pText p

esc :: Char -> Char
esc 'n' = '\n'
esc 't' = '\t'
esc 'b' = '\b'
esc c   = c

pLisp :: P x -> x -> (x -> x -> x) -> P x
pLisp p n c = pGap *> (n <$ pP "]" <|> c <$> p <*> pCdr) where
  pCdr = pGap *>
    (n <$ pP "]"
    <|> id <$ pP "|" <* pGap <*> p <* pGap <* pP "]"
    <|> c <$ pP "," <* pGap <*> p <*> pCdr)

pApp :: Exp -> P Exp
pApp f = (((f :$) <$ pP "(" <*> pCSep pExp ")") >>= pApp)
       <|> (((f :!) <$ pP ";" <* pGap <*> pExp) >>= pApp)
       <|> (((f ://) <$ pP "/" <* pGap <*> pExp) >>= pApp)
       <|> pure f

pCSep :: P x -> String -> P [x]
pCSep p t = pGap *> ((:) <$> p <*> pMore <|> [] <$ pP t) where
  pMore =  pGap *> ((:) <$ pP "," <* pGap <*> p <*> pMore <|> [] <$ pP t)

pDef :: P (Def Exp)
pDef = (:=) <$> pId <* pGap <* pP "->" <* pGap <*> pExp
  <|> (pId >>= pRules)

pClause :: P ([Pat],Exp)
pClause = (,) <$ pP "(" <*> pCSep pPat ")"
              <* pGap <* pP "->" <* pGap <*> pExp

pRules :: String -> P (Def Exp)
pRules f = DF f <$>
  (id <$ pP "(" <*> pCSep (many (pId <* pGap)) ")" <* pGap
     <* pP ":" <* pGap) <*>
  pCSep (pP f *> pClause) ""
  <|> DF f [] <$> ((:) <$> pClause <*>
       many (id <$ pGap <* pP "," <* pGap <* pP f <*> pClause))
    
pPat :: P Pat
pPat = PT <$ pP "{" <* pGap <*> pId <* pGap <* pP "}"
   <|> PC <$ pP "{" <* pGap <* pP "'" <*> pId <* pP "(" <*> pCSep pVPat ")"
          <* pGap <* pP "->" <* pGap <*> pId <* pGap <* pP "}"
   <|> PV <$> pVPat

pVPat :: P VPat
pVPat = VPV <$> pId
  <|> VPA <$ pP "'" <*> pId
  <|> VPX <$ pP "[|" <*> pText pVPat
  <|> VPQ <$ pP "=" <* pGap <*> pId
  <|> id <$ pP "[" <*> pLisp pVPat (VPA "") (:&:)

pLike :: P x -> (x -> Bool) -> P x
pLike p t = p >>= \ x -> if t x then return x else empty

pChar :: P Char
pChar = P $ \ s -> case s of
  (c : s) -> Just (c, s)
  [] -> Nothing

escape :: String -> String
escape = (>>= help) where
  help c | elem c "\\[|]`" = ['\\',c]
  help c = [c]

newtype P x = P {parse :: String -> Maybe (x, String)}

instance Monad P where
  return x = P $ \ s -> Just (x, s)
  P a >>= k = P $ \ s -> do
    (x, s) <- a s
    parse (k x) s

instance Applicative P where
  pure = return
  (<*>) = ap

instance Functor P where
  fmap = ap . return

instance Alternative P where
  empty = P $ \ _ -> Nothing
  p <|> q = P $ \ s -> case parse p s of
    Nothing -> parse q s
    y -> y
    