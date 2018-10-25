{-# LANGUAGE PatternSynonyms #-}

module Unison.TermPrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Foldable (fold, toList)
import           Data.Maybe (fromMaybe)
import           Data.Vector()
import           Unison.ABT (pattern AbsN')
import qualified Unison.Blank as Blank
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Reference (Reference)
import           Unison.Term
import qualified Unison.TypePrinter as TypePrinter
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

--TODO precedence comment and double check in type printer
--TODO ? askInfo suffix; > watches
--TODO more testing of line-breaking behaviour
--TODO try it out on 'real' code (as an in-place edit pass on unison-src maybe)
--TODO (improve code layout below)
--TODO use imports to trim fully qualified names

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=10
       10f 10x

   This example shows that a function application f x is enclosed in parentheses
   whenever the ambient precedence around it is >= 10, and that when printing its
   two components, an ambient precedence of 10 is used in both places.

   The pretty-printer uses the following rules for printing terms.

     >=10
       10f 10x 10y ...

     >=3
       x -> 2y

     >=2
       if 2a then 2b else 2c
       handle 2h in 2b
       case 2x of
         a | 2g -> 1b
       let x = 1y
           1z

     >=0
       10a : 0Int


   And the following for patterns.

     >=11
       x@11p

     >=10
       Con 10p 10q ...

     -- never any external parens added around the following
       { p }
       { Eff 10p 10q ... -> 0k }

-}

pretty :: Var v => (Reference -> Maybe Int -> Text) -> Int -> AnnotatedTerm v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
-- n resolves references to text names.  When getting the name of one of the constructors of a type, the
-- `Maybe Int` identifies which constructor.
pretty n p term = case term of
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r Nothing)
  Ann' tm t    -> let n' r = n r Nothing in
                    parenNest (p >= 0) $
                      pretty n 10 tm <> b" " <> (PP.Nest "  " $ PP.Group (l": " <> TypePrinter.pretty n' 0 t))
  Int' i       -> (if i >= 0 then l"+" else Empty) <> (l $ show i)
  Nat' u       -> l $ show u
  Float' f     -> l $ show f
  -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse them.  Haskell
  --      doesn't have literals for them either.  Is this function only required to
  --      operate on terms produced by the parser?  In which case the code is fine as
  --      it stands.  If it can somehow run on values produced by execution (or, one day, on
  --      terms produced by metaprograms), then it needs to be able to print them (and
  --      then the parser ought to be able to parse them, to maintain symmetry.)
  Boolean' b   -> if b then l"true" else l"false"
  Text' s      -> l $ show s
  Blank' id    -> l"_" <> (l $ fromMaybe "" (Blank.nameb id))
  RequestOrCtor' ref i -> l (Text.unpack (n ref (Just i)))
  Handle' h body -> parenNest (p >= 2) $
                      l"handle" <> b" " <> pretty n 2 h <> b" " <> l"in" <> b" "
                      <> PP.Nest "  " (PP.Group (pretty n 2 body))
  Apps' f args -> parenNest (p >= 10) $ pretty n 10 f <> appArgs args
  Vector' xs   -> PP.Nest "  " $ PP.Group $ l"[" <> commaList (toList xs) <> l"]"
  If' cond t f -> parenNest (p >= 2) $
                    (PP.Group (l"if" <> b" " <> pretty n 2 cond) <> b" " <>
                     PP.Group (l"then" <> b" " <> pretty n 2 t) <> b" " <>
                     PP.Group (l"else" <> b" " <> pretty n 2 f))
  And' x y     -> parenNest (p >= 10) $ l"and" <> b" " <> pretty n 10 x <> b" " <> pretty n 10 y
  Or' x y      -> parenNest (p >= 10) $ l"or" <> b" " <> pretty n 10 x <> b" " <> pretty n 10 y
  LamsNamed' vs body -> parenNest (p >= 3) $
                          varList vs <> l" ->" <> b" " <>
                          (PP.Nest "  " $ PP.Group $ pretty n 2 body)
  LetRecNamed' bs e -> printLet bs e
  Lets' bs e ->   printLet' bs e
  Match' scrutinee branches -> parenNest (p >= 2) $
                               l"case" <> b" " <> pretty n 2 scrutinee <> b" " <> l"of" <>
                               (PP.Nest "  " $ PP.Group $ b" " <> fold (intersperse (b"; ") (map printCase branches)))
  t -> l"error: " <> l (show t)
  where sepList sep xs = sepList' (pretty n 0) sep xs
        sepList' f sep xs = fold $ intersperse sep (map f xs)
        varList vs = sepList' (\v -> l $ Text.unpack (Var.name v)) (b" ") vs
        commaList = sepList (l"," <> b" ")

        appArgs (x : xs) = b" " <> pretty n 10 x <> appArgs xs
        appArgs [] = Empty

        -- TODO let requires layout.  Here we are manually controlling
        --      line breaking instead of leaving it to PrettyPrint rendering.
        --      Would it be better for PrettyPrint to expose a non-optional line breaking
        --      primitive?
        --      Or maybe the parser should accept let {a = foo; bar} so that there's always
        --      a layout-free option for any term.  (But then we'd want to suppress the
        --      { } when breaking lines, so would still need a PrettyPrint API change.)
        printLet bs e = parenNest (p >= 2) $
                        l"let" <> l"\n  " <> lets bs <> pretty n 0 e <> l"\n"
        printLet' bs e = parenNest (p >= 2) $
                        l"let" <> l"\n  " <> lets' bs <> pretty n 0 e <> l"\n"
        --TODO clean up this duplication
        lets ((v, binding) : rest) = (l $ Text.unpack (Var.name v)) <> b" " <> l"=" <> b" " <>
                                     pretty n 1 binding <> b"\n  " <> lets rest
        lets [] = Empty
        lets' ((_, v, binding) : rest) = (l $ Text.unpack (Var.name v)) <> b" " <> l"=" <> b" " <>
                                        pretty n 1 binding <> b"\n  " <> lets' rest
        lets' [] = Empty

        printCase (MatchCase pat guard (AbsN' vs body)) =
          (fst $ prettyPattern n (-1) vs pat) <> b" " <> printGuard guard <> l"->" <> b" " <>
          (PP.Nest "  " $ PP.Group $ pretty n 0 body)
        printCase _ = l"error"

        printGuard (Just g) = l"|" <> b" " <> pretty n 2 g <> b" "
        printGuard Nothing = Empty

pretty' :: Var v => (Reference -> Maybe Int -> Text) -> AnnotatedTerm v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t

prettyPattern :: Var v => (Reference -> Maybe Int -> Text) -> Int -> [v] -> Pattern loc -> (PrettyPrint String, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a tail of variables it doesn't use.
-- This tail is the second component of the return value.
prettyPattern n p vs patt = case patt of
  Pattern.Unbound _  -> (l"_", vs)
  Pattern.Var _      -> let (v : tail_vs) = vs
                        in (l $ Text.unpack (Var.name v), tail_vs)
  Pattern.Boolean _ b -> (if b then l"true" else l"false", vs)
  Pattern.Int _ i     -> ((if i >= 0 then l"+" else Empty) <> (l $ show i), vs)
  Pattern.Nat _ u     -> (l $ show u, vs)
  Pattern.Float _ f   -> (l $ show f, vs)
  Pattern.Constructor _ ref i pats -> let (pats_printed, tail_vs) = patterns vs pats
                                      in (parenNest (p >= 10) $ l (Text.unpack (n ref (Just i))) <> pats_printed, tail_vs)
  Pattern.As _ pat    -> let (v : tail_vs) = vs
                             (printed, eventual_tail) = prettyPattern n 11 tail_vs pat
                         in (parenNest (p >= 11) $ ((l $ Text.unpack (Var.name v)) <> l"@" <> printed), eventual_tail)
  Pattern.EffectPure _ pat -> let (printed, eventual_tail) = prettyPattern n (-1) vs pat
                              in (l"{" <> b" " <> printed <> b" " <> l"}", eventual_tail)
  Pattern.EffectBind _ ref i pats k_pat -> let (pats_printed, tail_vs) = patterns vs pats
                                               (k_pat_printed, eventual_tail) = prettyPattern n 0 tail_vs k_pat
                                           in (l"{" <> (PP.Nest "  " $ PP.Group $ b" " <>
                                               l (Text.unpack (n ref (Just i))) <> pats_printed <> b" " <> l"->" <> b" " <>
                                               k_pat_printed <> b" ") <> l"}", eventual_tail)
  t                   -> (l"error: " <> l (show t), vs)
  where l = Literal
        patterns vs (pat : pats) = let (printed, tail_vs) = prettyPattern n 10 vs pat
                                       (rest_printed, eventual_tail) = patterns tail_vs pats
                                   in (b" " <> printed <> rest_printed, eventual_tail)
        patterns vs [] = (Empty, vs)

paren :: Bool -> PrettyPrint String -> PrettyPrint String
paren True s = PP.Group $ l"(" <> s <> l")"
paren False s = PP.Group s

parenNest :: Bool -> PrettyPrint String -> PrettyPrint String
-- TODO fix line breaking tests
parenNest useParen contents = {-PP.Nest "  " $ -} paren useParen contents

l :: String -> PrettyPrint String
l = Literal

b :: String -> PrettyPrint String
b = Breakable
