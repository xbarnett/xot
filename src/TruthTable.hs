module TruthTable where
import qualified Data.ByteString as B
import qualified Data.Char as C
import qualified Data.Functor.Identity as F
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified System.Directory as I
import qualified System.IO as I
import qualified System.IO.Temp as I
import qualified System.Process as PR
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P
import qualified Text.Parsec.String as P

data Expr = Var Char |
  Not Expr |
  And Expr Expr |
  Or Expr Expr |
  Xor Expr Expr |
  Cond Expr Expr |
  Bicond Expr Expr
  deriving (Show)

parse_var :: P.Parser (Expr, [String])
parse_var = do
  result <- P.lower
  return (Var result, [[result]])

paren_type :: String -> String -> P.Parser (Expr, [String])
  -> P.Parser (Expr, [String])
paren_type p0 p1 parser = do
  P.string p0
  (e, s0) <- parser
  P.string p1
  let s1 = (p0 ++ head s0) : tail s0
  let s2 = init s1 ++ [last s1 ++ p1]
  return (e, s2)

paren_strings :: [(String, String)]
paren_strings = [("(", ")"), ("[", "]"), ("{", "}")]

parens :: P.Parser (Expr, [String]) -> P.Parser (Expr, [String])
parens parser = P.choice (map (\(p0, p1) -> paren_type p0 p1 parser)
  paren_strings)

parse_term :: P.Parser (Expr, [String])
parse_term = do
  P.spaces
  result <- parens parse_expr P.<|> parse_var
  P.spaces
  return result

data Operator = Prefix String [String] (Expr -> Expr) |
  Infix String [String] (Expr -> Expr -> Expr) P.Assoc

operators :: [[Operator]]
operators = [
  [Prefix "\\Sim" ["~", "not"] Not],
  [Infix "\\land" ["&", "and"] And P.AssocLeft,
   Infix "\\lor" ["|", "or"] Or P.AssocLeft,
   Infix "\\oplus" ["xor"] Xor P.AssocLeft],
  [Infix "\\to" [">", "->", "to", "implies"] Cond P.AssocLeft,
   Infix "\\leftrightarrow" ["<>", "<->", "iff"] Bicond P.AssocLeft]]

convert_op :: Operator -> [P.Operator String () F.Identity (Expr, [String])]
convert_op (Prefix lname names fun) = map (\name -> P.Prefix (do
  P.try (P.string name)
  return (\(e0, s0) -> (fun e0, lname : s0)))) names
convert_op (Infix lname names fun assoc) = map (\name -> P.Infix (do
  P.try (P.string name)
  return (\(e0, s0) (e1, s1) -> (fun e0 e1, s0 ++ [lname] ++ s1)))
    assoc) names

parse_expr :: P.Parser (Expr, [String])
parse_expr = P.buildExpressionParser (map (concat . (map convert_op)) operators)
  parse_term

fix_braces :: String -> String
fix_braces = T.unpack . T.replace (T.pack "{") (T.pack "\\{") .
  T.replace (T.pack "}") (T.pack "\\}") . T.pack

modify_header_string :: String -> String
modify_header_string s = (if (null s0) then "" else ("\\llap{" ++
                                                     fix_braces s0 ++ "}"))
  ++ s1 ++ (if (null s2) then "" else ("\\rlap{" ++ fix_braces s2 ++ "}"))
  where
    pchars :: [Char]
    pchars = concat (map (uncurry (++)) paren_strings)

    s0 :: String
    s0 = takeWhile (\c -> elem c pchars) s

    s2 :: String
    s2 = reverse (takeWhile (\c -> elem c pchars) (reverse s))

    s1 :: String
    s1 = [c | (i, c) <- zip [0..] s, i >= length s0, i < length s - length s2]

parse_full_expr :: P.Parser (Expr, [String])
parse_full_expr = do
  (e, ss) <- parse_expr
  return (e, map modify_header_string ss)

parse_input :: P.Parser ([Expr], [[String]])
parse_input = do
  result <- P.many1 parse_full_expr
  P.eof
  return (map fst result, (map snd result))

get_vars :: Expr -> [Char]
get_vars (Var c) = [c]
get_vars (Not a) = get_vars a
get_vars (And a b) = get_vars a ++ get_vars b
get_vars (Or a b) = get_vars a ++ get_vars b
get_vars (Xor a b) = get_vars a ++ get_vars b
get_vars (Cond a b) = get_vars a ++ get_vars b
get_vars (Bicond a b) = get_vars a ++ get_vars b

powerset :: [Char] -> [M.Map Char Bool]
powerset [] = [M.empty]
powerset (x:xs) = map (M.insert x True) p ++ map (M.insert x False) p
  where
    p :: [M.Map Char Bool]
    p = powerset xs

cond :: Bool -> Bool -> Bool
cond True b = b
cond False _ = True

eval_bool_fun :: (Bool -> Bool -> Bool) -> M.Map Char Bool -> Expr -> Expr ->
  ([Bool], Int)
eval_bool_fun f m a b = (xs ++ [val] ++ ys, length xs)
  where
    xs :: [Bool]
    i :: Int
    (xs, i) = evaluate m a

    ys :: [Bool]
    j :: Int
    (ys, j) = evaluate m b

    val :: Bool
    val = f (xs !! i) (ys !! j)

evaluate :: M.Map Char Bool -> Expr -> ([Bool], Int)
evaluate m (Var s) = ([m M.! s], 0)
evaluate m (Not a) = (val : xs, 0)
  where
    xs :: [Bool]
    i :: Int
    (xs, i) = evaluate m a

    val :: Bool
    val = not (xs !! i)
evaluate m (And a b) = eval_bool_fun (&&) m a b
evaluate m (Or a b) = eval_bool_fun (||) m a b
evaluate m (Xor a b) = eval_bool_fun (/=) m a b
evaluate m (Cond a b) = eval_bool_fun cond m a b
evaluate m (Bicond a b) = eval_bool_fun (==) m a b

truth_table :: [Char] -> [Expr] -> ([Int], [[Bool]])
truth_table vars es = (map (snd . evaluate (head p)) es,
  map (\m -> M.elems m ++ concat (map (\e -> fst (evaluate m e)) es)) p)
  where
    p :: [M.Map Char Bool]
    p = powerset vars

bool_to_str :: Bool -> String
bool_to_str True = "T"
bool_to_str False = "F"

get_latex :: [Expr] -> [[String]] -> Maybe String
get_latex es sss = if length bss > 256 || length (concat sss) > 256 then Nothing
  else Just (unlines ([
  "\\documentclass[border=0.4cm]{standalone}",
  "\\usepackage{colortbl}",
  "\\newcommand{\\Sim}{{\\sim}}",
  "\\definecolor{Gray}{gray}{0.8}",
  "\\newcolumntype{w}{>{\\centering\\arraybackslash}m{0.4cm}}",
  "\\newcolumntype{g}{>{\\columncolor{Gray}}w}",
  "\\begin{document}",
  "\\begin{tabular}{" ++ concat (replicate (length vars) "|w") ++ "||" ++
    concat (map (\(ss, i) -> replicate i 'w' ++ "g" ++
                  replicate (length ss - 1 - i) 'w' ++ "|")
             (zip sss xs)) ++ "}",
  "\\hline",
  L.intercalate " & " ((map (\c -> "$" ++ [c] ++ "$") vars) ++ concat
                       (map (\(i, ss) ->(map (\s -> "$" ++ s ++ "$")
    (whiten_strings i ss))) (zip xs sss))) ++ " \\\\",
  "\\hline"] ++ map (\bs -> L.intercalate " & " (map bool_to_str bs) ++
                      " \\\\") bss ++ [
  "\\hline",
  "\\end{tabular}",
  "\\end{document}"]))
  where
    vars :: [Char]
    vars = S.toList (S.fromList (concat (map get_vars es)))

    xs :: [Int]
    bss :: [[Bool]]
    (xs, bss) = truth_table vars es

    whiten_strings :: Int -> [String] -> [String]
    whiten_strings i ss = take i ss ++ ["\\cellcolor{white}" ++ ss !! i] ++
      drop (i + 1) ss

compile_latex :: String -> String -> IO B.ByteString
compile_latex s dir = do
  I.setCurrentDirectory dir
  I.writeFile "sol.tex" s
  PR.callProcess "/usr/bin/pdflatex" ["sol.tex"]
  PR.callProcess "/usr/bin/pdftoppm" ["-png", "-rx", "300", "-ry", "300",
                                      "sol.pdf", "sol"]
  result <- I.withFile "sol-1.png" I.ReadMode B.hGetContents
  I.setCurrentDirectory ".."
  return result

generate_truth_table :: String -> IO (Either String B.ByteString)
generate_truth_table formula =
  case P.parse parse_input "" formula of
    Left e -> return (Left ("parse error: " ++ show e))
    Right (es, sss) -> case get_latex es sss of
        Nothing -> return (Left "formula too complicated")
        Just code -> do
          image <- I.withTempDirectory "." "tempdir" (compile_latex code)
          return (Right image)
