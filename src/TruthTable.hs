module TruthTable where
import qualified Data.ByteString as B
import qualified Data.Char as C
import qualified Data.Functor.Identity as F
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified OpParse as OP
import qualified System.Directory as I
import qualified System.IO as I
import qualified System.IO.Temp as I
import qualified System.Process as PR
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Token as P

data Expr = Var Char |
  Not Expr |
  And Expr Expr |
  Or Expr Expr |
  Xor Expr Expr |
  Cond Expr Expr |
  If Expr Expr |
  Bicond Expr Expr
  deriving (Show)

type SExpr = (Expr, [String])

paren_type :: String -> String -> P.Parser SExpr -> P.Parser SExpr
paren_type p0 p1 parser = do
  P.spaces
  P.string p0
  (e, s0) <- parser
  P.spaces
  P.string p1
  let s1 = (p0 ++ head s0) : tail s0
  let s2 = init s1 ++ [last s1 ++ p1]
  return (e, s2)

paren_strings :: [(String, String)]
paren_strings = [("(", ")"), ("[", "]"), ("{", "}")]

parens :: P.Parser SExpr -> P.Parser SExpr
parens parser = P.choice (map (\(p0, p1) -> paren_type p0 p1 parser)
  paren_strings)

parse_literal :: P.Parser SExpr
parse_literal = do
  P.spaces
  result <- P.satisfy C.isAsciiLower
  return (Var result, [[result]])

lift_unary :: String -> [String] -> (Expr -> Expr) -> [(String, SExpr -> SExpr)]
lift_unary name op_strings fun = map (\op_string -> (op_string, lifted_fun))
  op_strings
  where
    lifted_fun :: SExpr -> SExpr
    lifted_fun (e, s) = (fun e, name : s)

lift_binary :: String -> [String] -> OP.Assoc -> (Expr -> Expr -> Expr) ->
  [(String, (OP.Assoc, SExpr -> SExpr -> SExpr))]
lift_binary name op_strings assoc fun = map (\op_string ->
  (op_string, (assoc, lifted_fun))) op_strings
  where
    lifted_fun :: SExpr -> SExpr -> SExpr
    lifted_fun (e0, s0) (e1, s1) = (fun e0 e1, s0 ++ [name] ++ s1)

prefix_ops :: M.Map String (SExpr -> SExpr)
prefix_ops = M.fromList (lift_unary "\\Sim" ["~", "!", "not"] Not)

infix_ops :: [M.Map String (OP.Assoc, SExpr -> SExpr -> SExpr)]
infix_ops = map (M.fromList . concat) [
  [
  lift_binary "\\to" [">", "->", "implies", "only if"] OP.LeftAssoc Cond,
  lift_binary "\\leftarrow" ["<", "<-", "if"] OP.LeftAssoc If,
  lift_binary "\\leftrightarrow" ["<>", "<->", "=", "iff", "if and only if"]
    OP.LeftAssoc Bicond],
  [
  lift_binary "\\land" ["&", "&&", "*", "and"] OP.LeftAssoc And,
  lift_binary "\\lor" ["|", "||", "or"] OP.LeftAssoc Or,
  lift_binary "\\oplus" ["+", "!=", "xor"] OP.LeftAssoc Xor]]

fix_braces :: String -> String
fix_braces "" = ""
fix_braces (c : cs) = (if c == '{' then "\\{" else if c == '}' then "\\}"
  else [c]) ++ fix_braces cs

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

parse_full_expr :: P.Parser SExpr
parse_full_expr = do
  (e, ss) <- OP.parse_expr (OP.ParseOpts {
    OP.optSpace = P.spaces,
    OP.optParens = parens,
    OP.optLiteral = parse_literal,
    OP.optPrefix = prefix_ops,
    OP.optInfix = infix_ops})
  return (e, map modify_header_string ss)

parse_input :: P.Parser ([Expr], [[String]])
parse_input = do
  result <- P.endBy parse_full_expr P.spaces
  P.eof
  return (map fst result, map snd result)

get_vars :: Expr -> [Char]
get_vars (Var c) = [c]
get_vars (Not a) = get_vars a
get_vars (And a b) = get_vars a ++ get_vars b
get_vars (Or a b) = get_vars a ++ get_vars b
get_vars (Xor a b) = get_vars a ++ get_vars b
get_vars (Cond a b) = get_vars a ++ get_vars b
get_vars (If a b) = get_vars a ++ get_vars b
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
evaluate m (If a b) = eval_bool_fun (\x y -> cond y x) m a b
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

latex_header :: String
latex_header = unlines [
  "\\documentclass[border=0.4cm]{standalone}",
  "\\usepackage{colortbl}",
  "\\newcommand{\\Sim}{{\\sim}}",
  "\\definecolor{Gray}{gray}{0.8}",
  "\\newcolumntype{w}{>{\\centering\\arraybackslash}m{0.4cm}}",
  "\\newcolumntype{g}{>{\\columncolor{Gray}}w}",
  "\\setlength{\\arrayrulewidth}{0.4mm}",
  "\\begin{document}"]

latex_footer :: String
latex_footer = "\\end{document}"

get_latex :: [Expr] -> [[String]] -> Maybe String
get_latex es sss = if length bss > 64 || length (concat sss) > 32 then Nothing
  else if null es then Just "" else Just (unlines ([
  "\\begin{tabular}{" ++ concat (replicate (length vars) "|w") ++ "||" ++
    concat (map (\(ss, i) -> replicate i 'w' ++ "g" ++
                  replicate (length ss - 1 - i) 'w' ++ "|")
             (zip sss xs)) ++ "}",
  "\\hline",
  L.intercalate " & " ((map (\c -> "$" ++ [c] ++ "$") vars) ++ concat
                       (map (\(i, ss) -> (map (\s -> "$" ++ s ++ "$")
    (whiten_strings i ss))) (zip xs sss))) ++ " \\\\",
  "\\hline"] ++ map (\bs -> L.intercalate " & " (map bool_to_str bs) ++
                            " \\\\") bss ++ [
  "\\hline",
  "\\end{tabular}"]))
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
compile_latex s dir = I.withCurrentDirectory dir
  (do
     I.writeFile "sol.tex" (latex_header ++ s ++ latex_footer)
     PR.callProcess "/usr/bin/pdflatex" ["sol.tex"]
     PR.callProcess "/usr/bin/pdftoppm" ["-png", "-rx", "600", "-ry", "600",
                                         "sol.pdf", "sol"]
     I.withFile "sol-1.png" I.ReadMode B.hGetContents)

generate_truth_table :: String -> IO (Either String B.ByteString)
generate_truth_table formula =
  case P.parse parse_input "" formula of
    Left _ -> return (Left "parse error")
    Right (es, sss) -> case get_latex es sss of
        Nothing -> return (Left "formula too complicated")
        Just code -> do
          image <- I.withSystemTempDirectory "xot" (compile_latex code)
          return (Right image)
