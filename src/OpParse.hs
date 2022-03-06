module OpParse where
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data Assoc = LeftAssoc | RightAssoc

data ParseOpts a = ParseOpts {
  optSpace :: P.Parser (),
  optParens :: P.Parser a -> P.Parser a,
  optLiteral :: P.Parser a,
  optPrefix :: M.Map String (a -> a),
  optInfix :: [M.Map String (Assoc, (a -> a -> a))]}

parse_term :: ParseOpts a -> P.Parser a
parse_term opts = optParens opts (parse_expr opts) P.<|> (optLiteral opts)

parse_not_prefix :: [String] -> String -> P.Parser String
parse_not_prefix strings string = do
  let others = filter (\s -> L.isPrefixOf string s && s /= string) strings
  P.notFollowedBy (P.choice (map (P.try . P.string) others))
  P.string string

parse_longest_match :: [String] -> P.Parser String
parse_longest_match strings = P.choice (map (P.try . parse_not_prefix strings)
  strings)

parse_prefix_expr :: ParseOpts a -> P.Parser a
parse_prefix_expr opts = do
  optSpace opts
  op_string <- P.optionMaybe (parse_longest_match (M.keys (optPrefix opts)))
  case op_string of
    Nothing -> parse_term opts
    Just s -> do
      e <- parse_prefix_expr opts
      return ((optPrefix opts M.! s) e)

parse_infix_expr_rec :: ParseOpts a -> Int -> Maybe a -> P.Parser a
parse_infix_expr_rec opts level val = if level == length (optInfix opts)
  then parse_prefix_expr opts
  else do
    let infix_ops = optInfix opts !! level
    e0 <- case val of
      Nothing -> parse_infix_expr_rec opts (level + 1) Nothing
      Just e -> return e
    optSpace opts
    op_string <- P.optionMaybe (parse_longest_match (M.keys infix_ops))
    case op_string of
      Nothing -> return e0
      Just s -> do
        let (assoc, fun) = infix_ops M.! s
        case assoc of
          LeftAssoc -> do
            e1 <- parse_infix_expr_rec opts (level + 1) Nothing
            parse_infix_expr_rec opts level (Just (fun e0 e1))
          RightAssoc -> do
            e1 <- parse_infix_expr_rec opts level Nothing
            return (fun e0 e1)

parse_infix_expr :: ParseOpts a -> P.Parser a
parse_infix_expr opts = parse_infix_expr_rec opts 0 Nothing

parse_expr :: ParseOpts a -> P.Parser a
parse_expr = parse_infix_expr
