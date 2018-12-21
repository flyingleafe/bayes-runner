module Parser where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as M
import Universum

data BIF = BIF
  { bifNetwork :: !NetworkBlock
  , bifVars    :: ![VariableBlock]
  , bifProbs   :: ![ProbabilityBlock]
  } deriving (Show, Eq, Generic)

data NetworkBlock = NetworkBlock
  { nbName  :: !ByteString
  , nbProps :: ![Property]
  } deriving (Show, Eq, Generic)

data VariableBlock = VariableBlock
  { vbName   :: !ByteString
  , vbValues :: ![ByteString]
  , vbProps  :: ![Property]
  } deriving (Show, Eq, Generic)

data ProbabilityBlock = ProbabilityBlock
  { pbVar        :: !ByteString
  , pbConditions :: ![ByteString]
  , pbProbs      :: !Probabilities
  } deriving (Show, Eq, Generic)

data Probabilities
  = Table ![Double]
  | ProbMap !(Map [ByteString] [Double])
  deriving (Show, Eq, Generic)

data Property = Property ByteString ByteString
  deriving (Show, Eq, Generic)

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

token :: ByteString -> Parser ByteString
token = lexeme . string

name :: Parser ByteString
name = lexeme $ takeWhile1 $ inClass "a-zA-Z0-9_-"

block :: Parser p -> Parser p
block p = token "{" *> p <* token "}"

semicolonList :: Parser p -> Parser [p]
semicolonList p = many $ p <* token ";"

commaList :: Parser p -> Parser [p]
commaList p = p `sepBy1'` token ","

property :: Parser Property
property = token "property" *>
  (Property <$> name <*> takeWhile1 (not . inClass " ;"))

varType :: Parser [ByteString]
varType = do
  token "type"
  token "discrete"
  _ <- (token "[" *> lexeme decimal <* token "]")
  block $ commaList name

networkBlock :: Parser NetworkBlock
networkBlock = NetworkBlock
  <$> (token "network" *> name)
  <*> block (semicolonList property)

varBlock :: Parser VariableBlock
varBlock = do
  token "variable"
  vName <- name
  block $ VariableBlock vName
    <$> (varType <* token ";")
    <*> semicolonList property

probBlock :: Parser ProbabilityBlock
probBlock = do
  token "probability"
  token "("
  var <- name
  conds <- (token "|" *> commaList name <|> pure [])
  token ")"
  probs <- block probabilities
  return $ ProbabilityBlock var conds probs

probabilities :: Parser Probabilities
probabilities =
  Table <$> probTable <|>
  ProbMap <$> probMap

probTable :: Parser [Double]
probTable = token "table" *> (commaList $ lexeme double) <* token ";"

probMap :: Parser (Map [ByteString] [Double])
probMap = M.fromList <$> many1 valMapping
  where valMapping = do
          token "("
          vals <- commaList name
          token ")"
          probs <- commaList (lexeme double)
          token ";"
          return (vals, probs)

bif :: Parser BIF
bif = do
  skipSpace
  network <- networkBlock
  varsOrProbs <- many1 $ (Left <$> varBlock) <|> (Right <$> probBlock)
  let unzipper (vs, ps) (Left var)   = (var : vs, ps)
      unzipper (vs, ps) (Right prob) = (vs, prob : ps)
      (vars, probs) = foldl' unzipper ([], []) varsOrProbs
  skipSpace
  return BIF
    { bifNetwork = network
    , bifVars = vars
    , bifProbs = probs
    }

parseBif :: ByteString -> Either String BIF
parseBif = parseOnly bif
