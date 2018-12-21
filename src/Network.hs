module Network where

import Data.List (elemIndex, lookup, (!!))
import qualified Data.Map.Strict as M
import Universum

import Parser

type Var = ByteString
type Value = ByteString

type ValueMap = Map Var [Value]
data CPT = CPT
  { cptConds :: ![Var]
  , cptTable :: ![([Value], [Double])]
  } deriving (Show, Eq, Generic)

data BayesNet = BayesNet
  { bnVarValues :: !ValueMap
  , bnCPTs      :: !(Map Var CPT)
  } deriving (Show, Eq, Generic)

-- | Takes lists of values for different variables and yields
-- all possible tuples of those values
allTuples :: [[a]] -> [[a]]
allTuples []         = [[]]
allTuples (vs : vss) = [ v : tuple | v <- vs, tuple <- allTuples vss ]

-- | Splits the list into chunks of N elemens
chunks :: Int -> [a] -> [[a]]
chunks n ls
  | n <= 0 = error "bad chunk size"
  | otherwise = case ls of
      [] -> []
      ls' ->
        let (chunk, rest) = splitAt n ls'
        in chunk : chunks n rest

-- | Deletes a value from list by index
deleteIx :: Int -> [a] -> [a]
deleteIx i ls = take i ls ++ drop (i + 1) ls

-- | Delete all given keys from map
deleteAll :: Ord k => [k] -> Map k v -> Map k v
deleteAll = foldl' (.) id . map M.delete

valTable :: ValueMap -> [Var] -> Maybe [[Value]]
valTable vMap vars = allTuples <$> mapM (`M.lookup` vMap) vars

probsToCPT :: ValueMap -> Var -> [Var] -> Probabilities -> Maybe CPT
probsToCPT vMap var conds probs = do
  varVals <- M.lookup var vMap
  vTable <- valTable vMap conds
  case probs of
    ProbMap pMap -> forM_ vTable (`M.lookup` pMap) $> CPT conds (M.toList pMap)
    Table ls -> do
      let ls' = ls ++ [0 ..]
          valCount = length varVals
          probChunks = chunks valCount ls'
          varProbs = zip vTable probChunks
      pure $ CPT conds $ foldl' (flip (:)) [] varProbs

bifToBayesNet :: BIF -> Maybe BayesNet
bifToBayesNet BIF {..} = BayesNet varMap <$> cpts
  where
    varMap = M.fromList $ map (vbName &&& vbValues) bifVars
    cpts = M.fromList <$> mapM pBlockToCPT bifProbs
    pBlockToCPT ProbabilityBlock {..} =
      (pbVar,) <$> probsToCPT varMap pbVar pbConditions pbProbs

calculateProb :: BayesNet -> [(Var, Value)] -> [(Var, Value)] -> Double
calculateProb bn targets conditions =
  calculateProbNoCond bn (targets ++ conditions) /
  calculateProbNoCond bn conditions

-- | The function calculates the joint probability of given values,
-- when given variables are not all the variables present in network.
calculateProbNoCond :: BayesNet -> [(Var, Value)] -> Double
calculateProbNoCond bn@BayesNet {..} varVals =
  let vars = map fst varVals
      freeVarVals' = M.toList $ deleteAll vars bnVarValues
      freeVarVals = map (\(k, vs) -> map (k,) vs) freeVarVals'
      allFreeVarVals = allTuples freeVarVals
  in sum $ map (calculateProbJoint bn . (varVals ++)) allFreeVarVals

-- | The function assumes that given list of variables is a full list of variables
calculateProbJoint :: BayesNet -> [(Var, Value)] -> Double
calculateProbJoint BayesNet {..} varVals = product $ map getProb varVals
  where
    varMap = M.fromList varVals
    getProb (var, val) = fromMaybe (error "bad call to joint distr") $ do
      vVals <- M.lookup var bnVarValues
      vIdx <- elemIndex val vVals
      CPT conds table <- M.lookup var bnCPTs
      cVals <- mapM (`M.lookup` varMap) conds
      cProbs <- lookup cVals table
      pure $ cProbs !! vIdx
