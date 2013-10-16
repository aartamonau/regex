{-# LANGUAGE RecursiveDo #-}

module DFA.Internal
       -- ( DFA (..), regexpToDFA, dfaMatches )
       where

import Control.Monad.State ( State, evalState, get, put )

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import NFA.Internal ( NFA, regexpToNFA, step, epsClosure, nfaSingleton )
import qualified NFA.Internal as NFA

import Regexp ( Regexp, RegexpBackend ( compile, matches ) )

type Label = Int

data DFA =
  DFAState { label       :: Label
           , accepting   :: Bool
           , transitions :: Map Char DFA
           }
  deriving Show


regexpToDFA :: Regexp -> DFA
regexpToDFA = dfaMinimize . nfaToDFA . regexpToNFA

nfaToDFA :: NFA -> DFA
nfaToDFA nfa =
  evalState (nfaToDFA' $ epsClosure $ nfaSingleton nfa) (0, Map.empty)

type TransformationState = (Label, Map (IntMap NFA) DFA)

nfaToDFA' :: IntMap NFA -> State TransformationState DFA
nfaToDFA' nfas = do
  (_, states) <- get

  case Map.lookup nfas states of
    Just dfa -> return dfa
    Nothing  -> newState nfas

  where nextLabel :: State (Label, b) Label
        nextLabel = do
          (l, b) <- get
          put (l + 1, b)
          return l

        putState :: IntMap NFA -> DFA -> State TransformationState ()
        putState state dfa = do
          (l, states) <- get
          put (l, Map.insert state dfa states)

        newState :: IntMap NFA -> State TransformationState DFA
        newState nfas = do
          label <- nextLabel

          let accepting = IntMap.fold ((||) . NFA.accepting) False nfas
          let alpha = nfaAlpha nfas

          rec
            putState nfas dfa

            dfas <- mapM (nfaToDFA' . (flip step nfas)) alpha
            let transitions = Map.fromList $ zip alpha dfas
            let dfa = DFAState label accepting transitions

          return dfa

dfaFlatten :: DFA -> [DFA]
dfaFlatten = flatten IntSet.empty . (:[])
  where flatten :: IntSet -> [DFA] -> [DFA]
        flatten _ [] = []
        flatten visited (d : ds)
          | IntSet.member (label d) visited = flatten visited ds
          | otherwise = d : flatten visited' ds'
          where visited' = IntSet.insert (label d) visited
                ds'      = (Map.elems $ transitions d) ++ ds

dfaMaxLabel :: DFA -> Label
dfaMaxLabel = maximum . map label . dfaFlatten

nfaAlpha :: IntMap NFA -> [Char]
nfaAlpha = Set.toList . IntMap.fold Set.union Set.empty . IntMap.map nfaAlpha'
  where nfaAlpha' :: NFA -> Set Char
        nfaAlpha' = Map.keysSet . NFA.transitions

dfaMinimize :: DFA -> DFA
dfaMinimize = id

dfaMatches :: String -> DFA -> Bool
dfaMatches str dfa = dfaMatches' str (Just dfa)
  where dfaMatches' :: String -> Maybe DFA -> Bool
        dfaMatches' _         Nothing   = False
        dfaMatches' ""       (Just dfa) = accepting dfa
        dfaMatches' (c : cs) (Just dfa) = dfaMatches' cs next
          where next = Map.lookup c (transitions dfa)

instance RegexpBackend DFA where
  compile = regexpToDFA
  matches = dfaMatches
