{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NFA.Internal
       ( NFA (..), regexpToNFA, nfaMatches, epsClosure, step, nfaSingleton )
       where

import Control.Applicative ( (<$>), (<*>), pure )
import Control.Arrow ( (&&&), (***) )
import Control.Monad.State ( State, evalState, get, put )

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Maybe ( fromMaybe )

import Regexp ( Regexp ( Epsilon, Symbol, Alt, Seq, Kleene )
              , RegexpBackend ( compile, matches )
              )

type Label = Int
type Transition = (Char, NFA)
type Transitions = Map Char (IntMap NFA)

data NFA =
  NFAState { label          :: Label
           , accepting      :: Bool
           , transitions    :: Transitions
           , epsTransitions :: IntMap NFA
           }

instance Eq NFA where
  nfa1 == nfa2 = label nfa1 == label nfa2

instance Ord NFA where
  compare nfa1 nfa2 = compare (label nfa1) (label nfa2)

regexpToNFA :: Regexp -> NFA
regexpToNFA re = evalState (regexpToNFA' re final) 1
  where final :: NFA
        final = NFAState 0 True Map.empty IntMap.empty

        nextLabel :: State Label Label
        nextLabel = do
          label <- get
          put (label + 1)
          return label

        nfaState :: [Transition] -> [NFA] -> State Label NFA
        nfaState transitions epsTransitions =
          NFAState <$> nextLabel
                   <*> pure False
                   <*> pure (Map.fromListWith merge transitions')
                   <*> pure (IntMap.fromListWith const epsTransitions')
          where epsTransitions' = map (label &&& id) epsTransitions
                transitions'    = map (id *** nfaSingleton) transitions

                merge         = IntMap.unionWith const

        regexpToNFA' :: Regexp -> NFA -> State Label NFA
        regexpToNFA' Epsilon nfaCont = nfaState [] [nfaCont]
        regexpToNFA' (Symbol c) nfaCont = nfaState [(c, nfaCont)] []
        regexpToNFA' (Alt rx ry) nfaCont = do
          rx' <- regexpToNFA' rx nfaCont
          ry' <- regexpToNFA' ry nfaCont

          nfaState [] [rx', ry']
        regexpToNFA' (Seq rx ry) nfaCont =
          regexpToNFA' rx =<< regexpToNFA' ry nfaCont
        regexpToNFA' (Kleene re) nfaCont = do
          rec
            loop <- nfaState [] [nfaCont, re']
            re'  <- regexpToNFA' re loop

          nfaState [] [nfaCont, loop]

epsClosure :: IntMap NFA -> IntMap NFA
epsClosure nfas
  | IntMap.size nfas == IntMap.size nfas' = nfas
  | otherwise                             = epsClosure nfas'
  where nfas' =
          IntMap.unionWith const
                 nfas
                 (IntMap.unionsWith const $
                      IntMap.elems $ IntMap.map epsTransitions nfas)

step :: Char -> IntMap NFA -> IntMap NFA
step ch = epsClosure . edge ch
  where nfaEdge :: Char -> NFA -> IntMap NFA
        nfaEdge ch (NFAState _ _ ts _) =
          fromMaybe IntMap.empty (Map.lookup ch ts)

        edge :: Char -> IntMap NFA -> IntMap NFA
        edge ch =
          IntMap.unionsWith const . IntMap.elems . IntMap.map (nfaEdge ch)

nfaSingleton :: NFA -> IntMap NFA
nfaSingleton nfa = IntMap.singleton (label nfa) nfa

nfaMatches :: String -> NFA -> Bool
nfaMatches str nfa = nfaMatches' (epsClosure $ nfaSingleton nfa) str
  where nfaMatches' :: IntMap NFA -> String -> Bool
        nfaMatches' nfas "" = IntMap.fold ((||) . accepting) False nfas
        nfaMatches' nfas (c : cs)
          | IntMap.null nfas = False
          | otherwise        = nfaMatches' (step c nfas) cs

instance RegexpBackend NFA where
  compile = regexpToNFA
  matches = nfaMatches
