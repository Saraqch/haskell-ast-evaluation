module Analysis.ResolvedGuard
  ( ResolvedGuard (..),
  )
where

data ResolvedGuard = ResolvedGuard
  { resolvedGuardLine :: Int,
    resolvedCondition :: String,
    resolvedExpression :: String
  }
  deriving (Show, Eq)
