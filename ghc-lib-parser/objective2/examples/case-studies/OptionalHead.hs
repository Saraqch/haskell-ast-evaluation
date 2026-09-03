module OptionalHead where

optionalHead xs =
  case xs of
    [] -> Nothing
    (x : _) -> Just x



