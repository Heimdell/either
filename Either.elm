module Either where

{-| A simple module providing an Either type.

# Types
@docs Either

# Maps
@docs mapLeft, mapRight, mapBoth

# Functor
@docs map

# Elimination
@docs elim, isLeft, isRight

# Applicative
@docs (<*>), pure

# Monad
@docs bind
-}

type Either a b
  = Left a
  | Right b

mapRight : (b -> b') -> Either a b -> Either a b'
mapRight = mapBoth identity

map : (b -> b') -> Either a b -> Either a b'
map = mapRight

mapLeft : (a -> a') -> Either a b -> Either a' b
mapLeft f = mapBoth f identity

mapBoth : (a -> a') -> (b -> b') -> Either a b -> Either a' b'
mapBoth f g = elim (Left << f) (Right << g)

swap : Either a b -> Either b a
swap = elim Right Left

bind : Either e a -> (a -> Either e b) -> Either e b
-- | Monadic interface
bind ma amb = mapRight amb ma |> joinRight

(<*>) : Either e (a -> b) -> Either e a -> Either e b
-- | Applicative interface
mf <*> mx =
  mf `bind` \f ->
  mx `bind` \x ->
    pure <| f x

pure : a -> Either e a
pure = Right

catch : Either e a -> (e -> Either e a) -> Either e a
catch ma amb = mapLeft amb ma |> joinLeft

throw : e -> Either e a
throw = Left

joinRight : Either e (Either e a) -> Either e a
joinRight = elim Left (elim Left Right)

joinLeft : Either (Either e a) a -> Either e a
joinLeft = elim (elim Left Right) Right

elim : (a -> c) -> (b -> c) -> Either a b -> c
elim f g ex = case ex of
  Left a  -> f a
  Right b -> g b

isLeft : Either a b -> Bool
isLeft = elim (\_ -> True) (\_ -> False)
  
isRight : Either a b -> Bool
isRight = elim (\_ -> False) (\_ -> True)
