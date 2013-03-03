{-# LANGUAGE FlexibleInstances #-}
module Playable(
	  Playable(..)
	) where 

class Playable a where
	sound :: a -> (Double,[Double])

instance Playable (Double,[Double]) where 
	sound = id 

	