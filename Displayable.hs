{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{- | Haskell values which can be displayed in hViewer

By default, any Draw() value can be displayed !

-}
module Displayable(
	  Displayable(..)
	) where 

import Graphics.PDF

-- | Class of values which can be displayed
class Displayable a b | a -> b where 
	drawing :: a -> (PDF b, b -> Draw())

-- | Instance for the Draw() values
instance Displayable (Draw()) () where 
	drawing a = (return (), const a)

