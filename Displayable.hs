{-# LANGUAGE FlexibleInstances #-}
{- | Haskell values which can be displayed in hViewer

By default, any PDF Draw() value can be displayed !
-}
module Displayable(
	  Displayable(..)
	) where 

import Graphics.PDF

-- | Class of values which can be displayed
class Displayable a where 
	drawing :: a -> Draw ()

-- | Instance for the Draw() values
instance Displayable (Draw()) where 
	drawing a = a 
