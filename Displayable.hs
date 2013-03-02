{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- | Haskell values which can be displayed in hViewer

By default, any Draw() value can be displayed !

-}
module Displayable(
      Displayable(..)
    , Rsrc(..)
    , DrawingAction(..)
    , Vertical(..)
    ) where 

import Graphics.PDF hiding(Vertical)

type Rsrc b = Int -> Int -> PDF b 
type DrawingAction b = b -> Int -> Int -> Draw ()
type DispResult b = (Rsrc b, DrawingAction b)

-- | Class of values which can be displayed
class Displayable a b | a -> b where 
    drawing :: a -> (Rsrc b, DrawingAction b)

-- | Instance for the Draw() values
instance Displayable (Draw()) () where 
    drawing a = (\_ -> \_ -> return (), \_ -> \_ -> \_ -> a)

data Vertical a = Vertical Double [a] 

instance Displayable a b => Displayable (Vertical a) [b] where 
    drawing (Vertical sep l) = 
        let nb = length l 
            nl = map drawing l
            newH h = floor $ (fromIntegral h - (fromIntegral nb - 1)*sep) / fromIntegral nb
            r w h = do 
                let h' = newH h 
                mapM ((\ir -> ir w h') . fst) nl
            action rl w h = do 
                let h' = newH h 
                    mkPlot (plotNb,rsrc,daction) = do 
                    withNewContext $ do
                            applyMatrix $ translate (0 :+ (fromIntegral (h - h') - fromIntegral plotNb *(fromIntegral h' + sep)))
                            daction rsrc w h'
                mapM_ mkPlot (zip3 [0,1..] rl (map snd nl))
        in 
        (r, action)
                
                
                    

--instance Displayable (Pair a) (a,a) where 
--   drawing (Pair pa pb c) = 
--    let (ra, da) = pa
--        (rb, db) = pb 
--        sep = 20.0 :: Double
--        r = \w -> \h -> do 
--            let h' = floor $ ((fromIntegral h - sep) / 2.0)
--            a <- ra w h'
--            b <- rb w h'
--            return (a,b)
--        action = \(a,b) -> \w -> \h -> do
--           let h' = floor ((fromIntegral h - sep) / 2.0)
--           c (da a w h') (db b w h') 
--    in 
--    (r,action)
