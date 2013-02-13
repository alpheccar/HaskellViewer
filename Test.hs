{- | Test to load in ghci

testt will display the signals
testf will display the spectral density

The hViewer OS X must be running and the port 9000 avaiable (it is used by the apps)
-}
import Graphics.PDF
import Viewer
import Signal


import Numeric.GSL.Fourier
import qualified Data.Packed.Vector as V

-- | Magnitude of the FFT function with the right scaling (I hope)
-- for the energy.
myFFT :: Double -> [Double] -> [Double]
myFFT t d = 
	let l =  fromIntegral (length d)
	    complexd = map (:+ 0.0) d
	    m (x :+ y) = t*(x*x + y*y) / l
	in 
	map m . V.toList . fft  . V.fromList $ complexd

-- | Duration of the sample for the frequency resolution
duration = 4.0

-- | Sampling rate
samplingRate = 0.01 

-- | Sampling times
theTimes :: [Double]
theTimes = [0,samplingRate..duration]

-- | Sample signal a
sa = map (\t -> sin (2 * pi * t) ) $ theTimes

-- | Sample signal b
sb = map (\t -> 0.5*cos (2 * pi * t*10)) $ theTimes

-- | Display the signals
testt :: IO ()
testt = do 
    let lightBlue = Rgb 0.6 0.6 1.0
        g = discreteSignalsWithStyle samplingRate 0.0 [sa,sb] $
                                    (defaultPlotStyle { title = Just "Temporal"
        	                                          , signalStyles = [defaultSignalStyle lightBlue, defaultSignalStyle red]
        	                                          }) 
    display g

-- | Display the spectral density
testf :: IO ()
testf = do 
    let lightBlue = Rgb 0.6 0.6 1.0
        fa = myFFT samplingRate sa 
        fb = myFFT samplingRate sb
        g = discreteSignalsWithStyle (1.0 / duration) (0.0) [fa,fb]  $
                                        (defaultPlotStyle { title = Just "Frequential"
        	                                              , signalStyles = [defaultSignalStyle lightBlue, defaultSignalStyle red]
        	                                              , horizontalLabel = Just "Hz"
        	                                              }) 
    display g