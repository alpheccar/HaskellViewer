{- | Functions used to communicate with the OS X hViewer

-}
module Viewer(
    display
  , viewerSize
  , play
  ) where 

import Graphics.PDF
import Displayable 
import Playable

import Network
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as B
import GHC.IO.Handle(hFlush,hClose)
import AIFF(encodeSound)

dispCode = L.pack "disp"
playCode = L.pack "play"

-- | Convert a Displayable into a PDF byetstream
getPDFStream :: Displayable a b => Int -> Int -> a -> IO L.ByteString
getPDFStream width height drawingCode = do
    let rect = PDFRect 0 0 width height
    let (aPdfValue, drawAction) = drawing drawingCode 
    pdfByteString (standardDocInfo { author=toPDFString "haskell", compressed = True}) rect $ do
        value <- aPdfValue width height
        page <- addPage Nothing
        drawWithPage page $ do 
           drawAction value width height

-- | Send the PDF bytestream to the hViewer
sendStream :: PortNumber -> L.ByteString -> IO ()
sendStream port stream = withSocketsDo $
    do h <- connectTo "127.0.0.1" (PortNumber port)
       L.hPut h stream
       hFlush h
       hClose h

-- | Display a Displayable in the OS X hViewer
display :: Displayable a b 
        => a 
        -> IO ()
display d = do 
  let (w,h) = viewerSize
  stream <- getPDFStream w h d
  sendStream 9000 (L.append dispCode stream)

play :: Playable a 
     => a 
     -> IO ()
play s = do 
  let (samplingFreq, signal) = sound s 
      --peak = maximum (map abs signal)
      --normSignal = map (/ peak) signal
      stream = encodeSound samplingFreq signal
  --L.writeFile "test.aiff" stream
  sendStream 9000 (L.append playCode stream)  

viewerSize :: (Int,Int)
viewerSize = (800,600)

s :: [Double]
s = let d = (1.0 :: Double) / 44100.0 :: Double
    in map (\t -> 0.5*cos(2*pi*(fromIntegral t / 44100.0)*0.5)*sin (2*pi*(fromIntegral t / 44100.0)*2000)) [0..88200]

testPlay = play (44100 :: Double,s)

