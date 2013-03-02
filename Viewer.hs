{- | Functions used to communicate with the OS X hViewer

-}
module Viewer(
    display
  , viewerSize
  ) where 

import Graphics.PDF
import Displayable 

import Network
import qualified Data.ByteString.Lazy as L 
import GHC.IO.Handle(hFlush,hClose)

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
  sendStream 9000 stream

viewerSize :: (Int,Int)
viewerSize = (800,600)
