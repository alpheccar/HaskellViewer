{- | Functions used to communicate with the OS X hViewer

-}
module Viewer(
    display
  ) where 

import Graphics.PDF
import Displayable 

import Network
import qualified Data.ByteString.Lazy as L 
import GHC.IO.Handle(hFlush,hClose)

-- | Convert a Displayable into a PDF byetstream
getPDFStream :: Displayable a => Int -> Int -> a -> IO L.ByteString
getPDFStream width height drawingCode = do
    let rect = PDFRect 0 0 width height
    pdfByteString (standardDocInfo { author=toPDFString "haskell", compressed = True}) rect $ do
        page <- addPage Nothing
        drawWithPage page $ do 
           drawing drawingCode

-- | Send the PDF bytestream to the hViewer
sendStream :: PortNumber -> L.ByteString -> IO ()
sendStream port stream = withSocketsDo $
    do h <- connectTo "127.0.0.1" (PortNumber port)
       L.hPut h stream
       hFlush h
       hClose h

-- | Display a Displayable in the OS X hViewer
display :: Displayable a => a -> IO ()
display d = do 
  stream <- getPDFStream 600 400 d
  sendStream 9000 stream


