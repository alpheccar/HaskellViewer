module AIFF(
    encodeSound
  ) where 

import Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy as L
import Data.Word 
import Data.Monoid 
import Data.Bits

import Debug.Trace

debug a = trace (show a) a

samplingRate :: Double 
             -> Builder 
samplingRate r = 
  let (a,b) = ieee80 r
  in 
  word16BE a <>
  word64BE b

ieee80 :: Double -> (Word16,Word64)
ieee80 r =
  let f = floor r :: Word64
  in 
  (fromIntegral $ 16398 , f `shiftL` 48)

commSize = 24 
verSize = 4 
sndSize nbSamples = 8 + nbSamples * 8 

common :: Double 
       -> Word32
       -> Builder 
common sampling nbSamples = 
  string7 "COMM" <>
  word32BE commSize <> 
  int16BE 1 <> 
  word32BE nbSamples <>
  int16BE 64 <> 
  samplingRate sampling <> 
  string7 "FL64" <> 
  int8 0 <> 
  string7 "\0"

sound :: [Double] 
      -> Word32
      -> Builder 
sound samples nbSamples = 
   string7 "SSND" <> 
   word32BE (sndSize nbSamples) <> 
   int32BE 0 <> 
   int32BE 0 <> 
   (mconcat . map doubleBE $ samples) 

verChunk :: Builder 
verChunk = 
  string7 "FVER" <> 
  word32BE verSize <> 
  word32BE 0xA2805140 

cs f = f + 8 

encodeSound :: Double
            -> [Double]
            -> L.ByteString 
encodeSound sampling samples = 
  let nbSamples = fromIntegral $ length samples
      totalSize = 4 + cs commSize + cs (sndSize nbSamples) + cs verSize
      r = string7 "FORM" <>
          word32BE totalSize <>
          string7 "AIFC" <> 
          verChunk <>
          common sampling nbSamples <>
          sound samples nbSamples    
  in 
  toLazyByteString r    