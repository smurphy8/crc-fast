{-# CFILES lib_crc/lib_crc.c #-}
{-# INCLUDE "lib_crc.h" #-}
{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, NoMonomorphismRestriction #-}
module Foreign.CRC where
import Foreign.C 
import Foreign.C.Types
import Data.Word
import Data.ByteString
import Data.ByteString.Builder
import qualified Data.Foldable as F
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as CB
import Control.Applicative
import Data.Binary.Get
import Numeric

foreign import ccall "lib_crc.h update_crc_16" c_updateCRC16 :: CUShort -> CChar -> CUShort


tList = [0x01,0x02,0x01,0x00,0x07,0x00]

tString :: LB.ByteString
tString = LB.concat $ toLazyByteString.word16BE <$> tList

updateCRC16 :: Word16 -> CUShort -> Word16
updateCRC16 wi c = let cs = fromIntegral $ wi :: CChar               
                   in fromIntegral.(c_updateCRC16 c) $ cs


crcWord16List :: F.Foldable t => t Word16 -> Word16
crcWord16List = F.foldl' (\a b -> updateCRC16 b (fromIntegral a) ) 0 

makeWord16Listbe :: LB.ByteString -> [Word16]
makeWord16Listbe bs = do 
   case pushChunks (runGetIncremental getWord16be) bs of 
     (Done bs os a) -> a : (makeWord16Listbe.LB.fromStrict $ bs)
     _ -> []
                       
 

showCRC x =  let crc = crcWord16List.makeWord16Listbe $ x
             in print $ (showHex crc "") 