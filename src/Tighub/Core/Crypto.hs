-- Crypto.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ScopedTypeVariables #-}

module Tighub.Core.Crypto
  (
    Key (..),
    IVAES,
    ByteArray,
    CryptoError,
    ByteString,
    generateSecret,
    generateArray,
    initCipher,
    cryptBlock,
    decryptBlock,
    encodeHex,
    decodeHex
  )
  where


import           Tighub.Core.Types      (Key (..), IVAES)

import           Crypto.Cipher.AES      (AES256)
import           Crypto.Cipher.Types    (BlockCipher (..), Cipher (..),
                                         makeIV)
import           Crypto.Error           (CryptoError (..), CryptoFailable (..))
import qualified Crypto.Random.Types    as CRT

import           Data.ByteArray         (ByteArray)
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base16 (decode, encode)

generateSecret :: (CRT.MonadRandom m, ByteArray a) => Int -> m (Key a)
generateSecret = fmap Key . CRT.getRandomBytes

generateArray :: CRT.MonadRandom m => m (Maybe IVAES)
generateArray = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV bytes

initCipher :: ByteArray a => Key a -> Either CryptoError AES256
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

cryptBlock :: ByteArray a => Key a -> IVAES -> a -> Either CryptoError a
cryptBlock secretKey initIV msg =
  case initCipher secretKey of
    Left e  -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decryptBlock :: ByteArray a => Key a -> IVAES -> a -> Either CryptoError a
decryptBlock = cryptBlock

encodeHex :: ByteString -> ByteString
encodeHex = encode

decodeHex :: ByteString -> ByteString
decodeHex = fst . decode
