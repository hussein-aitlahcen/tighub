-- CryptoSpec.hs ---

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

{-# LANGUAGE OverloadedStrings #-}

module CryptoSpec
  (
    tests
  )
  where

import           Tighub.Core.Crypto

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (Assertion, (@?=))

import           Data.Bifunctor                 (first)

tests :: Test
tests = testGroup "crypto-spec"
  [
    testCase "hex encoding" testHexEncoding,
    testCase "hex decoding" testHexDecoding,
    testCase "AES256 encrypt/decrypt" testEncrypt
  ]

combine :: (Show b, Eq b) => (a -> b) -> [(a, b)] -> Assertion
combine f = mconcat . map (uncurry (@?=) . first f)

testHexEncoding :: Assertion
testHexEncoding = combine encodeHex pairs
  where
    pairs =
      [
        ("Hello, World !", "48656c6c6f2c20576f726c642021")
      ]

testHexDecoding :: Assertion
testHexDecoding = combine decodeHex pairs
  where
    pairs =
      [
        ("48656c6c6f2c20576f726c642021", "Hello, World !")
      ]

testEncrypt :: Assertion
testEncrypt = (@?=) (CryptoPassed "Hello, World !") =<< doTest "Hello, World !"
  where
    doTest :: ByteString -> IO (CryptoFailable ByteString)
    doTest msg = do
      (key, array) <- (,) <$> generateSecret 32 <*> generateArray
      pure $ go key msg =<< array
        where
          go key content array  = decryptBlock key array =<< cryptBlock key array content
