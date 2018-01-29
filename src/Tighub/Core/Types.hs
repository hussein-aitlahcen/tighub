-- Types.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that e useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE GADTs #-}

module Tighub.Core.Types
  (
    Key (..),
    ByteArray,
    IVAES
  )
  where

import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (IV)
import           Data.ByteArray      (ByteArray)

type IVAES = IV AES256

data Key a where
  Key :: ByteArray a => a -> Key a
