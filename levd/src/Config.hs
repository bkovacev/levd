-- Config.hs
-- Functions / data types for working with the levd.cfg config file

module Config where

import Data.Map (Map)
import qualified Data.Map as Map

-- Pseduonym for function which will perform map to values
type SlopeCalcFn = Int -> Int

-- AST Representing the parameters in a levd.cfg config
data LevdConfig
  = CfgFile (Map Int SlopeCalcFn) Word32 Word32 Bool Bool Word8 Word8



-- data AST
--   = Const Int
--   | Var String
--   | Lam String AST
--   | App AST AST
--   | Let String AST AST
--   | Add AST AST

