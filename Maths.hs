module Maths where
import qualified Linear.Matrix     as LA
import qualified Linear.V4         as LA
import qualified Linear.V3         as LA
import qualified Linear.Vector     as LA
import qualified Linear.Quaternion as LA

import Control.Lens hiding (deep)

decomposeMatrix :: LA.M44 Double -> (LA.M33 Double, LA.V3 Double, LA.V3 Double)
decomposeMatrix m = (LA.transpose rot, scale, trans)
    where trans = (m ^.LA.column LA._w ^. LA._xyz)
          scale = LA.V3 sx sy sz
          sx    = vecLength (m ^.(LA.column LA._x) ^. LA._xyz)
          sy    = vecLength (m ^.(LA.column LA._y) ^. LA._xyz)
          sz    = vecLength (m ^.(LA.column LA._z) ^. LA._xyz)
          rot   = LA.V3 ((m ^. (LA.column LA._x) ^.LA._xyz) LA.^/ sx)
                        ((m ^. (LA.column LA._y) ^.LA._xyz) LA.^/ sy)
                        ((m ^. (LA.column LA._z) ^.LA._xyz) LA.^/ sz)

vecLength :: LA.V3 Double -> Double
vecLength (LA.V3 a b c) = sqrt (a*a + b*b + c*c)