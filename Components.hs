 {-# LANGUAGE FlexibleInstances #-}
 --{-# LANGUAGE ConstrainedClassMethods #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE ScopedTypeVariables #-}


 --{-# LANGUAGE FlexibleContexts #-}

module Components where
import qualified Graphics.OpenSCAD as Cad
import Data.Map
--import Data.HList
import Maths
import Linear.Matrix as LA
import Linear.Quaternion as LA
import Linear.Vector as LA
import qualified Linear.V4         as LA
import qualified Linear.V3         as LA
import qualified GHC.Float as LA
import Debug.Trace
import qualified Maths as Math
import GHC.Generics (Meta)
import Data.Set
import qualified Control.Lens as Data.Set


epsillon = 0.001

--import Data.HashMap
type ParamMap = Map String Float

--                                 name   def.args  giv.args    
type Link settings = settings -> (String, ParamMap, ParamMap -> Element -> Element)

data Element = Element { 
    solid :: Cad.Model3d,
    antiSolid :: Cad.Model3d,
    vis :: Cad.Model3d
} deriving Show

data MetaData = MetaData { 
    mSolidGroups :: Set String,
    mVisGroups :: Set String
} deriving Show

data SynthesisParameters = SynthesisParameters { 
    solidGroups :: [String],
    visGroups :: [String],
    release :: Bool
} deriving Show

type Mount = LA.M44 Double

emptyModel = Cad.union []

zeroMount :: LA.V4 (LA.V4 Double)
zeroMount = LA.identity

emptyElement = Element { 
    solid = emptyModel,
    antiSolid = emptyModel,
    vis = emptyModel
}

emptyMetaData = MetaData { 
    mSolidGroups = Data.Set.empty,
    mVisGroups = Data.Set.empty
}

translate v e = 
    Element {
        solid = Cad.translate v (solid e),
        antiSolid = Cad.translate v (antiSolid e),
        vis = Cad.translate v (vis e)
    }

cutantiSolid :: Element -> Element
cutantiSolid e =
    Element {
        solid = Cad.difference (solid e) (antiSolid e),
        antiSolid = emptyModel,
        vis = vis e
    }

toantiSolid :: Element -> Element
toantiSolid e =
    Element {
        solid = emptyModel,
        antiSolid = Cad.union [solid e, antiSolid e],
        vis = vis e
    }



data Units =   Abs | Rel
data Origin = Low | Middle | High
type Coord = (Units, Origin, Double)

coordToAbs :: Coord -> Double -> Double
coordToAbs (units, origin, value) length = 
        direction * abs_value + abs_origin
    where
        abs_value = case units of Abs -> value
                                  Rel -> value * length
        (direction, abs_origin) = case origin of Low -> (1.0, 0.0)
                                                 Middle -> (1.0, 0.5 * length)
                                                 High -> (1.0, length)

roll :: Mount -> Double -> Mount
roll x y = x

invVec :: LA.V3 Double -> LA.V3 Double
invVec (LA.V3 x y z) = LA.V3 (-x) (-y) (-z)

invMount :: Mount -> Mount
invMount = LA.inv44 

applyMount :: Mount -> Cad.Model3d -> Cad.Model3d
applyMount m =
    let LA.V4 (LA.V4 i00 i01 i02 i03)
              (LA.V4 i10 i11 i12 i13)
              (LA.V4 i20 i21 i22 i23)
              (LA.V4 i30 i31 i32 i33) = m
    in
        Cad.multMatrix (
                   (i00, i01, i02, i03),
                   (i10, i11, i12, i13),
                   (i20, i21, i22, i23),
                   (i30, i31, i32, i33))

(@@) :: Element -> Mount -> Element
e @@ m = Element {
            solid = applyMount m (solid e),
            antiSolid = applyMount m (antiSolid e),
            vis = applyMount m (vis e)
        }

(+++) :: Element -> Element -> Element
rhs +++ lhs = Element {
            solid = Cad.union [solid rhs, solid lhs],
            antiSolid = Cad.union [antiSolid rhs, antiSolid lhs],
            vis = Cad.union [vis rhs, vis lhs]
        }

joinMetaData :: MetaData -> MetaData -> MetaData
joinMetaData lhs rhs = MetaData {
        mSolidGroups = Data.Set.union (mSolidGroups lhs) (mSolidGroups rhs),
        mVisGroups = Data.Set.union (mVisGroups lhs) (mVisGroups rhs)
    }

class Elementable elementType mountType | elementType -> mountType where
    getMount :: elementType -> mountType -> Mount
    toElement :: elementType -> SynthesisParameters -> Element
    getMetaData :: elementType -> MetaData


data HJoinResult nestedMounts = JoinResult {
    helement :: Element, 
    hmounts :: nestedMounts
}

-- instance Elementable (HJoinResult nestedMounts) (nestedMounts -> Mount) where
--     toElement join_result = helement join_result
--     getMount join_result selector = selector $ hmounts join_result

data ElementableWrapper mountType = ElementableWrapper {
    wElement :: SynthesisParameters -> Element, 
    wMount :: mountType -> Mount,
    wMeta :: MetaData
}

wrap :: Elementable e eM  => e -> ElementableWrapper eM
wrap e = ElementableWrapper {
            wElement = toElement e,
            wMount = getMount e,
            wMeta = getMetaData e
        }

instance Elementable (ElementableWrapper mountType) mountType where
    toElement e = wElement e
    getMount e m = wMount e m
    getMetaData e = wMeta e


-- M = MountType
-- b = base
-- c = child
-- j = joined

--join :: Ord jM => Elementable b bM => b -> JoinResult jM bM
--join base mount_mappings = JoinResult {
--    element = toElement base,
--    mounts = fromList [(joined_mount, getMount base base_mount) | (base_mount, joined_mount) <- mount_mappings]
--}

-- (<<+) :: Elementable c cM => HJoinResult (HList nestedMounts) -> (HList nestedMounts -> Mount, cM, c) -> HJoinResult (HExtendR (cM -> [MountTransform]) (HList nestedMounts))
-- joined <<+ (nested_mount, childMount, child) = 
--         ret
--     where
--         ret = JoinResult {
--             helement = helement joined +++ child_mounted,
--             hmounts = hAppend (get_child_mount.*.HNil) (hmounts joined)
--         }
--         base_mount_transform = getMount joined nested_mount
--         child_at_its_mount = toElement child @@ invMount (getMount child childMount)
--         child_mounted = child_at_its_mount @@ base_mount_transform
--         get_child_mount mountType = base_mount_transform  ++ getMount child mountType --  FIXME; how about child transform


degToRad x = x / 180.0 * LA.pi

rotationX :: Double -> Quaternion Double
rotationX = LA.axisAngle (LA.V3 1 0 0) . degToRad
rotationY = LA.axisAngle (LA.V3 0 1 0) . degToRad
rotationZ = LA.axisAngle (LA.V3 0 0 1) . degToRad
translation = LA.mkTransformation (rotationX 0)

rotationTransform = flip LA.mkTransformation (LA.V3 0 0 0)

data Choice t1 t2 = L t1 | R t2 -- FIXME

(<<) :: Elementable b bM => Elementable c cM => b -> (bM, cM, Double, rM -> Choice bM cM, c) -> ElementableWrapper rM
base << (baseMount, childMount, angle, selector, child) = 
        ElementableWrapper {
            wElement = \p -> toElement base p +++ childMounted p,
            wMount = wmount,
            wMeta = joinMetaData (getMetaData base) (getMetaData child) 
        }
    where
        base_mount_transform = getMount base baseMount
        inv_child_mount = rotationTransform (rotationY 180) !*! rotationTransform (rotationZ angle) !*! invMount (getMount child childMount)
        childAtItsMount = \p -> toElement child p @@ inv_child_mount
        childMounted = \p -> childAtItsMount p @@ base_mount_transform
        wmount mount = case selector mount of
            L baseMount -> getMount base baseMount
            R outerChildMount -> base_mount_transform !*! inv_child_mount !*! getMount child outerChildMount

-- ----------------------------------------------------------

(<<+) :: Elementable b bM => b -> (SynthesisParameters -> Element) -> ElementableWrapper bM
base <<+ child = 
        ElementableWrapper {
            wElement = \p -> toElement base p +++ child p,
            wMount = getMount base,
            wMeta = getMetaData base
        }

-- ----------------------------------------------------------

--s :: (Int, m) -> Choice m m
--s (i, m) = if i == 0 then R m else m (i - 1)

repeat :: Elementable e m => e -> m -> m -> Double -> Int -> ElementableWrapper (Int, m)
repeat element out_mount in_mount angle repeats = 
    let f (e, ms)  = 
            let joined = e << (out_mount, in_mount, angle, R, element)
            in (joined, getMount joined : ms)
        (e, ms) = (iterate f (wrap element, [getMount element]) !! (repeats - 1))
    in ElementableWrapper {
        wElement = wElement e,
        wMount = uncurry (ms !!),
        wMeta = getMetaData element
    }
         

----------------------------------------------------------

data Box = Box { 
    x :: Double,
    y :: Double,
    z :: Double 
} deriving Show 

data BoxSide = Px | Nx | Py | Ny | Pz | Nz
type BoxMount = (BoxSide, Coord, Coord)

instance Elementable Box BoxMount where
    toElement box _ = Element {
        solid = Cad.box (x box) (y box) (z box),
        antiSolid = emptyModel,
        vis = emptyModel
    }
    getMount box (side, dim1, dim2) = 
        let (d1_len, d2_len) = case side of Px -> (y box, z box)
                                            Nx -> (y box, z box)
                                            Py -> (x box, z box)
                                            Ny -> (x box, z box)
                                            Pz -> (x box, y box)
                                            Nz -> (x box, y box)
            d1 = coordToAbs dim1 d1_len
            d2 = coordToAbs dim2 d2_len
            transform =
                Components.translation( case side of Px -> LA.V3 (x box) d1 d2
                                                     Nx -> LA.V3 0 d1 d2
                                                     Py -> LA.V3 d1 (y box) d2
                                                     Ny -> LA.V3 d1 0 d2
                                                     Pz -> LA.V3 d1 d2 (z box)
                                                     Nz -> LA.V3 d1 d2 0
                    )
                !*!
                rotationTransform(case side of Px -> rotationY 90
                                               Nx -> rotationY (-90)
                                               Py -> rotationX (-90)
                                               Ny -> rotationX 90
                                               Pz -> rotationY 0
                                               Nz -> rotationY (-180)
                )
        in
            transform
    getMetaData _ = emptyMetaData

----------------------------------------------------------

data CylinderMount = 
    Top | Bottom

data Cylinder = Cylinder { 
    radius :: Double,
    height :: Double
} deriving Show 

instance Elementable Cylinder CylinderMount where
    toElement c _ = Element {
        solid = Cad.cylinder (radius c) (height c) f,
        antiSolid = emptyModel,
        vis = emptyModel
    }
    getMount c m =
        case m of Top -> Components.translation $ LA.V3 0 0 (height c)
                  Bottom -> rotationTransform $ rotationY 180
    getMetaData _ = emptyMetaData
                     

----------------------------------------------------------

f :: Cad.Facet
f = Cad.fn 20

screw :: Double -> Double -> (Maybe String, Maybe String, Element)
screw m l = 
    let s = m
        trunk = Cad.translate (0, 0, -l) (Cad.cylinder (m/2) (l+epsillon) f)
        vis = Cad.union [
                Cad.difference (Cad.sphere s f) (
                    Cad.union [
                        Cad.translate (-s, -s, -s) (Cad.box (2.0*s) (2.0*s) s),
                        Cad.translate (-s, -s/4.0/2.0, s/2.0) (Cad.box (s*2) (s/4.0) s),
                        Cad.translate (-s/4.0/2.0, -s, s/2.0) (Cad.box (s/4.0) (s*2) s)
                    ]
                ),
                trunk
            ]
    in (Nothing, Just "screw", Element {
        solid = emptyModel,
        antiSolid = Cad.rotate (0,180,0) trunk,
        vis = Cad.color Cad.yellow $ Cad.rotate (0,180,0) vis
    })

----------------------------------------------------------

widescrew :: Double -> Double -> Double -> (Maybe String, Maybe String, Element)
widescrew m l ll = 
    let s = m
        trunk = Cad.hull [
                Cad.translate (0, ll / 2, -l) (Cad.cylinder (m/2) (l+epsillon) f),
                Cad.translate (0, -ll / 2, -l) (Cad.cylinder (m/2) (l+epsillon) f)
            ] 
    in (Nothing, Nothing, Element {
        solid = emptyModel,
        antiSolid = Cad.rotate (0,180,0) trunk,
        vis = emptyModel
    })


-- ----------------------------------------------------------

pivot :: (Maybe String, Maybe String, Element)
pivot = (Nothing, Just "pivot", Element {
    solid = emptyModel,
    antiSolid = emptyModel,
    vis = 
         Cad.rotate (0, 180, 0) $ Cad.union [
            Cad.color Cad.blue $ Cad.union [
                Cad.cylinder 1 10 f,
                Cad.translate (0,0,10) (Cad.obCylinder 2 5 0 f)
            ],
            Cad.rotate (0,90,0) $ Cad.color Cad.red $ Cad.union [
                Cad.cylinder 1 10 f,
                Cad.translate (0,0,10) (Cad.obCylinder 2 5 0 f)
            ],
            Cad.rotate (-90,0,0) $ Cad.color Cad.green $ Cad.union [
                Cad.cylinder 1 10 f,
                Cad.translate (0,0,10) (Cad.obCylinder 2 5 0 f)
            ]
        ]
})

-- ----------------------------------------------------------

instance Elementable (Maybe String, Maybe String, Element) () where
    toElement (solidGroup, visGroup, e) params = 
        let (s, as) = case solidGroup of 
                Just g -> if g `elem` solidGroups params then (solid e, antiSolid e) else (emptyModel, emptyModel)
                _ -> (solid e, antiSolid e)
            v = case visGroup of 
                Just g -> if g `elem` visGroups params then vis e else emptyModel
                _ -> vis e
        in Element {
            solid = s,
            antiSolid = as,
            vis = v
        }
    getMount _ _ = zeroMount
    getMetaData (solidGroup, visGroup, _) = MetaData {
        mSolidGroups = case solidGroup of Just g -> Data.Set.fromList [g]
                                          _ -> Data.Set.fromList [],
        mVisGroups = case visGroup of Just g -> Data.Set.fromList [g]
                                      _ -> Data.Set.fromList []
    }
    
    

-- ----------------------------------------------------------

-- data TrvialElementable = TrvialElement { 
--     element :: Element,
--     mountPoint :: Mount
-- } deriving Show

-- instance Elementable TrvialElementable () where
--     toElement model = element model
--     getMount model _ = mountPoint model

-- ----------------------------------------------------------

averageMount :: [Mount] -> ([(M33 Double, LA.V3 Double, LA.V3 Double)] -> (M33 Double, LA.V3 Double, LA.V3 Double)) -> Mount
averageMount ms s = 
    let ds = Prelude.map Math.decomposeMatrix ms
        l  :: Double = fromIntegral $ length ms
        xs = sum (Prelude.map (\(_, _, LA.V3 x _ _) -> x) ds) / l
        ys = sum (Prelude.map (\(_, _, LA.V3 _ y _) -> y) ds) / l
        zs = sum (Prelude.map (\(_, _, LA.V3 _ _ z) -> z) ds) / l
        (d, _, _) = s ds
    in LA.mkTransformationMat d (LA.V3 xs ys zs)

-- ----------------------------------------------------------

modelize :: Elementable elementType mountType => SynthesisParameters -> mountType -> elementType -> Cad.Model Cad.Vector3d
modelize params m e = 
    let
        element = toElement e params @@ invMount (getMount e m)
        v = vis element
    in 
        Cad.rotate (180, 0, 0) $ Cad.union [Cad.difference (solid element) (antiSolid element), v]

-- inspect :: Elementable elementType mountType => elementType -> Cad.Model Cad.Vector3d
-- inspect e = 
--     let
--         element = toElement e
--         vis = Prelude.map snd (vis element)
--     in 
--         Cad.union $ Cad.difference (solid element) (antiSolid element) : vis 

-- ----------------------------------------------------------

remount :: Ord a => Elementable elementType mountType => elementType -> Map a Mount -> ElementableWrapper a
remount e m =
    ElementableWrapper {
        wElement = toElement e,
        wMount = (m !),
        wMeta = getMetaData e
    }

-- ----------------------------------------------------------

data GridMount em = Center | Cell Int Int em

data Grid e em = Grid {
    xn :: Int,
    yn :: Int,
    xSpacing :: Double,
    ySpacing :: Double,
    zF :: (Int, Int) -> Double,
    cellMount :: (Int, Int) -> em,
    cell :: (Int, Int) -> e
}

gridIndices grid = [(x,y) | x <- [0..xn grid - 1], y <- [0..yn grid - 1]]

newtype GridConstructor e em = GridConstructor{ g :: Grid e em}

instance Elementable (GridConstructor e em) (Int, Int) where
    getMount gc (x, y) = Components.translation (LA.V3 (fromIntegral x * xSpacing (g gc)) 
                                                        (fromIntegral y * ySpacing (g gc))
                                                        0)
    toElement _ = const emptyElement
    getMetaData = const emptyMetaData

instance (Elementable e em) => Elementable (Grid e em) (GridMount em) where
    getMount grid gm = 
        -- fixe: maybe use << to implement getMount computation
        let xc :: Double = (fromIntegral (xn grid) - 1) * xSpacing grid / 2
            yc :: Double = (fromIntegral (yn grid) - 1) * ySpacing grid / 2
            rtY = rotationTransform $ rotationY 180
            inv_child_mount x y = rotationTransform (rotationY 180) !*! rotationTransform (rotationZ $ zF grid (x,y)) !*! invMount (getMount (cell grid (x, y)) (cellMount grid (x, y)))
            m = case gm of Center -> Components.translation (LA.V3 xc yc 0) !*! rtY
                           Cell x y m -> getMount GridConstructor {g = grid} (x, y)
                                         !*! inv_child_mount x y
                                         !*! getMount (cell grid (x, y)) m
        in m 

    toElement grid = 
                let f :: ElementableWrapper (Int, Int) -> (Int, Int) -> ElementableWrapper (Int, Int) =
                        \ partial m -> partial << (m, cellMount grid m, zF grid m, L, cell grid m)
                    result = Prelude.foldl
                                f
                                (wrap $ GridConstructor {g = grid})
                                (gridIndices grid)
                in toElement result

    getMetaData grid = getMetaData $ cell grid (0, 0)


-- ----------------------------------------------------------

connector :: Double -> Double -> Mount -> Mount -> SynthesisParameters -> Element
connector w h m1 m2 _ = Element {
        solid = let b = Cad.box h w 0.001
                 in Cad.hull [
                       applyMount m1 $ Cad.translate (-h/2, -w/2, 0) b,
                       applyMount m2 $ Cad.translate (-h/2, -w/2, 0) b
                    ],
        antiSolid = emptyModel,
        vis = emptyModel
    }

-- ----------------------------------------------------------

data SolidGroup e em = SolidGroup{
    groupName :: String,
    element :: e
}

instance (Elementable e em) => Elementable (SolidGroup e em) em where
    toElement sg params = 
        if groupName sg `elem` solidGroups params then toElement (element sg) params else emptyElement
    getMount sg = getMount $ element sg
    getMetaData sg = joinMetaData (getMetaData $ element sg) MetaData {
            mSolidGroups = Data.Set.fromList [groupName sg],
            mVisGroups = Data.Set.Empty
        }

-- ----------------------------------------------------------

isolate :: (Elementable e em) => e -> ElementableWrapper em
isolate e = ElementableWrapper {
            wElement = \p ->
                let element = toElement e p
                in Element {
                        solid = Cad.difference (solid element) (antiSolid element),
                        antiSolid = emptyModel,
                        vis = vis element
                    },
            wMount = getMount e,
            wMeta = getMetaData e
        }