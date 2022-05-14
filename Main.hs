
module Main where
import BananaHolder
import Hascad
import Graphics.OpenSCAD
import Data.Type.Coercion (trans)

import Components
import Maths
import qualified Data.Bits as OpenSCAD
import qualified Graphics.OpenSCAD as OpenSCAD
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment   
import Data.List  
import Data.Set


b = (Nz, (Rel, Middle, 0.0), (Rel, Middle, 0))
top = (Pz, (Rel, Middle, 0.0), (Rel, Middle, 0))

-- main :: IO ()
-- main = draw $ modelize True b $
--    e << (s, s, 0, L, e)

-- main :: IO ()
-- main = 
--     let t = 2.5

--         plank = Box {
--             x = 50,
--             y = 2.5,
--             z = t
--         }

--         fill = Box {
--             x = 5,
--             y = 2.5,
--             z = t
--         }

--         leg1 = Box {
--             x = 10,
--             y = 20,
--             z = t
--         }

--         leg2 = Box {
--             x = 10,
--             y = 60,
--             z = t
--         }

--         edgeMount = (Py, (Abs, Low, 0), (Rel, Middle, 0))
--         legMount1 = (Px, (Rel, Middle, 0), (Rel, Middle, 0))
--         legMount2 = (Nx, (Rel, Middle, 0.15), (Rel, Middle, 0))
--         screwMount1 = (Nz, (Abs, High, -5), (Abs, Middle, -25))
--         screwMount2 = (Nz, (Abs, High, -5), (Abs, Middle, 25))


--         component = fill << (edgeMount, edgeMount, 180, id, plank)
--                          << (L (Pz, (Rel, Middle, 0), (Rel, High, 0)), (), 0, L, screw 3 5)
--         m1 = L (Ny, (Rel, Low, 0.25), (Rel, Middle, 0))
--         m2 = R (Px, (Rel, Middle, 0), (Rel, Middle, 0))
--         ring = Components.repeat component m1 m2 90 4

--         lm1 = (0, R (Nx, (Rel, Low, 0.0), (Rel, Middle, 0)))
--         lm2 = (3, R (Ny, (Rel, Low, 0.0), (Rel, Middle, 0)))

--         tring = remount ring $ Map.fromList [((), averageMount [get_mount ring lm1, get_mount ring lm2] head)]

--     in draw $ modelize False legMount1 $ 
--         tring << ((), legMount1, 0, R, leg1) << (legMount2, legMount1, 0, R, leg2) << (screwMount1, (), 0, L, screw 3 5) << (screwMount2, (), 0, L, screw 3 5)


test = let bb = Box {x = 10, y = 10, z = 10}
           box_ze_sruba = 
            bb
                << ((Pz, (Rel, Low, 0.25), (Rel, Low, 0.25)), (), 0, L, pivot)
                << ((Pz, (Rel, Low, 0.25), (Rel, Low, 0.25)), (), 0, L, screw 2 200)

           box_ze_sruba2 = isolate box_ze_sruba

           box = box_ze_sruba << (
                    (Nz, (Rel, Middle, 0), (Rel, Middle, 0.25)),
                    (Pz, (Rel, Middle, 0), (Rel, Middle, 0.25)), 0, R, Box {x = 10, y = 0.0, z = 10})

           box2 = box << (
                    (Nz, (Rel, Middle, 0), (Rel, Middle, 0.25)),
                    (Pz, (Rel, Middle, 0), (Rel, Middle, 0.25)), 0, R, bb)
       in SolidGroup { groupName = "Test", element = box2}


boardToHolder = let t = 2.5
                    plank = Box {x = 10, y = 82.5, z = t}
                    short_plank = Box {x = 10, y = 72.5, z = t}
                    plank_side = Box { x = t, y = 72.5, z = 20.5-(2*t)}
                    wscrew_mount = (Pz, (Rel, High, 0), (Rel, Middle, 0))
                    plank_bottom_top = (Pz, (Abs, Low, t/2), (Rel, Middle, 0))
                    plank_top_bottom = (Nz, (Abs, Low, t/2), (Rel, Middle, 0))
                    lscrew = (Pz, (Abs, High, -2.5), (Abs, Low, 2.5))
                    rscrew = (Pz, (Abs, High, -2.5), (Abs, High, -2.5))
                    topmount1 = (Pz, (Rel, Middle, 0.0), (Abs, Low, 10))
                    topmount2 = (Pz, (Rel, Middle, 0.0), (Abs, Low, 60))

                 in 
                    SolidGroup { groupName = "BoardToHolder", element = 
                          plank << (b, (), 90, L, pivot) << 
                          (wscrew_mount, (), 0, L, widescrew (20-t*2) 5 57) <<
                          (lscrew, (), 0, L, screw 3 5) <<
                          (rscrew, (), 0, L, screw 3 5) <<
                          (plank_bottom_top, b, 0, R, plank_side) << 
                          (top, plank_top_bottom, 0, R, short_plank) <<
                          (topmount1, (), 0, L, widescrew 3 5 10) <<
                          (topmount2, (), 0, L, widescrew 3 5 10)
                    }

psuToFan = let  t = 3

                plank = Box {
                    x = 50,
                    y = 2.5,
                    z = t
                }

                fill = Box {
                    x = 5,
                    y = 2.5,
                    z = t
                }

                edgeMount = (Py, (Abs, Low, 0), (Rel, Middle, 0))
                component = fill << (edgeMount, edgeMount, 180, id, plank)
                                << (L (Pz, (Rel, Middle, 0), (Rel, High, 0)), (), 0, L, screw 3 5)
                                
                m1 = L (Ny, (Rel, Low, 0.25), (Rel, Middle, 0))
                m2 = R (Px, (Rel, Middle, 0), (Rel, Middle, 0))

                ring = Components.repeat component m1 m2 90 4
                center = averageMount [getMount ring (0, L b), getMount ring (2, L b)] head
                trivial_ring = remount ring $ Map.fromList [
                        (Just (0, 0), getMount ring (2, R (Nx, (Abs, Low, 10.0), (Rel, Middle, 0)))),
                        (Just (0, 1), getMount ring (1, R (Ny, (Abs, Low, 10.0), (Rel, Middle, 0)))),
                        (Just (1, 0), getMount ring (3, R (Ny, (Abs, Low, 10.0), (Rel, Middle, 0)))),
                        (Just (1, 1), getMount ring (0, R (Nx, (Abs, Low, 10.0), (Rel, Middle, 0)))),
                        (Nothing, center)
                    ]

                column_radius = 2.3

                armsEnd = R (Px, (Abs, Middle, 0), (Abs, Middle, 0))

                (arm_top, arm_top_mount) = 
                    (
                        Box{x = 14, y = column_radius * 2, z = t}, -- 84
                        (Nz, (Abs, Low, column_radius), (Rel, Middle, 0))
                    )

                arm = Cylinder{radius=column_radius, height = 29}
                        << (Bottom, (), 0, L, screw 3.12 35)
                        << (Top, arm_top_mount, 0, id, arm_top)


                grid = Grid {
                    xn = 2,
                    yn = 2,
                    xSpacing = 124, -- 214
                    ySpacing = 76.5,-- 46.5
                    zF = \(x, _) -> if x == 0 then 180 else 0,
                    cellMount = const $ L Top,
                    cell = const arm
                }

                grid_and_ring = grid << (Center, Nothing, 0, id, trivial_ring)
                whole = Prelude.foldl
                        (\ partial (x,y) -> partial <<+ connector t t (getMount grid_and_ring (R $ Just (x,y))) (getMount grid (Cell x y armsEnd)))
                        (wrap grid_and_ring)
                        (gridIndices grid)

            -- in draw $ inspect arm2
            in SolidGroup { groupName = "PsuToFan", element = whole } 

mnt :: (BoxSide, (Units, Origin, Double), (Units, Origin, Double))
mnt = b

main :: IO ()
main = 
      let totalModel = 
              Box {x = 0, y = 0, z = 0} 
                                        << (top, b, 0, L, boardToHolder)
                                        << (top, L Center, 0, L, psuToFan)
                                        << (top, b, 0, L, test)
          metadata = getMetaData totalModel
    
      in do
            args <- getArgs

            if "--info" `elem` args 
            then do                  
                  print $ Data.Set.toList $ mSolidGroups metadata
                  print $ Data.Set.toList $ mVisGroups metadata
            else do
                  let params = SynthesisParameters {
                              solidGroups = words $ head args,
                              visGroups = words $ args !! 1,
                              release = "--release" `elem` args 
                        }
                  draw $ modelize params b totalModel 