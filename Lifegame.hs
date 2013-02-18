module Lifegame
(
World
, display
, surround
, willBeBorn
, willBeAlive
, willBeDead
) where

import System.Posix
import Data.List

-- $setup
-- >>> let d22 = display [0..2] [0..2] :: AliveList -> IO() 

-- | 座標を表すデータ
-- >>> Point 1 2
-- Point 1 2
data Point = Point Int Int deriving(Show, Eq, Ord)


-- | ある座標の周りにある座標のリスト
-- >>> surround (Point 1 2)
-- [Point 0 1,Point 1 1,Point 2 1,Point 0 2,Point 2 2,Point 0 3,Point 1 3,Point 2 3]
--
-- >>> surround (Point 2 2)
-- [Point 1 1,Point 2 1,Point 3 1,Point 1 2,Point 3 2,Point 1 3,Point 2 3,Point 3 3]
--
surround :: Point -> [Point]
surround (Point x y) = [ Point a b | 
    b <- [y-1, y, y+1],
    a <- [x-1, x, x+1],
    a/=x || b/=y ]

-- | 生存しているセルのリストで、世界全体を表現する
-- （とりあえずの仮実装）
-- >>> AliveList [Point 1 3,Point 2 2]
-- AliveList [Point 1 3,Point 2 2]
data AliveList = AliveList [Point] deriving(Show)

class World a where
    alive :: a -> Point -> Bool
    next :: a -> a

-- | あるPointが生きているか返す
-- >>> let world = AliveList [Point 1 2]
-- >>> alive world (Point 1 2)
-- True
--
-- >>> alive world (Point 1 3)
-- False
--
-- >>> let w = AliveList [Point 0 1,Point 0 2,Point 1 2]
-- >>> display [0..2] [0..2] w
-- oo_
-- o__
-- ___
--
-- >>> display [0..2] [0..2] (next w)
-- oo_
-- oo_
-- ___
--
-- >>> let w = AliveList [Point 1 1,Point 0 2,Point 1 2]
-- >>> display [0..2] [0..2] w
-- oo_
-- _o_
-- ___
--
-- >>> d22 (next w)
-- oo_
-- oo_
-- ___
--
-- >>> let w3 = AliveList [Point 0 1,Point 1 1,Point 0 2,Point 1 2,Point 2 2]
-- >>> display [0..2] [0..3]  w3
-- ___
-- ooo
-- oo_
-- ___
-- 
-- >>> display [0..2] [0..3] $ next w3
-- _o_
-- o_o
-- o_o
-- ___
--
-- >>> next w3
-- AliveList [Point 0 1,Point 0 2,Point 1 3,Point 2 1,Point 2 2]
--
-- >>> let w4 = AliveList [Point 1 1,Point 2 1]
-- >>> d22 w4
-- ___
-- _oo
-- ___
--
-- >>> d22 $ next w4
-- ___
-- ___
-- ___
instance World AliveList where
    alive (AliveList ls) pt = pt `elem` ls
    next w@(AliveList ls) = AliveList nextList where
        baby = filter (willBeBorn w) $
            filter (not . alive w) $ 
            concatMap surround ls
        stay = filter (willBeAlive w) ls
        uniq = map head . group . sort
        nextList = uniq (baby ++ stay)

run :: World a => [Int] -> [Int] -> a -> Int -> IO ()
run _ _ _ 0 = return ()
run xs ys world num = do
    putStr "\ESC[2J"
    display xs ys world
    usleep 1000000
    run xs ys (next world) (num-1)


-- | 盤面を表示する
-- >>> let world = AliveList [Point 1 2]
-- >>> display [0..3] [0..2] world
-- _o__
-- ____
-- ____
--
-- >>> let world2 = AliveList [Point 1 2, Point 2 2]
-- >>> display [0..3] [0..2] world2
-- _oo_
-- ____
-- ____
--
-- >>> display [0..5] [0..2] world2
-- _oo___
-- ______
-- ______
display :: World a => [Int] -> [Int] -> a -> IO ()
display xs ys w = 
    mapM_ (putStrLn . lineAt) ys'
        where
            ys' = reverse ys
            lineAt y = map ptToChar [Point a y | a <- xs ]
            ptToChar pt 
                | alive w pt = 'o'
                | otherwise  = '_'

-- FIXME 適切な名前を考える。Bornって形容詞じゃないか？
-- | ある座標が、次回「誕生」するか
-- >>> let w = AliveList [Point 0 1,Point 0 2,Point 1 2]
-- >>> display [0..2] [0..2] w
-- oo_
-- o__
-- ___
--
-- >>> willBeBorn w (Point 1 1)
-- True
--
-- >>> willBeBorn w (Point 2 2)
-- False
willBeBorn :: World a => a -> Point -> Bool
willBeBorn w pt = num == 3 where
    num = length $ filter (alive w) $ surround pt

-- | 次の状態で、あるポイントが生存するかどうか
-- >>> let w = AliveList [Point 1 1,Point 0 2,Point 1 2]
-- >>> display [0..2] [0..2] w
-- oo_
-- _o_
-- ___
--
-- >>> willBeAlive w (Point 1 1)
-- True
--
-- >>> let w2 = AliveList []
-- >>> display [0..3] [0..3] w2
-- ____
-- ____
-- ____
-- ____
--
-- >>> willBeAlive w2 (Point 1 1)
-- False
willBeAlive :: World a => a -> Point -> Bool
willBeAlive world pt = num == 2 || num == 3 where
    num = length $ filter (alive world) $ surround pt 

-- | 死ぬケース
-- >>> let w = AliveList [Point 1 1, Point 2 1]
-- >>> willBeDead w (Point 1 1)
-- True
--
-- >>> let w2 = AliveList [Point 0 1,Point 1 1,Point 0 2,Point 1 2,Point 2 2]
-- >>> display [0..2] [0..2] w2
-- ooo
-- oo_
-- ___
-- 
-- >>> willBeDead w2 (Point 1 1)
-- True
willBeDead :: World a => a -> Point -> Bool
willBeDead w pt = not $ willBeAlive w pt 

