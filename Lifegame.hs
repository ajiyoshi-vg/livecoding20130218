module Lifegame
(
World
) where

-- | 座標を表すデータ
-- >>> Point 1 2
-- Point 1 2
data Point = Point Int Int deriving(Show, Eq)


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

-- | あるPointが生きているか返す
-- >>> let world = AliveList [Point 1 2]
-- >>> alive world (Point 1 2)
-- True
--
-- >>> alive world (Point 1 3)
-- False
instance World AliveList where
    alive (AliveList ls) pt = pt `elem` ls


