module FileSystem
( FSItem(..)
, ls
, cd
, mkdir
)
where

import  Data.List.Split (splitOn)
import  FileSystem.Internal



-- | Move to given path
-- This works as sh builtin 'cd' command (I hope)
cd :: Path -> FSZipper -> Maybe FSZipper
cd ('.':'.':ps) z   = fsUp z >>= cd ps
cd ('/':ps) z       = fsUpToRoot z >>= cd ps
    where
        fsUpToRoot :: FSZipper -> Maybe FSZipper
        fsUpToRoot (item, []) = Just (item, [])
        fsUpToRoot z          = fsUp z
cd ps z             = cdTo (splitOn "/" ps) z
    where
        cdTo :: [Path] -> FSZipper -> Maybe FSZipper
        cdTo [] z = Just z
        cdTo ("..":xs) z = fsUp z >>= cdTo xs
        cdTo (x:xs) z = fsTo x z >>= cdTo xs
