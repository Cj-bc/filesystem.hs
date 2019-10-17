module FileSystem
( FSItem(..)
, FSZipper
, cd
)
where

import  Data.List.Split (splitOn)
import  FileSystem.Internal



-- | Move to given path
-- This works as sh builtin 'cd' command (I hope)
cd :: Path -> FSZipper -> Either FSError FSZipper
cd ('.':'.':ps) z   = fsUp z >>= cd ps
cd ('/':ps) z       = fsUpToRoot z >>= cd ps
cd ps z             = cdTo (splitOn "/" ps) z


cdTo :: [Path] -> FSZipper -> Either FSError FSZipper
cdTo [] z = Right z
cdTo ("..":xs) z = fsUp z >>= cdTo xs
cdTo (x:xs) z = fsTo x z >>= cdTo xs

fsUpToRoot :: FSZipper -> Either FSError FSZipper
fsUpToRoot (item, []) = Right (item, [])
fsUpToRoot z          = fsUp z
