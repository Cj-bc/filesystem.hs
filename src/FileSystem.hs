module My.Playground.FileSystem
( sampleDisk
, FSItem(..)
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


-- | Shows entries in current directory
--
ls :: FSZipper -> IO ()
ls (Directory _ fs, _) = mapM_ putStrLn $ map showItem fs
    where
        showItem (Directory name _) = "d " ++ name
        showItem (File name _) = name


-- | Create new directory
mkdir :: Name -> FSZipper -> Maybe FSZipper
mkdir _ (File _ _, _) = Nothing
mkdir n (Directory name fs, bs) = Just (Directory name (Directory n []:fs), bs)
