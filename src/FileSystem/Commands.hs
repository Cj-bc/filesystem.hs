module FileSystem.Commands
(mkdir
, ls
)

where
import FileSystem.Internal (Name, FSZipper(..), FSItem(..))


-- | Create new directory
mkdir :: Name -> FSZipper -> Maybe FSZipper
mkdir _ (File _ _, _) = Nothing
mkdir n (Directory name fs, bs) = Just (Directory name (Directory n []:fs), bs)


-- | Shows entries in current directory
-- --
ls :: FSZipper -> IO ()
ls (Directory _ fs, _) = mapM_ putStrLn $ map showItem fs
    where
        showItem (Directory name _) = "d " ++ name
        showItem (File name _) = name
