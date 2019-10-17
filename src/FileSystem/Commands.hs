module FileSystem.Commands
(mkdir
, ls
)

where
import FileSystem.Internal (Name, FSZipper(..), FSItem(..), FSError(..))


-- | Create new directory
mkdir :: Name -> FSZipper -> Either FSError FSZipper
mkdir _ (File _ _, _)           = Left  OperationNotAllowed
mkdir n (Directory name fs, bs) = Right (Directory name (Directory n []:fs), bs)


-- | Shows entries in current directory
-- --
ls :: FSZipper -> IO ()
ls (Directory _ fs, _) = mapM_ putStrLn $ map showItem fs
    where
        showItem (Directory name _) = "d " ++ name
        showItem (File name _) = name
