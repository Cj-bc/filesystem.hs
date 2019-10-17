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
ls :: FSZipper -> Either FSError (FSZipper, [String])
ls (File _ _, _) = Left OperationNotAllowed
ls z@(Directory _ fs, _) = Right (z, map showItem fs)
    where
        showItem (Directory name _) = "d " ++ name
        showItem (File name _) = name
