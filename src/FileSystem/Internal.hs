module FileSystem.Internal
( Name, Data, Path
, FSItem(..) , FSZipper(..), FSError(..)
, fsUp, fsTo, fsRename, fsNewFile
)
where

type Name = String
type Data = String
type Path = String
data FSItem = Directory Name [FSItem]
            | File Name Data deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])
data FSError = FileNotFound
             | OperationNotAllowed
             | CommandNotFound
             | ExitCalled deriving (Show)


fsUp :: FSZipper -> Either FSError FSZipper
fsUp (_, []) = Left OperationNotAllowed
fsUp (i, FSCrumb name prevItems nextItems:cs)
    = Right (Directory name (prevItems ++ [i] ++ nextItems), cs)


fsTo :: Name -> FSZipper -> Either FSError FSZipper
fsTo _      (File _ _, _) = Left OperationNotAllowed
fsTo target (Directory basename items, bs)
    | doesDirExist = let (before, i:after) = break (nameIs target) items
                     in Right (i, FSCrumb basename before after:bs)
    | otherwise    = Left OperationNotAllowed
    where
        doesDirExist = True `elem` map (isDir target) items


isDir :: Name -> FSItem -> Bool
isDir _ (File _ _) = False
isDir target (Directory name _) = name == target

nameIs :: Name -> FSItem -> Bool
nameIs target (Directory name _) = name == target
nameIs target (File name _) = name == target

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Directory _ items, bs) = (Directory n items, bs)
fsRename n (File _ c, bs) = (File n c, bs)

fsNewFile :: FSItem -> FSZipper -> Either FSError FSZipper
fsNewFile _ (File _ _, _) = Left OperationNotAllowed
fsNewFile item (Directory dirname cs, bs) = Right (Directory dirname (item:cs), bs)
