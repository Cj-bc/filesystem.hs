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

fsUp :: FSZipper -> Maybe FSZipper
fsUp (_, []) = Nothing
fsUp (i, FSCrumb name prevItems nextItems:cs)
    = Just (Directory name (prevItems ++ [i] ++ nextItems), cs)


fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo _ (File _ _, _) = Nothing
fsTo target (Directory basename items, bs)
    | True `elem` map (nameIs target) items
        = let (before, i:after) = break (nameIs target) items
          in Just (i, FSCrumb basename before after:bs)
    | otherwise = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs target (Directory name _) = name == target
nameIs target (File name _) = name == target

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Directory _ items, bs) = (Directory n items, bs)
fsRename n (File _ c, bs) = (File n c, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Directory dirname cs, bs) = (Directory dirname (item:cs), bs)
