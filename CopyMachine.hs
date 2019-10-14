import System.Directory
import System.Environment   
import System.FilePath (takeFileName, (</>))
import System.IO
import System.IO.Error

type ListZipper a = ([a],[a])  

type Args = Maybe Int

right :: ListZipper a -> ListZipper a  
right (x:xs, bs) = (xs, x:bs)  
  
left :: ListZipper a -> ListZipper a  
left (xs, b:bs) = (b:xs, bs)  

start :: ListZipper a -> ListZipper a  
start (xs, []) = (xs,[])
start z = start (left z)

modify :: a -> ListZipper a -> ListZipper a
modify x (y:ys, bs) = (x:ys, bs)



main = do
    (fileToCopy:destination:depth:numberOfCopies:xs) <- getArgs
    initCopyMachine fileToCopy [Nothing, Nothing] destination
    
initCopyMachine :: FilePath -> [Maybe Int] -> FilePath -> IO ()
initCopyMachine fileToCopy args destination = copyFiles fileToCopy args destination


copyFiles :: FilePath -> [Maybe Int] -> FilePath -> IO ()
copyFiles fileToCopy args destination = catchIOError (copyFiles_ fileToCopy args destination) (handler fileToCopy destination)
    where   copyFiles_ :: FilePath -> [Maybe Int] -> FilePath -> IO ()
            copyFiles_ fileToCopy args destination = do 
                absoluteDestination <- makeAbsolute destination
                destinationContents <- listDirectory absoluteDestination
                let destinationFolders = checkDirs destinationContents
                let fileName = takeFileName fileToCopy
                actionWithArguments fileToCopy absoluteDestination args
                let dirPath = absoluteDestination ++ "/"
                if (destinationFolders == Nothing) || predicateWithArguments args
                    then return ()
                    else 
                    let (Just dirs) = destinationFolders 
                    in mapM_ (copyFiles fileToCopy args) (map (dirPath ++) dirs) 
                    
actionWithArguments :: FilePath -> FilePath -> ListZipper Args -> IO (ListZipper Args)
actionWithArguments fileToCopy absoluteDestination (_:Nothing:_) = do
    copyFile fileToCopy $ absoluteDestination </> (takeFileName fileToCopy)
    putStrLn $ "copied " ++ (takeFileName fileToCopy) ++ " to " ++ absoluteDestination

actionWithArguments fileToCopy absoluteDestination (_:(Just numberOfCopies):_) = do 
    multiCopy fileToCopy absoluteDestination $ numberOfCopies 
    putStrLn $ "copied " ++ (takeFileName fileToCopy) ++ " to " ++ absoluteDestination

predicateWithArguments :: [Maybe Int] -> Bool
predicateWithArguments (Nothing:_) = False
predicateWithArguments ((Just depth):_) = depth < 1


multiCopy :: FilePath -> FilePath -> Int -> IO ()
multiCopy fileToCopy destination numberOfCopies = do
    absoluteDestination <- makeAbsolute destination
    copyFile fileToCopy (absoluteDestination </> (mCPrompt numberOfCopies fileName))
    multiCopy fileToCopy destination $ numberOfCopies - 1
    where fileName = takeFileName fileToCopy

mCPrompt :: Int -> String -> String
mCPrompt  0 fileName = fileName
mCPrompt  1 fileName = fileName ++ " - копия "
mCPrompt  n fileName = fileName ++ " - копия (" ++ show n ++ ")"
   
handler :: FilePath -> FilePath -> IOError -> IO ()
handler _ _ e = ioError e

isDir :: FilePath -> Bool
isDir [] = True
isDir (x:xs) = isDir xs && if x == '.' then False else True

checkDirs :: [FilePath] -> Maybe [FilePath]
checkDirs [] = Nothing
checkDirs x = if filtered == [] then Nothing else Just filtered
    where filtered = filter (isDir) x



{-copyFilesWithDepth :: FilePath -> Int -> FilePath -> IO ()
copyFilesWithDepth fileToCopy depth destination = catchIOError (copyFilesWithDepth_ fileToCopy depth destination) (handler fileToCopy destination)
    where   copyFilesWithDepth_ :: FilePath -> Int -> FilePath -> IO ()
            copyFilesWithDepth_ fileToCopy depth destination = do 
                absoluteDestination <- makeAbsolute destination
                destinationContents <- listDirectory absoluteDestination
                let destinationFolders = checkDirs destinationContents
                let fileName = takeFileName fileToCopy
                copyFile fileToCopy $ absoluteDestination </> fileName
                putStrLn $ "copied " ++ fileName ++ " to " ++ absoluteDestination
                let dirPath = absoluteDestination ++ "/"
                if (destinationFolders == Nothing) || ( depth < 1)
                    then return ()
                    else 
                    let (Just dirs) = destinationFolders 
                    in mapM_ (copyFilesWithDepth fileToCopy (depth - 1)) (map (dirPath ++) dirs) 


copyFilesMultiCopy :: FilePath -> Int -> Int -> FilePath -> IO ()
copyFilesMultiCopy fileToCopy depth numberOfCopies destination = catchIOError (copyFiles_ fileToCopy depth destination) (handler fileToCopy destination)
    where   copyFilesMultiCopy_ :: FilePath -> Int -> Int -> FilePath -> IO ()
            copyFilesMultiCopy_ fileToCopy depth numberOfCopies destination = do 
                absoluteDestination <- makeAbsolute destination
                destinationContents <- listDirectory absoluteDestination
                let destinationFolders = checkDirs destinationContents
                let fileName = takeFileName fileToCopy
                multiCopy fileToCopy (absoluteDestination </> fileName) $ mCPrompt numberOfCopies
                putStrLn $ "copied " ++ fileName ++ " to " ++ absoluteDestination
                let dirPath = absoluteDestination ++ "/"
                if (destinationFolders == Nothing) || ( depth < 1)
                    then return ()
                    else 
                    let (Just dirs) = destinationFolders 
                    in mapM_ (copyFilesMultiCopy fileToCopy (depth - 1) numberOfCopies) (map (dirPath ++) dirs) 
-}