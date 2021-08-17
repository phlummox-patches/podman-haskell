{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

A demo program.

Assume we're running bash, and have the following setup (for the sake
of example):

@
alias mypod="podman-remote --url tcp://localhost:3000/"
img=alpine:3.12.0
ctr=my-ctr
@

Then the demo program is roughly equivalent to running the following Bash
commands (see the 'demo' function):

@
mypod version

if ! mypod image exists $img ; then
  mypod pull $img
fi

mypod image tree $img

if ! mypod container exists $ctr; then
  mypod container create --name $ctr -i -t $img
fi

mypod container inspect $ctr
mypod container start $ctr
mypod container wait --condition=running $ctr
mypod container attach $ctr
mypod exec $ctr cat /etc/os-release
fi

echo test-data > test.dat
mypod cp test.dat $ctr:/tmp/test-send
mypod cp $ctr:/tmp/test-send test.dat

mypod logs $ctr
mypod kill $ctr
mypod container wait --condition=exited $ctr
mypod rm $ctr
@

-}


module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Monad
import Control.Monad.IO.Class   (MonadIO (..))

import            Data.ByteString.Lazy (ByteString)
import            Data.Either (fromRight)
import qualified  Data.List as L
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as T

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Podman

-- | whether there is an end-user/tty, and we
-- want to run the "attach" portion of the demo
data Interactive = Interactive | NonInteractive
  deriving Eq

-- | Ensure an image is available
ensureImage :: MonadIO m => PodmanClient -> ImageName -> m ()
ensureImage client iname@(ImageName name) = do
  trace "imageExists" iname
  existsError <- imageExists client iname
  case existsError of
    Left err -> abort "imageExists" err
    Right True -> ok "image exists" True
    Right False -> ok "image missing" False >> pullImage
  where
    pullImage = do
      trace "imagePull" iname
      pullResult <- imagePull client (mkImagePullQuery name)
      case pullResult of
        Right x -> ok "image pulled" x
        Left err -> abort "imagePull" err

-- | Show image list
listImages :: MonadIO m => PodmanClient -> m ()
listImages client = do
  trace "imageList" defaultImageListQuery
  xs <- imageList client defaultImageListQuery
  case xs of
    Left err -> abort "imageList" err
    Right xs' -> ok "image list" (head xs')

-- | Show an image
showImage :: MonadIO m => PodmanClient -> ImageName -> m ()
showImage client iname = do
  trace "imageTree" iname
  res <- imageTree client iname (Just True)
  case res of
    Left err -> abort "imageTree" err
    Right x -> ok "image tree" x

-- | Ensure a container is running
ensureContainer :: forall m . MonadIO m =>
    PodmanClient -> ImageName -> ContainerName -> m ()
ensureContainer client (ImageName iname) containerName@(ContainerName cname) = do
  trace "containerExists" cname
  existsError <- containerExists client containerName
  case existsError of
    Left err -> abort "containerExists" err
    Right True -> ok "container exists" True
    Right False -> ok "container missing" False >> createContainer
  trace "containerInspect" cname
  inspect <- containerInspect client containerName False
  case inspect of
    Left err -> abort "containerInspect" err
    Right x
      | _inspectContainerStateRunning (_inspectContainerResponseState x) -> ok "container running" x
      | otherwise -> ok "container not running" x >> startContainer
  trace "containerWait" cname
  wait <- containerWait client containerName Running
  case wait of
    Left err -> abort "containerWait" err
    Right x -> ok "container is running" x
  where

    startContainer :: m ()
    startContainer = do
      trace "containerStart" cname
      res <- containerStart client containerName Nothing
      case res of
        Nothing -> ok "container started" True
        Just err -> abort "containerStart" err

    createContainer :: m ()
    createContainer = do
      trace "containerCreate" cname
      res <-
        containerCreate
          client
          ( (mkSpecGenerator iname)
              { _specGeneratorname = Just cname,
                _specGeneratorstdin = Just True,
                _specGeneratorterminal = Just True
              }
          )
      case res of
        Left err -> abort "containerCreate" err
        Right x -> ok "container created" x

-- | Exec a command in container
execContainer :: MonadIO m => PodmanClient -> ContainerName -> [Text] -> m ()
execContainer client name args = do
  trace "execCreate" args
  res <- execCreate client name ((mkExecConfig args) {_execConfigAttachStdout = Just True})
  case res of
    Left err -> abort "execCreate" err
    Right execId -> ok "exec created" execId >> inspectExec execId >> startExec execId
  where
    inspectExec execId = do
      trace "execInspect" execId
      res <- execInspect client execId
      case res of
        Left err -> abort "execInspect" err
        Right resp -> ok "execInspect" resp
    startExec execId = do
      trace "execStart" execId
      res <- execStart client execId
      case res of
        Left err -> abort "execStart" err
        Right resp -> ok "execStart" resp

killContainer :: MonadIO m => PodmanClient -> ContainerName -> m ()
killContainer client name = do
  trace "containerKill" name
  res <- containerKill client name Nothing
  case res of
    Just err -> abort "containerKill" err
    Nothing -> ok "container killed" ()
  wait <- containerWait client name Exited
  case wait of
    Left err -> abort "containerWait" err
    Right x -> ok "container has exited" x
  trace "containerDelete" name
  res' <- containerDelete client name Nothing Nothing
  case res' of
    Just err -> abort "containerDelete" err
    Nothing -> ok "container deleted" ()

version :: MonadIO m => PodmanClient -> m ()
version client = do
  trace "getVersion" ()
  res <- getVersion client
  case res of
    Left err -> abort "getVersion" err
    Right x -> ok "version" x

-- | Create a tarball
tar :: [(FilePath, ByteString)] -> Either String [Tar.Entry]
tar = mapM (\(path, content) -> Tar.fileEntry <$> Tar.toTarPath False path <*> pure content)

-- | Copy and retrieve files
filesCopy :: MonadIO m => PodmanClient -> ContainerName -> m ()
filesCopy client name = do
  let tarball = case tar [("test.dat", "test-data")] of
        Right x -> x
        Left e -> error "Tarball creation failed" e
  trace "containerSendFiles" tarball
  sendRes <- containerSendFiles client name tarball "/tmp/test-send" Nothing
  case sendRes of
    Just err -> abort "containerSendFiles" err
    Nothing -> ok "file sent" ()
  trace "containerGetFiles" ()
  getRes <- containerGetFiles client name "/tmp/test-send"
  case getRes of
    Left err -> error "containerGetFiles" err
    Right x -> ok "file received" x

-- | A demo program that tries to call every api
demo :: MonadIO m =>
    Interactive -> ImageName -> ContainerName -> PodmanClient -> m ()
demo isInteractive imageName containerName client = do
  version client
  ensureImage client imageName
  listImages client
  showImage client imageName
  ensureContainer client imageName containerName
  when (isInteractive == Interactive) $
    interactiveAttach client containerName
  execContainer client containerName ["cat", "/etc/os-release"]
  filesCopy client containerName
  tailContainer client containerName False
  killContainer client containerName

-- | Demonstrate an interactive session
interactiveAttach :: MonadIO m => PodmanClient -> ContainerName -> m ()
interactiveAttach client name = do
  trace "containerAttach" name
  liftIO $ do
    let attQ = defaultAttachQuery
                  { _attachQuerystderr = Just True,
                    _attachQuerystdout = Just True,
                    _attachQuerystdin = Just True
                  }
    res <- containerAttach client name attQ go
    trace "attach finished" res
  where

    go :: ContainerConnection -> IO ()
    go conn = do
      ok "attached, press \"exit\" to quit" ()
      a1 <- async $ writer conn
      a2 <- async $ reader conn
      (_, res) <- waitAnyCancel [a1, a2]
      pure res

    writer :: ContainerConnection -> IO ()
    writer conn = do
      putStr "> "
      hFlush stdout
      x <- T.encodeUtf8 . T.pack . flip mappend "\n" <$> getLine
      if x == "exit\n"
        then pure ()
        else do
          print ("Sending" <> show x)
          containerSend conn x
          writer conn

    reader :: ContainerConnection -> IO ()
    reader conn = do
      dat <- containerRecv conn
      print dat
      when (dat /= EOF) (reader conn)

-- | Tail a container logs
tailContainer :: MonadIO m => PodmanClient -> ContainerName -> Bool -> m ()
tailContainer client name follow = do
  trace "containerLogs" name
  res <- containerLogs client name LogBoth (defaultLogsQuery {_logsQueryfollow = Just follow}) print
  case res of
    Just err -> error "containerLogs" err
    Nothing -> ok "logs printed" ()

-- | Ensure a container exists and attach to it
shell :: MonadIO m => PodmanClient -> m ()
shell client = do
  exist <- containerExists client demoName
  unless (fromRight False exist) createContainer
  ensureStarted
  interactiveAttach client demoName
  where
    createContainer = do
      res <- containerCreate client spec
      case res of
        Left err -> error (show err)
        Right x -> liftIO $ print $ "Created: " <> show x
    ensureStarted = do
      start <- containerStart client demoName Nothing
      case start of
        Nothing -> liftIO $ putStrLn "Started!"
        Just x ->
          if _errorresponse x == 304
            then liftIO $ putStrLn "Already started"
            else error (show x)
    spec =
      (mkSpecGenerator "registry.access.redhat.com/ubi8/ubi")
        { _specGeneratorname = Just demoName',
          _specGeneratorstdin = Just True
        }
    demoName' = "demo-haskell"
    demoName = ContainerName demoName'



-- | CLI entrypoint
main :: IO ()
main = do
  (isInteractive, args) <- do args <- getArgs
                              if "--interactive" `elem` args
                                then return (Interactive, L.delete "--interactive" args)
                                else return (NonInteractive, args)

  case map T.pack args of
    [url, image, container] -> withClient url $
                                let img = ImageName image
                                    ctr = ContainerName container
                                in  demo isInteractive img ctr
    [url, "shell"] -> withClient url shell
    [url, container] -> withClient url $ \client ->
                          tailContainer client (ContainerName container) True
    _ -> putStrLn $ unlines [
            "usage: podman-demo [--interactive] URL IMAGE_NAME CONTAINER_NAME"
          , ""
          , "URL specifies some HTTP endpoint to connect to. e.g. 'http://localhost:3000/',"
          , "or 'http+unix://tmp/podman.sock'."
          , ""
          , "IMAGE_NAME is an image which will be pulled (if necessary) and used to"
          , "create a demo container. e.g. 'alpine:3.12.0'."
          , ""
          , "(In fact Alpine is probably a good choice of container, because it's"
          , "small, and has all the files this demo program expects to see.)"
          , ""
          , "CONTAINER_NAME is a name for a container which will be used for the demo"
          , "(and created, if necessary). e.g. 'foo', 'my-container'."
          , ""
          , "If you specify --interactive, the demo will include attaching"
          , "to the launched container."
          , ""
          , "other usages: see the source code."
          , "'podman-demo URL shell' and 'podman-demo CONTAINER_NAME' are also allowable"
          , "invocations."
          ]

-- Some helper functions
trace' :: (Show a, MonadIO m) => Text -> Text -> a -> m ()
trace' prefix msg arg = liftIO $ putStrLn $ T.unpack prefix <> " " <> T.unpack msg <> " " <> show arg

trace :: (Show a, MonadIO m) => Text -> a -> m ()
trace = trace' "[API]"

ok :: (Show a, MonadIO m) => Text -> a -> m ()
ok = trace' " [OK]"

abort :: Text -> Error -> a
abort msg error' = error (T.unpack msg <> " " <> show error')
