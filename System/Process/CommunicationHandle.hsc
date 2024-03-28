{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module System.Process.CommunicationHandle
  ( -- * 'CommunicationHandle': a 'Handle' that can be serialised,
    -- enabling inter-process communication.
    CommunicationHandle
      -- NB: opaque, as the representation depends on the operating system
  , useCommunicationHandle
  , getCommunicationHandleHandle
    -- * Creating 'CommunicationHandle's to communicate with
    -- a child process
  , createWeReadTheyWritePipe
  , createTheyReadWeWritePipe
  )
 where

import Control.Arrow ( first )
import Foreign.C
import GHC.IO.Handle (Handle())
#if defined(mingw32_HOST_OS)
import Foreign.Ptr
import GHC.IO (onException)
import GHC.Windows (HANDLE)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.Handle.FD (fdToHandle)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.IOMode
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle())
# if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (handleToHANDLE)
import GHC.Event.Windows (associateHandle')
# endif

#include <fcntl.h>     /* for _O_BINARY */

#else
import System.Posix ( Fd(..), fdToHandle, handleToFd, FdOption(..), setFdOption )
#endif

import System.Process.Internals
  (
#if defined(mingw32_HOST_OS)
  createPipeFd,
#endif
#if !defined(mingw32_HOST_OS) || defined(__IO_MANAGER_WINIO__)
  createPipe
#endif
  )

--------------------------------------------------------------------------------
-- Communication handles.

-- | A 'CommunicationHandle' is an operating-system specific representation
-- of a 'Handle' that can be communicated through a command-line interface.
--
-- In a typical use case, the parent process creates a pipe, using e.g.
-- 'createWeReadTheyWritePipe' or 'createTheyReadWeWritePipe'.
--
--  - One end of the pipe is a 'Handle', which can be read from/written to by
--    the parent process.
--  - The other end is a 'CommunicationHandle', which can be serialised (using
--    the 'Show' instance), and passed to a child process.
--  - The child process can deserialise the 'CommunicationHandle' (using
--    the 'Read' instance), and then use 'useCommunicationHandle'
--    in order to retrieve a 'Handle' which it can write to/read from.
--
-- @since 1.6.19.0
newtype CommunicationHandle =
  CommunicationHandle
#if defined(mingw32_HOST_OS)
    HANDLE
#else
    Fd
#endif
  deriving ( Eq, Ord )

-- @since 1.6.19.0
instance Show CommunicationHandle where
  showsPrec p (CommunicationHandle h) =
    showsPrec p
#if defined(mingw32_HOST_OS)
      $ ptrToWordPtr
#endif
      h

-- @since 1.6.19.0
instance Read CommunicationHandle where
  readsPrec p str =
    fmap
      ( first $ CommunicationHandle
#if defined(mingw32_HOST_OS)
              . wordPtrToPtr
#endif
      ) $
      readsPrec p str

-- | Turn the 'CommunicationHandle' into a 'Handle' that can be used
-- in the current process.
--
-- Use 'getCommunicationHandleHandle' if you want access to the 'Handle' (e.g.
-- to close it) without reading/writing to it in the current process.
--
-- @since 1.6.19.0
useCommunicationHandle :: CommunicationHandle -> IO Handle
useCommunicationHandle ch@(CommunicationHandle _h) = do
#if defined(mingw32_HOST_OS) && defined(__IO_MANAGER_WINIO__)
  -- register the handle we received with the I/O manager
  return () <!> associateHandle' _h
#endif
  getCommunicationHandleHandle ch

-- | Turn the 'CommunicationHandle' into a 'Handle' that cannot be read from or
-- written to in the current process.
--
-- Use this if you want to e.g. close the handle.
-- Use 'useCommunicationHandle' if you want to read from or write to the handle.
--
-- @since 1.6.19.0
getCommunicationHandleHandle :: CommunicationHandle -> IO Handle
getCommunicationHandleHandle (CommunicationHandle h) = getGhcHandle h

-- | Gets a GHC Handle File description from the given OS Handle or POSIX fd.

#if defined(mingw32_HOST_OS)
getGhcHandle :: HANDLE -> IO Handle
getGhcHandle = getGhcHandlePOSIX <!> getGhcHandleNative

getGhcHandlePOSIX :: HANDLE -> IO Handle
getGhcHandlePOSIX handle =
  _open_osfhandle handle (#const _O_BINARY) >>= fdToHandle

foreign import ccall "io.h _open_osfhandle"
  _open_osfhandle :: HANDLE -> CInt -> IO CInt

getGhcHandleNative :: HANDLE -> IO Handle
getGhcHandleNative hwnd =
  do mb_codec <- fmap Just getLocaleEncoding
     let iomode = ReadWriteMode
         native_handle = fromHANDLE hwnd :: Io NativeHandle
     hw_type <- IODevice.devType $ native_handle
     mkHandleFromHANDLE native_handle hw_type (show hwnd) iomode mb_codec
       `onException` IODevice.close native_handle
#else
getGhcHandle :: Fd -> IO Handle
getGhcHandle fd = fdToHandle fd
#endif

--------------------------------------------------------------------------------
-- Creating pipes.

-- | Create a pipe @(weRead,theyWrite)@ that the current process can read from,
-- and whose write end can be passed to a child process in order to receive data from it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.19.0
createWeReadTheyWritePipe :: IO (Handle, CommunicationHandle)
createWeReadTheyWritePipe = create_pipe id

-- | Create a pipe @(theyRead,weWrite)@ that the current process can write to,
-- and whose read end can be passed to a child process in order to send data to it.
--
-- See 'CommunicationHandle'.
--
-- @since 1.6.19.0
createTheyReadWeWritePipe :: IO (CommunicationHandle, Handle)
createTheyReadWeWritePipe = sw <$> create_pipe sw
  where
    sw (a,b) = (b,a)

-- | Internal helper function used to define 'createWeReadTheyWritePipe'
-- and 'createTheyReadWeWritePipe' while reducing code duplication.
create_pipe
  :: ( forall a. (a, a) -> (a, a) )
  -> IO (Handle, CommunicationHandle)
create_pipe oursTheirs = do

  -- On Windows:
  --  - with WinIO, use pipes.
  --  - without WinIO, use FDs.
  -- On POSIX: use pipes.

  res@(hUs, _chThem) <-
#if defined(mingw32_HOST_OS)
   usingFDs
#  if defined(__IO_MANAGER_WINIO__)
     <!> usingPipes
#  endif
#else
    usingPipes
#endif
  associateToCurrentProcess hUs
  return res
  where
#if defined(mingw32_HOST_OS)
    usingFDs :: IO (Handle, CommunicationHandle)
    usingFDs = do
      (fdRead, fdWrite) <- createPipeFd
      let (fdUs, fdThem) = oursTheirs (fdRead, fdWrite)
      chThem <-
        CommunicationHandle <$>
          _get_osfhandle fdThem
      hUs <- fdToHandle fdUs `onException` c__close fdUs
      return (hUs, chThem)
#endif
#if !defined(mingw32_HOST_OS) || defined(__IO_MANAGER_WINIO__)
    usingPipes :: IO (Handle, CommunicationHandle)
    usingPipes = do
      (hRead, hWrite) <- createPipe
      let (hUs, hThem) = oursTheirs (hRead, hWrite)
      chThem <-
        CommunicationHandle <$>
#  if defined(__IO_MANAGER_WINIO__)
          handleToHANDLE hThem
#  else
          handleToFd hThem
#  endif
      return (hUs, chThem)
#endif

-- | Associate the 'Handle' to the current process. This is an internal
-- operation that ensures the handle can be properly read from/written to,
-- within the current process.
associateToCurrentProcess :: Handle -> IO ()
associateToCurrentProcess _h = do
#if !defined(mingw32_HOST_OS)
  fd <- handleToFd _h
  -- Don't allow the child process to inherit a parent file descriptor
  -- (such inheritance happens by default on Unix).
  setFdOption fd CloseOnExec True
#elif defined (__IO_MANAGER_WINIO__)
  -- With WinIO, we need to associate any handles we are going to use in
  -- the current process before being able to use them.
  return () <!> ( associateHandle' =<< handleToHANDLE _h )
#else
  return ()
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "io.h _get_osfhandle"
  _get_osfhandle :: CInt -> IO HANDLE

foreign import ccall "io.h _close"
  c__close :: CInt -> IO CInt
#endif
