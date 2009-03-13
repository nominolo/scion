import Scion
import Scion.Types
import Scion.Types.Notes
import Scion.Server.Protocol
import Scion.Server.Commands
import qualified Data.MultiSet as MS
import System.Directory ( removeFile )
import GHC.Conc

main =
  runScion $ do
--     setGHCVerbosity 5
--     setVerbosity deafening
    io $ writeFile test_file "main = return ()"
    io $ threadDelay 1000000
    rslt <- loadComponent (File test_file)
    io $ print (MS.size (compilationNotes rslt))
    io $ writeFile test_file "main = return -4"
    io $ threadDelay 1000000
    (ok, rslt') <- backgroundTypecheckFile test_file
    io $ print (ok, MS.size (compilationNotes rslt'))
    io $ writeFile test_file "main = print () >> return ()"
    io $ threadDelay 1000000
    (ok', rslt'') <- backgroundTypecheckFile test_file
    io $ print (ok', MS.size (compilationNotes rslt''))
    io $ removeFile test_file

io = liftIO
test_file = "./test.hs"
