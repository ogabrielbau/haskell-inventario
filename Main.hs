import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Exception (catch, IOException)
import System.IO (hFlush, stdout)
import Data.List (isInfixOf, foldl')

type Inventario = Map.Map String Item
type ResultadoOperacao = (Inventario, LogEntry)

data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read)

data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)
