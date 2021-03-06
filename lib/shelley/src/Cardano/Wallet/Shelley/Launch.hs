{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- NOTE Temporary until we can fully enable the cluster
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch cardano-node for /testing/.

module Cardano.Wallet.Shelley.Launch
    ( -- * Integration Launcher
      withCluster
    , withBFTNode
    , withStakePool
    , NodeParams (..)
    , singleNodeParams

    -- * Utils
    , NetworkConfiguration (..)
    , nodeSocketOption
    , networkConfigurationOption
    , parseGenesisData
    , withTempDir
    , withSystemTempDir

    -- * Logging
    , ClusterLog (..)
    ) where

import Prelude

import Cardano.Api.Shelley.Genesis
    ( ShelleyGenesis (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.CLI
    ( optionT )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn (..)
    , NodePort (..)
    , withCardanoNode
    )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), NetworkParameters (..), PoolId (..), ProtocolMagic (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, fromGenesisData, testnetVersionData )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, link, race )
import Control.Concurrent.Chan
    ( newChan, readChan, writeChan )
import Control.Concurrent.MVar
    ( MVar, newMVar, putMVar, takeMVar )
import Control.Exception
    ( SomeException, finally, handle, throwIO )
import Control.Monad
    ( forM, forM_, replicateM, replicateM_, unless, void, (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( FromJSON (..), eitherDecode, toJSON, (.:), (.=) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isLeft, isRight )
import Data.Functor
    ( ($>), (<&>) )
import Data.List
    ( isInfixOf, nub, permutations, sort )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import GHC.TypeLits
    ( KnownNat, Nat, SomeNat (..), someNatVal )
import Options.Applicative
    ( Parser, help, long, metavar, (<|>) )
import Ouroboros.Consensus.Shelley.Node
    ( sgNetworkMagic )
import Ouroboros.Consensus.Shelley.Protocol
    ( TPraosStandardCrypto )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import System.Directory
    ( copyFile, createDirectory )
import System.Environment
    ( lookupEnv, setEnv )
import System.Exit
    ( ExitCode (..) )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory, withTempDirectory )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process
    ( readProcess, readProcessWithExitCode )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.Coin as SL

data NetworkConfiguration where
    TestnetConfig
        :: FilePath
        -> NetworkConfiguration

    StagingConfig
        :: FilePath
        -> NetworkConfiguration

-- | Hand-written as there's no Show instance for 'NodeVersionData'
instance Show NetworkConfiguration where
    show = \case
        TestnetConfig genesisFile ->
            "TestnetConfig " <> show genesisFile
        StagingConfig genesisFile ->
            "StagingConfig " <> show genesisFile

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = optionT $ mempty
    <> long "node-socket"
    <> metavar "FILE"
    <> help "Path to the node's domain socket."

-- | --testnet=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption =
    (TestnetConfig <$> customNetworkOption "testnet")
    <|>
    (StagingConfig <$> customNetworkOption "staging")
  where
    customNetworkOption
        :: String
        -> Parser FilePath
    customNetworkOption network = optionT $ mempty
        <> long network
        <> metavar "FILE"
        <> help "Path to the genesis .json file."

someCustomDiscriminant
    :: (forall (pm :: Nat). KnownNat pm => Proxy pm -> SomeNetworkDiscriminant)
    -> ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeVersionData)
someCustomDiscriminant mkSomeNetwork pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( mkSomeNetwork proxy
            , testnetVersionData pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."

parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeVersionData, Block)
parseGenesisData = \case
    TestnetConfig genesisFile -> do
        (genesis :: ShelleyGenesis TPraosStandardCrypto)
            <- ExceptT $ eitherDecode <$> BL.readFile genesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Testnet pm)

        let nm = sgNetworkMagic genesis
        let pm = ProtocolMagic $ fromIntegral nm
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, block0) = fromGenesisData genesis (Map.toList $ sgInitialFunds genesis)
        pure
            ( discriminant
            , np
            , vData
            , block0
            )

    StagingConfig genesisFile -> do
        (genesis :: ShelleyGenesis TPraosStandardCrypto)
            <- ExceptT $ eitherDecode <$> BL.readFile genesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Staging pm)

        let nm = sgNetworkMagic genesis
        let pm = ProtocolMagic $ fromIntegral nm
        let (discriminant, vData) = someCustomDiscriminant mkSomeNetwork pm
        let (np, block0) = fromGenesisData genesis (Map.toList $ sgInitialFunds genesis)
        pure
            ( discriminant
            , np
            , vData
            , block0
            )

-- NOTE
-- Fixture wallets we use in integration tests comes from "initialFunds"
-- referenced in the genesis file. As of today, initial funds are declared as a
-- key-value map in JSON, and parsing libraries does not enforce any particular
-- ordering on the keys when parsing the map. Because this wallet uses sequential
-- derivation, it relies on addresses being discovered in a certain order.
newtype PreserveInitialFundsOrdering =
    PreserveInitialFundsOrdering
        ( ShelleyGenesis TPraosStandardCrypto
        , [(SL.Addr TPraosStandardCrypto, SL.Coin)]
        )
    deriving (Show)

instance FromJSON PreserveInitialFundsOrdering where
    parseJSON source = do
        json <- parseJSON source
        base <- clearField "initialFunds" json >>= parseJSON
        initialFunds <- flip (Aeson.withObject "ShelleyGenesis") source $ \obj ->
            obj .: "initialFunds"
        pure $ PreserveInitialFundsOrdering
            ( base
            , mconcat (Map.toList <$> initialFunds)
            )
      where
        clearField field = withObject
            (pure . HM.update (const (Just $ Aeson.Object mempty)) field)

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO String
cli tr args = do
    traceWith tr $ MsgCLI args
    readProcess "cardano-cli" args stdin
  where
    stdin = ""

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@ and that the env var
-- @CARDANO_NODE_SOCKET_PATH@ has already been set.
cliRetry
    :: Tracer IO ClusterLog
    -> String -- ^ message to print before running command
    -> [String] -- ^ arguments to @cardano-cli@
    -> IO String
cliRetry tr msg args = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO $ ProcessHasExited
            (unwords (prog:args) <> " failed: " <> err) st
  where
    prog = "cardano-cli"
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcessWithExitCode "cardano-cli" args mempty
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
withCluster
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> Severity
    -- ^ Minimum logging severity for @cardano-node@
    -> Int
    -- ^ How many pools should the cluster spawn.
    -> FilePath
    -- ^ Parent state directory for cluster
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Action to run with the cluster up
    -> IO a
withCluster tr severity n dir action = bracketTracer' tr "withCluster" $ do
    systemStart <- addUTCTime 1 <$> getCurrentTime
    (port0:ports) <- randomUnusedTCPPorts (n + 2)
    let bftCfg = NodeParams severity systemStart (head $ rotate ports)
    withBFTNode tr dir bftCfg $ \bftSocket block0 params -> do
        waitForSocket tr bftSocket
        waitGroup <- newChan
        doneGroup <- newChan
        let waitAll   = do
                traceWith tr $ MsgDebug "waiting for stake pools to register"
                replicateM  n (readChan waitGroup)
        let cancelAll = do
                traceWith tr $ MsgDebug "stopping all stake pools"
                replicateM_ n (writeChan doneGroup ())

        let onException :: SomeException -> IO ()
            onException e = do
                traceWith tr $ MsgDebug $ "exception while starting pool: " <>
                    T.pack (show e)
                writeChan waitGroup (Left e)

        let pledgeOf 0 = 2*oneMillionAda
            pledgeOf _ = oneMillionAda
        forM_ (zip [0..] $ tail $ rotate ports) $ \(idx, (port, peers)) -> do
            link =<< async (handle onException $ do
                let spCfg = NodeParams severity systemStart (port, peers)
                withStakePool tr dir idx spCfg (pledgeOf idx) $ do
                    writeChan waitGroup $ Right port
                    readChan doneGroup)

        traceWith tr MsgCartouche
        group <- waitAll
        if length (filter isRight group) /= n then do
            cancelAll
            throwIO $ ProcessHasExited
                ("cluster didn't start correctly: " <> show (filter isLeft group))
                (ExitFailure 1)
        else do
            let cfg = NodeParams severity systemStart (port0, ports)
            withPassiveNode tr dir cfg $ \socket -> do
                action socket block0 params `finally` cancelAll
  where
    -- | Get permutations of the size (n-1) for a list of n elements, alongside with
    -- the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap (\(x:xs) -> (x, sort xs)) . permutations

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams = NodeParams
    { minSeverity :: Severity -- ^ Minimum logging severity
    , systemStart :: UTCTime -- ^ Genesis block start time
    , nodePeers :: (Int, [Int]) -- ^ A list of ports used by peers and this node
    } deriving (Show)

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr baseDir (NodeParams severity systemStart (port, peers)) action =
    bracketTracer' tr "withBFTNode" $ do
        createDirectory dir

        [vrfPrv, kesPrv, opCert] <- forM
            ["bft-leader.vrf.skey", "bft-leader.kes.skey", "bft-leader.opcert"]
            (\f -> copyFile (source </> f) (dir </> f) $> (dir </> f))

        (config, block0, networkParams, versionData)
            <- genConfig dir severity systemStart
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Nothing
                , nodeSignKeyFile = Nothing
                , nodeOpCertFile = Just opCert
                , nodeKesKeyFile = Just kesPrv
                , nodeVrfKeyFile = Just vrfPrv
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg $ \(CardanoNodeConn socket) -> do
            setEnv "CARDANO_NODE_SOCKET_PATH" socket
            (rawTx, faucetPrv) <- prepareKeyRegistration tr dir
            tx <- signTx tr dir rawTx [faucetPrv]
            submitTx tr "pre-registered stake key" tx
            action socket block0 (networkParams, versionData)
  where
    source :: FilePath
    source = $(getTestData) </> "cardano-node-shelley"

    name = "bft"
    dir = baseDir </> name

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
withPassiveNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (FilePath -> IO a)
    -- ^ Callback function with socket path
    -> IO a
withPassiveNode tr baseDir (NodeParams severity systemStart (port, peers)) act =
    bracketTracer' tr "withPassiveNode" $ do
        createDirectory dir

        (config, _, _, _) <- genConfig dir severity systemStart
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Nothing
                , nodeSignKeyFile = Nothing
                , nodeOpCertFile = Nothing
                , nodeKesKeyFile = Nothing
                , nodeVrfKeyFile = Nothing
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg $ \(CardanoNodeConn socket) ->
            act socket
  where
    name = "node"
    dir = baseDir </> name

singleNodeParams :: Severity -> IO NodeParams
singleNodeParams severity = do
    systemStart <- getCurrentTime
    pure $ NodeParams severity systemStart (0, [])

-- | Populates the configuration directory of a stake pool @cardano-node@.
--
-- Returns a tuple with:
--  * A config for launching the stake pool node.
--  * The public operator certificate - used for verifying registration.
--  * A transaction which should be submitted to register the pool.
setupStakePoolData
    :: Tracer IO ClusterLog
    -- ^ Logging object.
    -> FilePath
    -- ^ Output directory.
    -> String
    -- ^ Short name of the stake pool.
    -> NodeParams
    -- ^ Parameters used for generating config files.
    -> String
    -- ^ Base URL of metadata server.
    -> Integer
    -- ^ Pledge (and stake) amount
    -> IO (CardanoNodeConfig, FilePath, FilePath)
setupStakePoolData tr dir name params url pledgeAmt = do
    let NodeParams severity systemStart (port, peers) = params

    (opPrv, opPub, opCount, metadata) <- genOperatorKeyPair tr dir
    (vrfPrv, vrfPub) <- genVrfKeyPair tr dir
    (kesPrv, kesPub) <- genKesKeyPair tr dir
    (stakePrv, stakePub) <- genStakeAddrKeyPair tr dir

    stakeCert <- issueStakeCert tr dir stakePub
    poolCert <- issuePoolCert tr dir opPub vrfPub stakePub url metadata pledgeAmt
    dlgCert <- issueDlgCert tr dir stakePub opPub
    opCert <- issueOpCert tr dir kesPub opPrv opCount

    (config, _, _, _) <- genConfig dir severity systemStart
    topology <- genTopology dir peers

    -- In order to get a working stake pool we need to.
    --
    -- 1. Register a stake key for our pool.
    -- 2. Register the stake pool
    -- 3. Delegate funds to our pool's key.
    --
    -- We cheat a bit here by delegating to our stake address right away
    -- in the transaction used to registered the stake key and the pool
    -- itself.  Thus, in a single transaction, we end up with a
    -- registered pool with some stake!
    (rawTx, faucetPrv) <-
        preparePoolRegistration tr dir stakePub [stakeCert, poolCert, dlgCert] pledgeAmt
    tx <- signTx tr dir rawTx [faucetPrv, stakePrv, opPrv]

    let cfg = CardanoNodeConfig
            { nodeDir = dir
            , nodeConfigFile = config
            , nodeTopologyFile = topology
            , nodeDatabaseDir = "db"
            , nodeDlgCertFile = Nothing
            , nodeSignKeyFile = Nothing
            , nodeOpCertFile = Just opCert
            , nodeKesKeyFile = Just kesPrv
            , nodeVrfKeyFile = Just vrfPrv
            , nodePort = Just (NodePort port)
            , nodeLoggingHostname = Just name
            }

    pure (cfg, opPub, tx)

-- | Start a "stake pool node". The pool will register itself.
withStakePool
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node and stake pool data will be created in a
    -- subdirectory of this.
    -> Int
    -- ^ Stake pool index in the cluster
    -> NodeParams
    -- ^ Configuration for the underlying node
    -> Integer
    -- ^ Pledge amount / initial stake
    -> IO a
    -- ^ Action to run with the stake pool running
    -> IO a
withStakePool tr baseDir idx params pledgeAmt action =
    bracketTracer' tr "withStakePool" $ do
        createDirectory dir
        withStaticServer dir $ \url -> do
            traceWith tr $ MsgStartedStaticServer dir url
            (cfg, opPub, tx) <- setupStakePoolData tr dir name params url pledgeAmt
            withCardanoNodeProcess tr name cfg $ \_ -> do
                submitTx tr name tx
                timeout 120 ("pool registration", waitUntilRegistered tr name opPub)
                action
  where
    dir = baseDir </> name
    name = "pool-" ++ show idx

withCardanoNodeProcess
    :: Tracer IO ClusterLog
    -> String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> IO a
withCardanoNodeProcess tr name cfg = withCardanoNode tr' cfg >=> throwErrs
  where
    tr' = contramap (MsgLauncher name) tr
    throwErrs = either throwIO pure

genConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> Severity
    -- ^ Minimum severity level for logging
    -> UTCTime
    -- ^ Genesis block start time
    -> IO (FilePath, Block, NetworkParameters, NodeVersionData)
genConfig dir severity systemStart = do
    -- we need to specify genesis file location every run in tmp
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withObject (pure . addGenesisFilePath (T.pack nodeGenesisFile))
        >>= withObject (addMinSeverityStdout severity)
        >>= withObject (pure . addMinSeverity Debug)
        >>= Yaml.encodeFile (dir </> "node.config")

    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
        >>= withObject (pure . updateSystemStart systemStart)
        >>= withObject transformInitialFunds
        >>= Aeson.encodeFile nodeGenesisFile

    PreserveInitialFundsOrdering (genesis, initialFunds) <-
        Yaml.decodeFileThrow (source </> "genesis.yaml")
        >>= withObject (pure . updateSystemStart systemStart)
        >>= either fail pure . Aeson.parseEither parseJSON

    let nm = sgNetworkMagic genesis
    let (networkParameters, block0) = fromGenesisData genesis initialFunds
    let versionData =
            ( NodeToClientVersionData $ NetworkMagic nm
            , nodeToClientCodecCBORTerm
            )

    pure
        ( dir </> "node.config"
        , block0
        , networkParameters
        , versionData
        )
  where
    source :: FilePath
    source = $(getTestData) </> "cardano-node-shelley"

    nodeGenesisFile :: FilePath
    nodeGenesisFile = dir </> "genesis.json"

-- | Generate a topology file from a list of peers.
genTopology :: FilePath -> [Int] -> IO FilePath
genTopology dir peers = do
    let file = dir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object [ "Producers" .= map encodePeer peers ]
    pure file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port = Aeson.object
        [ "addr"    .= ("127.0.0.1" :: String)
        , "port"    .= port
        , "valency" .= (1 :: Int)
        ]

-- | Create a key pair for a node operator's offline key and a new certificate
-- issue counter
genOperatorKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath, FilePath, Aeson.Value)
genOperatorKeyPair tr dir = do
    traceWith tr $ MsgGenOperatorKeyPair dir
    (_poolId, pub, prv, count, metadata) <- takeMVar operators >>= \case
        [] -> fail "genOperatorKeyPair: Awe crap! No more operators available!"
        (op:q) -> putMVar operators q $> op

    let opPub = dir </> "op.pub"
    let opPrv = dir </> "op.prv"
    let opCount = dir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure (opPrv, opPub, opCount, metadata)

-- | Create a key pair for a node KES operational key
genKesKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genKesKeyPair tr dir = do
    let kesPub = dir </> "kes.pub"
    let kesPrv = dir </> "kes.prv"
    void $ cli tr
        [ "shelley", "node", "key-gen-KES"
        , "--verification-key-file", kesPub
        , "--signing-key-file", kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genVrfKeyPair tr dir = do
    let vrfPub = dir </> "vrf.pub"
    let vrfPrv = dir </> "vrf.prv"
    void $ cli tr
        [ "shelley", "node", "key-gen-VRF"
        , "--verification-key-file", vrfPub
        , "--signing-key-file", vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genStakeAddrKeyPair tr dir = do
    let stakePub = dir </> "stake.pub"
    let stakePrv = dir </> "stake.prv"
    void $ cli tr
        [ "shelley", "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]
    pure (stakePrv, stakePub)

-- | Issue a node operational certificate
issueOpCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert tr dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    void $ cli tr
        [ "shelley", "node", "issue-op-cert"
        , "--kes-verification-key-file", kesPub
        , "--cold-signing-key-file", opPrv
        , "--operational-certificate-issue-counter-file", opCount
        , "--kes-period", "0"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate
issueStakeCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> IO FilePath
issueStakeCert tr dir stakePub = do
    let file = dir </> "stake.cert"
    void $ cli tr
        [ "shelley", "stake-address", "registration-certificate"
        , "--staking-verification-key-file", stakePub
        , "--out-file", file
        ]
    pure file

-- | Create a stake pool registration certificate
issuePoolCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> String
    -> Aeson.Value
    -> Integer
    -> IO FilePath
issuePoolCert tr dir opPub vrfPub stakePub baseURL metadata pledgeAmt = do
    let file  = dir </> "pool.cert"
    let bytes = Aeson.encode metadata
    BL8.writeFile (dir </> "metadata.json") bytes
    void $ cli tr
        [ "shelley", "stake-pool", "registration-certificate"
        , "--cold-verification-key-file", opPub
        , "--vrf-verification-key-file", vrfPub
        , "--pool-pledge", show pledgeAmt
        , "--pool-cost", "0"
        , "--pool-margin", "0.1"
        , "--pool-reward-account-verification-key-file", stakePub
        , "--pool-owner-stake-verification-key-file", stakePub
        , "--metadata-url", baseURL </> "metadata.json"
        , "--metadata-hash", blake2b256S (BL.toStrict bytes)
        , "--mainnet"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address delegation certificate.
issueDlgCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> IO FilePath
issueDlgCert tr dir stakePub opPub = do
    let file = dir </> "dlg.cert"
    void $ cli tr
        [ "shelley", "stake-address", "delegation-certificate"
        , "--staking-verification-key-file", stakePub
        , "--stake-pool-verification-key-file", opPub
        , "--out-file", file
        ]
    pure file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
preparePoolRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> [FilePath]
    -> Integer
    -> IO (FilePath, FilePath)
preparePoolRegistration tr dir stakePub certs pledgeAmt = do
    let file = dir </> "tx.raw"
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    void $ cli tr
        [ "shelley", "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    addr <- cli tr
        [ "shelley", "address", "build"
        , "--payment-verification-key-file", sinkPub
        , "--stake-verification-key-file", stakePub
        , "--mainnet"
        ]

    (faucetInput, faucetPrv) <- takeFaucet dir
    void $ cli tr $
        [ "shelley", "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", init addr <> "+" <> show pledgeAmt
        , "--ttl", "100"
        , "--fee", show (faucetAmt - pledgeAmt - depositAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> IO (FilePath, FilePath)
prepareKeyRegistration tr dir = do
    let file = dir </> "tx.raw"

    let stakePub = dir </> "pre-registered-stake.pub"
    Aeson.encodeFile stakePub preRegisteredStakeKey

    (faucetInput, faucetPrv) <- takeFaucet dir

    cert <- issueStakeCert tr dir stakePub

    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    void $ cli tr
        [ "shelley", "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    addr <- cli tr
        [ "shelley", "address", "build"
        , "--payment-verification-key-file", sinkPub
        , "--mainnet"
        ]

    void $ cli tr
        [ "shelley", "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", init addr <> "+" <> "1"
        , "--ttl", "100"
        , "--fee", show (faucetAmt - depositAmt - 1)
        , "--certificate-file", cert
        , "--out-file", file
        ]
    pure (file, faucetPrv)

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> [FilePath]
    -> IO FilePath
signTx tr dir rawTx keys = do
    let file = dir </> "tx.signed"
    void $ cli tr $
        [ "shelley", "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ] ++ mconcat ((\key -> ["--signing-key-file", key]) <$> keys)
    pure file

-- | Submit a transaction through a running node.
submitTx :: Tracer IO ClusterLog -> String -> FilePath -> IO ()
submitTx tr name signedTx = do
    void $ cliRetry tr ("Submitting transaction for " ++ name)
        [ "shelley", "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet"
        ]

-- | Wait for a command which depends on connecting to the given socket path to
-- succeed.
--
-- It retries every second, for up to 30 seconds. An exception is thrown if
-- it has waited for too long.
--
-- As a side effect, after this subroutine finishes, the environment variable
-- @CARDANO_NODE_SOCKET_PATH@ is set.
waitForSocket :: Tracer IO ClusterLog -> FilePath -> IO ()
waitForSocket tr socketPath = do
    setEnv "CARDANO_NODE_SOCKET_PATH" socketPath
    let msg = "Checking for usable socket file " <> socketPath
    -- TODO: check whether querying the tip works just as well.
    void $ cliRetry tr msg
        ["shelley", "query", "stake-distribution", "--mainnet"]
    traceWith tr $ MsgSocketIsReady socketPath

-- | Wait until a stake pool shows as registered on-chain.
waitUntilRegistered :: Tracer IO ClusterLog -> String -> FilePath -> IO ()
waitUntilRegistered tr name opPub = do
    poolId <- init <$> cli tr
        [ "shelley", "stake-pool", "id"
        , "--verification-key-file", opPub
        ]
    (exitCode, distribution, err) <- readProcessWithExitCode "cardano-cli"
        [ "shelley", "query", "stake-distribution"
        , "--mainnet"
        ] mempty
    traceWith tr $ MsgStakeDistribution name exitCode distribution err
    unless (poolId `isInfixOf` distribution) $ do
        threadDelay 5000000
        waitUntilRegistered tr name opPub

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: FilePath -> IO (String, FilePath)
takeFaucet dir = takeMVar faucets >>= \case
    []    -> fail "takeFaucet: Awe crap! No more faucet available!"
    ((input,prv):q) -> do
        putMVar faucets q
        let file = dir </> "faucet.prv"
        Aeson.encodeFile file prv
        pure (input, file)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucets :: MVar [(String, Aeson.Value)]
faucets = unsafePerformIO $ newMVar
    [ ( "cea1b041dd5465be636b5b88805571f83537bd503bc4db447f088d942673736c#0"
      , Aeson.object
          [ "type" .= Aeson.String "Genesis UTxO signing key"
          , "description" .= Aeson.String "Genesis initial UTxO key"
          , "cborHex" .= Aeson.String
              "5820db101b5f4cc53ca1d61f7505b23c05b1b58de0b9f509c4dfede4348549dbaa9d"
          ]
      )
    , ( "fa271c369d4d9a6b78e18f9d554730ef9978847ecb187c064cb9c8d56c2092cd#0"
      , Aeson.object
          [ "type" .= Aeson.String "Genesis UTxO signing key"
          , "description" .= Aeson.String "Genesis initial UTxO key"
          , "cborHex" .= Aeson.String
              "582061e08f3e8ac1afbf0434fca2bb4aa6484270d8dd3e251c049006aab368a74a7e"
          ]
      )
    , ( "672d7558074f02c662b11a4ff761ec3a24c94a18b319033af5f9f22a03b8891b#0"
      , Aeson.object
          [ "type" .= Aeson.String "Genesis UTxO signing key"
          , "description" .= Aeson.String "Genesis initial UTxO key"
          , "cborHex" .= Aeson.String
              "58204054ff827451cad61241450a09ea80c9d0658398f588ff976393ae8eacb859fe"
          ]
      )
    , ( "ca97dc6662a21f1b7ea0790c380d13dad84386cbb7f731c7ba3982a8d105267b#0"
      , Aeson.object
          [ "type" .= Aeson.String "Genesis UTxO signing key"
          , "description" .= Aeson.String "Genesis initial UTxO key"
          , "cborHex" .= Aeson.String
              "58204a7a8e7a1ba0d33c407dc3ceda225c605287cfb0e3b51d9eba3822abd6aa75ca"
          ]
      )
    , ( "cfc08d97636877d94cd19a246e72d191bc3905712bbab8cdbb1aa240fc09be3c#0"
      , Aeson.object
          [ "type" .= Aeson.String "Genesis UTxO signing key"
          , "description" .= Aeson.String "Genesis initial UTxO key"
          , "cborHex" .= Aeson.String
              "5820e96f612fbff3df3d8eef4ea3a07e3dc98769020545ced0167998a85a4cc50aa7"
          ]
      )
    ]
{-# NOINLINE faucets #-}

operators :: MVar [(PoolId, Aeson.Value, Aeson.Value, Aeson.Value, Aeson.Value)]
operators = unsafePerformIO $ newMVar
    [ ( PoolId $ unsafeFromHex
          "c7258ccc42a43b653aaf2f80dde3120df124ebc3a79353eed782267f78d04739"
      , Aeson.object
          [ "type" .= Aeson.String "Node operator verification key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operator signing key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820d8f81c455ef786f47ad9f573e49dc417e0125dfa8db986d6c0ddc03be8634dc6"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operational certificate issue counter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool A"
          , "ticker" .= Aeson.String "GPA"
          , "description" .= Aeson.Null
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    , ( PoolId $ unsafeFromHex
          "775af3b22eff9ff53a0bdd3ac6f8e1c5013ab68445768c476ccfc1e1c6b629b4"
      , Aeson.object
          [ "type" .= Aeson.String "Node operator verification key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operator signing key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820fab9d94c52b3e222ed494f84020a29ef8405228d509a924106d05ed01c923547"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operational certificate issue counter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool B"
          , "ticker" .= Aeson.String "GPB"
          , "description" .= Aeson.Null
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    , ( PoolId $ unsafeFromHex
          "5a7b67c7dcfa8c4c25796bea05bcdfca01590c8c7612cc537c97012bed0dec35"
      , Aeson.object
          [ "type" .= Aeson.String "Node operator verification key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operator signing key"
          , "description" .= Aeson.String "Stake pool operator key"
          , "cborHex" .= Aeson.String
              "5820047572e48be93834d6d7ddb01bb1ad889b4de5a7a1a78112f1edd46284250869"
          ]
      , Aeson.object
          [ "type" .= Aeson.String "Node operational certificate issue counter"
          , "description" .= Aeson.String "Next certificate issue number: 0"
          , "cborHex" .= Aeson.String
              "82005820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
          ]
      , Aeson.object
          [ "name" .= Aeson.String "Genesis Pool C"
          , "ticker" .= Aeson.String "GPC"
          , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
          , "homepage" .= Aeson.String "https://iohk.io"
          ]
      )
    ]
{-# NOINLINE operators #-}

-- | A public stake key associated with a mnemonic that we pre-registered for
-- STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKey
    :: Aeson.Value
preRegisteredStakeKey = Aeson.object
    [ "type" .= Aeson.String "StakingVerificationKeyShelley"
    , "description" .= Aeson.String "Free form text"
    , "cborHex" .= Aeson.String
        "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
    ]

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 100000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 10 * oneMillionAda

-- | Just one million Ada, in Lovelace.
oneMillionAda :: Integer
oneMillionAda = 1_000_000_000_000

-- | Add a "systemStart" field in a given object with the current POSIX time as a
-- value.
updateSystemStart
    :: UTCTime
    -> Aeson.Object
    -> Aeson.Object
updateSystemStart systemStart =
    HM.insert "systemStart" (toJSON systemStart)

-- | Add a "GenesisFile" field in a given object with the current path of
-- genesis.json in tmp dir as value.
addGenesisFilePath
    :: Text
    -> Aeson.Object
    -> Aeson.Object
addGenesisFilePath path = HM.insert "GenesisFile" (toJSON path)

-- | Add a @setupScribes[1].scMinSev@ field in a given config object.
-- The full lens library would be quite helpful here.
addMinSeverityStdout
    :: Monad m
    => Severity
    -> Aeson.Object
    -> m Aeson.Object
addMinSeverityStdout severity ob = case HM.lookup "setupScribes" ob of
    Just (Aeson.Array scribes) -> do
        let scribes' = Aeson.Array $ fmap setMinSev scribes
        pure $ HM.insert "setupScribes" scribes' ob
    _ -> fail "setupScribes logging config is missing or the wrong type"
  where
    sev = toJSON $ show severity
    setMinSev (Aeson.Object scribe)
        | HM.lookup "scKind" scribe == Just (Aeson.String "StdoutSK")
            = Aeson.Object (HM.insert "scMinSev" sev scribe)
        | otherwise = Aeson.Object scribe
    setMinSev a = a

-- | Add a global "minSeverity" field in a given config object.
addMinSeverity
    :: Severity
    -> Aeson.Object
    -> Aeson.Object
addMinSeverity severity = HM.insert "minSeverity" (toJSON $ show severity)

-- | Transform initial funds back to a big object instead of a list of
-- singletons.
transformInitialFunds
    :: Aeson.Object
    -> IO Aeson.Object
transformInitialFunds = pure . HM.update toObject "initialFunds"
  where
    toObject = \case
        Aeson.Array xs ->
            pure $ Aeson.Object $ HM.fromList (singleton <$> V.toList xs)
        _ ->
            error "transformInitialFunds: expected initialFunds to be an array."
    singleton = \case
        Aeson.Object obj ->
            head $ HM.toList obj
        _ ->
            error "transformInitialFunds: expected initialFunds to be many singletons"

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ -> fail
        "withObject: was given an invalid JSON. Expected an Object but got \
        \something else."

-- | Little helper to run an action within a certain delay. Fails if the action
-- takes too long.
timeout :: Int -> (String, IO a) -> IO a
timeout t (title, action) = do
    race (threadDelay $ t * 1000000) action >>= \case
        Left _  -> fail ("Waited too long for: " <> title)
        Right a -> pure a

-- | A little disclaimer shown in the logs when setting up the cluster.
cartouche :: Text
cartouche = T.unlines
    [ ""
    , "################################################################################"
    , "#                                                                              #"
    , "#  ⚠                           DISCLAIMER                                   ⚠  #"
    , "#                                                                              #"
    , "#        Cluster is booting. Stake pools are being registered on chain.        #"
    , "#                                                                              #"
    , "#        This may take roughly 60s, after what pools will become active        #"
    , "#        and will start producing blocks. Please be patient...                 #"
    , "#                                                                              #"
    , "#  ⚠                           DISCLAIMER                                   ⚠  #"
    , "#                                                                              #"
    , "################################################################################"
    ]

-- | Hash a ByteString using blake2b_256 and encode it in base16
blake2b256S :: ByteString -> String
blake2b256S =
    T.unpack
    . T.decodeUtf8
    . convertToBase Base16
    . blake2b256

-- | Create a temporary directory and remove it after the given IO action has
-- finished -- unless the @NO_CLEANUP@ environment variable has been set.
withTempDir
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Parent directory
    -> String -- ^ Directory name template
    -> (FilePath -> IO a) -- ^ Callback that can use the directory
    -> IO a
withTempDir tr parent name action = isEnvSet "NO_CLEANUP" >>= \case
    True -> do
        dir <- createTempDirectory parent name
        res <- action dir
        traceWith tr $ MsgTempNoCleanup dir
        pure res
    False -> withTempDirectory parent name action
  where
    isEnvSet ev = lookupEnv ev <&> \case
        Nothing -> False
        Just "" -> False
        Just _ -> True

withSystemTempDir
    :: Tracer IO ClusterLog
    -> String   -- ^ Directory name template
    -> (FilePath -> IO a) -- ^ Callback that can use the directory
    -> IO a
withSystemTempDir tr name action = do
    parent <- getCanonicalTemporaryDirectory
    withTempDir tr parent name action

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgCartouche
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgTempNoCleanup FilePath
    | MsgBracket Text BracketLog
    | MsgCLIStatus String ExitCode String String
    | MsgCLIRetry String
    | MsgCLIRetryResult String Int String
    | MsgSocketIsReady FilePath
    | MsgStakeDistribution String ExitCode String String
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgCartouche -> cartouche
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for " <> T.pack fp
                <> " at " <> T.pack baseUrl
        MsgTempNoCleanup dir ->
            "NO_CLEANUP of temporary directory " <> T.pack dir
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> T.pack msg
            ExitFailure code -> "Failed " <> T.pack msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> T.pack msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> T.pack msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady socketPath ->
            T.pack socketPath <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for " <> T.pack name <>
                ":\n" <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli":args)
      where
        indent = T.unlines . map ("  " <>) . T.lines . T.pack

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgCartouche -> Warning
        MsgLauncher _ msg -> getSeverityAnnotation msg
        MsgStartedStaticServer _ _ -> Info
        MsgTempNoCleanup _ -> Notice
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _-> Debug
        MsgCLIStatus _ (ExitFailure _) _ _-> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Warning
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _-> Info
        MsgStakeDistribution _ (ExitFailure _) _ _-> Warning
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
