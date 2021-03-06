{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Pool.DB.Properties
    ( properties
    , withDB
    , newMemoryDBLayer
    ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( DBLog (..), SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..)
    , ErrPointAlreadyExists (..)
    , determinePoolLifeCycleStatus
    , readPoolLifeCycleStatus
    )
import Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..), genStakePoolMetadata )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo
    , PoolCertificate (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotId (..)
    , slotMinBound
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Exception
    ( evaluate )
import Control.Monad
    ( forM_, replicateM, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Function
    ( on, (&) )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( set, view )
import Data.List.Extra
    ( nubOrd )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Conc
    ( TVar, newTVarIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
    , anyException
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldThrow
    )
import Test.QuickCheck
    ( Positive (..)
    , Property
    , checkCoverage
    , classify
    , counterexample
    , cover
    , property
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO) -> SpecWith (DBLayer IO) -> Spec
withDB create = beforeAll create . beforeWith
    (\db@DBLayer{cleanDB, atomically}-> atomically $ cleanDB $> db)

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO (DBLayer IO)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer' :: IO (TVar [DBLog], (SqliteContext, DBLayer IO))
newMemoryDBLayer' = do
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer (traceInTVarIO logVar) Nothing

properties :: SpecWith (DBLayer IO)
properties = do
    describe "Stake Pool properties" $ do
        it "putPoolProduction . readPoolProduction yields expected results"
            (property . prop_putReadPoolProduction)
        it "putPoolProduction with already put slot yields error"
            (property . prop_putSlotTwicePoolProduction)
        it "Rollback of stake pool production"
            (property . prop_rollbackPools)
        it "readPoolProductionCursor should return the last applied blocks"
            (property . prop_readPoolProductionCursorTipIsLast)
        it "readPoolProduction for a given epoch should always give slots \
           \from given epoch"
            (property . prop_readPoolNoEpochLeaks)
        it "readPoolProduction should never give pools with no slots"
            (property . (prop_readPoolCond noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks noEmptyPools))
        it "readPoolProduction should give pools with descending slots"
            (property . (prop_readPoolCond descSlotsPerPool))
        it "readPoolProduction should give pools with descending slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks descSlotsPerPool))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks descSlotsPerPool))
        it "readStakeDistribution . putStakeDistribution == pure"
            (property . prop_putStakeReadStake)
        it "putPoolRegistration then readPoolRegistration yields expected result"
            (property . prop_poolRegistration)
        it "putPoolRetirement then readPoolRetirement yields expected result"
            (property . prop_poolRetirement)
        it "prop_multiple_putPoolRegistration_single_readPoolRegistration"
            (property .
                prop_multiple_putPoolRegistration_single_readPoolRegistration)
        it "prop_multiple_putPoolRetirement_single_readPoolRetirement"
            (property .
                prop_multiple_putPoolRetirement_single_readPoolRetirement)
        it "readPoolLifeCycleStatus should respect registration order"
            (property . prop_readPoolLifeCycleStatus)
        it "rollback of PoolRegistration"
            (property . prop_rollbackRegistration)
        it "rollback of PoolRetirement"
            (property . prop_rollbackRetirement)
        it "readStake . putStake a1 . putStake s0 == pure a1"
            (property . prop_putStakePutStake)
        it "readSystemSeed is idempotent"
            (property . prop_readSystemSeedIdempotent)
        it "putPoolRegistration . listRegisteredPools yield pools"
            (property . prop_listRegisteredPools)
        it "putPoolProduction* . readTotalProduction matches expectations"
            (property . prop_readTotalProduction)
        it "unfetchedPoolMetadataRefs"
            (property . prop_unfetchedPoolMetadataRefs)
        it "unfetchedPoolMetadataRefsIgnoring"
            (property . prop_unfetchedPoolMetadataRefsIgnoring)
        it "prop_determinePoolLifeCycleStatus_orderCorrect"
            (property . const
                prop_determinePoolLifeCycleStatus_orderCorrect)
        it "prop_determinePoolLifeCycleStatus_neverRegistered"
            (property . const
                prop_determinePoolLifeCycleStatus_neverRegistered)
        it "prop_determinePoolLifeCycleStatus_differentPools"
            (property . const
                prop_determinePoolLifeCycleStatus_differentPools)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition

-- | Can read put pool production
prop_putReadPoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        atomically cleanDB
        db'@DBLayer{cleanDB=cleanDB',atomically=atomically'} <- MVar.newDBLayer
        atomically' cleanDB'
        pure db'
    prop
        DBLayer
            { atomically = atomically'
            , putPoolProduction = putPoolProduction'
            , readPoolProduction = readPoolProduction'
            }
        = do
        run . atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        run . atomically' $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction' slot pool
        monitor $ classify (length pairs > 100) "productions > 100"
        monitor $ classify (length pairs > 1000) "productions > 1000"
        run . forM_ (uniqueEpochs pairs) $ \epoch -> do
            res' <- atomically' $ readPoolProduction' epoch
            atomically (readPoolProduction epoch) `shouldReturn` res'

prop_readTotalProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readTotalProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        atomically cleanDB
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
    prop = do
        production <- run $ atomically readTotalProduction
        monitor $ counterexample ("from database: " <> show production)
        let production'
                = Map.map Quantity
                $ Map.fromListWith (+)
                $ second (const 1) <$> pairs
        assert (production == production')

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrPointAlreadyExists slot
            atomically (runExceptT $ putPoolProduction slot pool) `shouldReturn` Right ()
            atomically (runExceptT $ putPoolProduction slot pool) `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> SlotId
    -> Property
prop_rollbackPools db@DBLayer{..} f@(StakePoolsFixture pairs _) sl =
    monadicIO prop
  where
    prop = do
        (beforeRollback, afterRollback) <- run $ do
            atomically $ forM_ pairs $ \(pool, point) ->
                runExceptT $ putPoolProduction point pool
            before <- map fst <$> allPoolProduction db f
            atomically $ rollbackTo sl
            after <- map fst <$> allPoolProduction db f
            pure (before, after)

        monitor $ counterexample $ unlines
            [ "Rollback point:    " <> showSlot sl
            , "Production before: " <> unwords (map showSlot beforeRollback)
            , "Production after:  " <> unwords (map showSlot afterRollback)
            ]
        monitor $ classify (any (> sl) beforeRollback) "something to roll back"
        monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"

        assert $ all (<= sl) afterRollback

    showSlot s = T.unpack $ pretty s

-- | Last element of cursor is the tip
prop_readPoolProductionCursorTipIsLast
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionCursorTipIsLast DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = do
        run $ atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        tip <- run $ atomically $ last <$> readPoolProductionCursor 2
        assert $ tip == snd (head pairs)

-- | Can read pool production only for a given epoch
prop_readPoolNoEpochLeaks
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolNoEpochLeaks DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    slotPartition = L.groupBy ((==) `on` epochNumber)
        $ L.sortOn epochNumber
        $ map (view #slotId . snd) pairs
    epochGroups = L.zip (uniqueEpochs pairs) slotPartition
    setup = liftIO $ atomically cleanDB
    prop = run $ do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ epochGroups $ \(epoch, slots) -> do
            slots' <- Set.fromList . map (view #slotId) . concat . Map.elems
                <$> atomically (readPoolProduction epoch)
            slots' `shouldBe` (Set.fromList slots)

-- | Read pool production satisfies conditions after consecutive
-- 1-slot-depth rollbacks
prop_readPoolCondAfterDeterministicRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterDeterministicRollbacks cond DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    slots = map (view #slotId . snd) pairs
    prop = run $ do
        atomically $ forM_ pairs $ \(pool, point) ->
            unsafeRunExceptT $ putPoolProduction point pool
        forM_ slots $ \slot -> do
            _ <- atomically $ rollbackTo slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- atomically $ readPoolProduction epoch
                cond res

-- | Read pool production satisfies conditions after consecutive
-- arbitrary N-slot-depth rollbacks
prop_readPoolCondAfterRandomRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterRandomRollbacks cond DBLayer{..} (StakePoolsFixture pairs rSlots) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = do
        run $ atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        run $ forM_ rSlots $ \slot -> do
            atomically $ rollbackTo slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- atomically $ readPoolProduction epoch
                cond res
        monitor $ classify (length pairs <= 10) "number of slots <= 10"
        monitor $ classify (length pairs > 10) "number of slots > 10"

-- | Read pool production satisfies condition
prop_readPoolCond
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCond cond DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = liftIO $ do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res <- atomically $ readPoolProduction epoch
            cond res

-- | read . put == pure
prop_putStakeReadStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakeReadStake DBLayer{..} epoch distribution =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        run $ atomically $ putStakeDistribution epoch distribution
        distribution' <- run $ atomically $ readStakeDistribution epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show distribution' ]
        monitor $ classify (null distribution) "Empty distributions"
        assert (L.sort distribution' == L.sort distribution)

-- | read $ put B $ put A == B
prop_putStakePutStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakePutStake DBLayer {..} epoch a b =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        run . atomically $ putStakeDistribution epoch a
        run . atomically $ putStakeDistribution epoch b
        res <- run . atomically $ readStakeDistribution epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show res ]
        monitor $ classify (null a) "a is empty"
        monitor $ classify (null b) "b is empty"
        monitor $ classify (null a && null b) "a & b are empty"
        assert (L.sort res == L.sort b)

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRegistration
    :: DBLayer IO
    -> [(CertificatePublicationTime, PoolRegistrationCertificate)]
    -> Property
prop_poolRegistration DBLayer {..} entries =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    entriesIn = L.sort entries
    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRegistration) entriesIn
        entriesOut <- run . atomically $ L.sort . catMaybes
            <$> mapM (readPoolRegistration . view #poolId . snd) entries
        monitor $ counterexample $ unlines
            [ "Written into DB: "
            , show entriesIn
            , "Read from DB: "
            , show entriesOut
            ]
        assert (entriesIn == entriesOut)

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRetirement
    :: DBLayer IO
    -> [(CertificatePublicationTime, PoolRetirementCertificate)]
    -> Property
prop_poolRetirement DBLayer {..} entries =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    entriesIn = L.sort entries
    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) entriesIn
        entriesOut <- run . atomically $ L.sort . catMaybes
            <$> mapM (readPoolRetirement . view #poolId . snd) entries
        monitor $ counterexample $ unlines
            [ "Written into DB: "
            , show entriesIn
            , "Read from DB: "
            , show entriesOut
            ]
        assert (entriesIn == entriesOut)

-- For the same pool, write /multiple/ pool registration certificates to the
-- database and then read back the current registration certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRegistration_single_readPoolRegistration
    :: DBLayer IO
    -> PoolId
    -> [PoolRegistrationCertificate]
    -> Property
prop_multiple_putPoolRegistration_single_readPoolRegistration
    DBLayer {..} sharedPoolId certificatesManyPoolIds =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRegistration) certificatePublications
        mRetrievedCertificatePublication <-
            run $ atomically $ readPoolRegistration sharedPoolId
        monitor $ counterexample $ unlines
            [ "\nExpected certificate publication: "
            , show mExpectedCertificatePublication
            , "\nRetrieved certificate publication: "
            , show mRetrievedCertificatePublication
            , "\nNumber of certificate publications: "
            , show (length certificatePublications)
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> certificatePublications)
            ]
        assert $ (==)
            mRetrievedCertificatePublication
            mExpectedCertificatePublication

    certificatePublications
        :: [(CertificatePublicationTime, PoolRegistrationCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    mExpectedCertificatePublication = certificatePublications
        & reverse
        & listToMaybe

    publicationTimes =
        [ CertificatePublicationTime (SlotId en sn) ii
        | en <- [0 ..]
        , sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    certificates = set #poolId sharedPoolId <$> certificatesManyPoolIds

-- For the same pool, write /multiple/ pool retirement certificates to the
-- database and then read back the current retirement certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRetirement_single_readPoolRetirement
    :: DBLayer IO
    -> PoolId
    -> [PoolRetirementCertificate]
    -> Property
prop_multiple_putPoolRetirement_single_readPoolRetirement
    DBLayer {..} sharedPoolId certificatesManyPoolIds =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) certificatePublications
        mRetrievedCertificatePublication <-
            run $ atomically $ readPoolRetirement sharedPoolId
        monitor $ counterexample $ unlines
            [ "\nExpected certificate publication: "
            , show mExpectedCertificatePublication
            , "\nRetrieved certificate publication: "
            , show mRetrievedCertificatePublication
            , "\nNumber of certificate publications: "
            , show (length certificatePublications)
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> certificatePublications)
            ]
        assert $ (==)
            mRetrievedCertificatePublication
            mExpectedCertificatePublication

    certificatePublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    mExpectedCertificatePublication = certificatePublications
        & reverse
        & listToMaybe

    publicationTimes =
        [ CertificatePublicationTime (SlotId en sn) ii
        | en <- [0 ..]
        , sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    certificates = set #poolId sharedPoolId <$> certificatesManyPoolIds

-- After writing an /arbitrary/ sequence of interleaved registration and
-- retirement certificates for the same pool to the database, verify that
-- reading the current lifecycle status returns a result that reflects
-- the correct order of precedence for these certificates.
--
-- Note that this property /assumes/ the correctness of the pure function
-- 'determinePoolLifeCycleStatus', which is tested /elsewhere/ with
-- the @prop_determinePoolLifeCycleStatus@ series of properties.
--
prop_readPoolLifeCycleStatus
    :: DBLayer IO
    -> PoolId
    -> [PoolCertificate]
    -> Property
prop_readPoolLifeCycleStatus
    DBLayer {..} sharedPoolId certificatesManyPoolIds =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    expectedStatus = determinePoolLifeCycleStatus
        mFinalRegistration
        mFinalRetirement

    prop = do
        actualStatus <-
            run $ atomically $ do
                mapM_ (uncurry putCertificate) certificatePublications
                readPoolLifeCycleStatus sharedPoolId
        monitor $ counterexample $ unlines
            [ "\nFinal registration: "
            , show mFinalRegistration
            , "\nFinal retirement: "
            , show mFinalRetirement
            , "\nExpected status: "
            , show expectedStatus
            , "\nActual status: "
            , show actualStatus
            , "\nNumber of certificate publications: "
            , show (length certificatePublications)
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> certificatePublications)
            ]
        assert (actualStatus == expectedStatus)

    certificatePublications :: [(CertificatePublicationTime, PoolCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    mFinalRegistration = certificatePublications
        & reverse
        & fmap (traverse toRegistrationCertificate)
        & catMaybes
        & listToMaybe

    mFinalRetirement = certificatePublications
        & reverse
        & fmap (traverse toRetirementCertificate)
        & catMaybes
        & listToMaybe

    publicationTimes =
        [ CertificatePublicationTime (SlotId en sn) ii
        | en <- [0 ..]
        , sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    certificates = setPoolId sharedPoolId <$> certificatesManyPoolIds

    toRegistrationCertificate = \case
        Registration cert -> Just cert
        Retirement _ -> Nothing

    toRetirementCertificate = \case
        Retirement cert -> Just cert
        Registration _ -> Nothing

    putCertificate cpt = \case
        Registration cert ->
            putPoolRegistration cpt cert
        Retirement cert ->
            putPoolRetirement cpt cert

    setPoolId newPoolId = \case
        Registration cert -> Registration
            $ set #poolId newPoolId cert
        Retirement cert -> Retirement
            $ set #poolId newPoolId cert

prop_rollbackRegistration
    :: DBLayer IO
    -> SlotId
    -> [(CertificatePublicationTime, PoolRegistrationCertificate)]
    -> Property
prop_rollbackRegistration DBLayer{..} rollbackPoint entries =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    beforeRollback pool = do
        case L.find (on (==) (view #poolId) pool . snd) entries of
            Nothing ->
                error "unknown pool?"
            Just (CertificatePublicationTime sl _, pool') ->
                (sl <= rollbackPoint) && (pool == pool')

    ownerHasManyPools =
        let owners = concatMap (poolOwners . snd) entries
        in L.length owners > L.length (L.nub owners)

    prop = do
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries
        run . atomically $ rollbackTo rollbackPoint
        pools <- run . atomically $ L.sort . fmap snd . catMaybes
            <$> mapM (readPoolRegistration . (view #poolId) . snd) entries
        monitor $ classify (length pools < length entries) "rolled back some"
        monitor $ classify ownerHasManyPools "owner has many pools"
        monitor $ counterexample $ unlines
            [ "Read from DB:   " <> show pools
            ]
        assert (all beforeRollback pools)

-- Verify that retirement certificates are correctly rolled back.
--
prop_rollbackRetirement
    :: DBLayer IO
    -> [PoolRetirementCertificate]
    -> Property
prop_rollbackRetirement DBLayer{..} certificates =
    checkCoverage
        $ cover 4 (rollbackPoint == slotMinBound)
            "rollbackPoint = slotMinBound"
        $ cover 4 (rollbackPoint > slotMinBound)
            "rollbackPoint > slotMinBound"
        $ cover 4 (null expectedPublications)
            "length expectedPublications = 0"
        $ cover 4 (not (null expectedPublications))
            "length expectedPublications > 0"
        $ cover 4
            ( (&&)
                (not (null expectedPublications))
                (length expectedPublications < length allPublications)
            )
            "0 < length expectedPublications < length allPublications"
        $ monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) allPublications
        run $ atomically $ rollbackTo rollbackPoint
        retrievedPublications <- catMaybes <$>
            run (atomically $ mapM readPoolRetirement poolIds)
        monitor $ counterexample $ unlines
            [ "\nRollback point: "
            , show rollbackPoint
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> allPublications)
            , "\nExpected certificate publications: "
            , unlines (("\n" <>) . show <$> expectedPublications)
            , "\nRetrieved certificate publications: "
            , unlines (("\n" <>) . show <$> retrievedPublications)
            ]
        assert $ (==)
            retrievedPublications
            expectedPublications

    poolIds :: [PoolId]
    poolIds = view #poolId <$> certificates

    rollbackPoint :: SlotId
    rollbackPoint =
        -- Pick a slot that approximately corresponds to the midpoint of the
        -- certificate publication list.
        publicationTimes
            & drop (length certificates `div` 2)
            & fmap (view #slotId)
            & listToMaybe
            & fromMaybe slotMinBound

    allPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    allPublications = publicationTimes `zip` certificates

    expectedPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    expectedPublications =
        filter
            (\(CertificatePublicationTime slotId _, _) ->
                slotId <= rollbackPoint)
            allPublications

    publicationTimes :: [CertificatePublicationTime]
    publicationTimes =
        [ CertificatePublicationTime (SlotId en sn) ii
        | en <- [0 ..]
        , sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

prop_listRegisteredPools
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_listRegisteredPools DBLayer {..} entries =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    hasDuplicateOwners PoolRegistrationCertificate{poolOwners} =
        L.nub poolOwners /= poolOwners

    prop = do
        let entries' =
                [ CertificatePublicationTime (SlotId ep 0) minBound
                | ep <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'
        pools <- run . atomically $ listRegisteredPools
        monitor $ classify (any hasDuplicateOwners entries)
            "same owner multiple time in the same certificate"
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show pools
            ]
        assert (pools == (view #poolId <$> reverse entries))

prop_unfetchedPoolMetadataRefs
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefs DBLayer{..} entries =
    monadicIO (setup >> propWellFormedResult >> propInteractionWithPutPoolMetadata)
  where
    setup = do
        run . atomically $ cleanDB
        let entries' =
                [ CertificatePublicationTime (SlotId ep 0) minBound
                | ep <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'
        monitor $ classify (length entries > 10) "10+ entries"
        monitor $ classify (length entries > 50) "50+ entries"

    propWellFormedResult = do
        let hashes = snd <$> mapMaybe poolMetadata entries
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        monitor $ counterexample $ unlines
            [ "Read from DB (" <> show (length refs) <> "): " <> show refs
            ]
        assertWith "fewer unfetchedPoolMetadataRefs than registrations"
            (length refs <= length entries)
        assertWith "all metadata hashes are indeed known"
            (all ((`elem` hashes) . snd) refs)
        assertWith "no duplicate"
            (L.nub refs == refs)

    propInteractionWithPutPoolMetadata = do
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        unless (null refs) $ do
            let [(url, hash)] = take 1 refs
            metadata <- pick $ genStakePoolMetadata url
            run . atomically $ putPoolMetadata hash metadata
            refs' <- run . atomically $ unfetchedPoolMetadataRefs 10
            monitor $ counterexample $ unlines
                [ "Read from DB (" <> show (length refs') <> "): " <> show refs'
                ]
            assertWith "fetching metadata removes it from unfetchedPoolMetadataRefs"
                (hash `notElem` (snd <$> refs'))

prop_unfetchedPoolMetadataRefsIgnoring
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefsIgnoring DBLayer{..} entries =
    length metas >= 2 ==> monadicIO (setup >> propIgnoredMetadataRefs)
  where
    metas = mapMaybe poolMetadata entries

    setup = do
        run . atomically $ cleanDB
        let entries' =
                [ CertificatePublicationTime (SlotId ep 0) minBound
                | ep <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'

    propIgnoredMetadataRefs = do
        let recent = head metas
        run . atomically $ putFetchAttempt recent
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        monitor $ counterexample $ unlines
            [ "Read from DB (" <> show (length refs) <> "): " <> show refs
            ]
        assertWith "recently failed URLs are ignored"
            (recent `notElem` refs)

-- | successive readSystemSeed yield the exact same value
prop_readSystemSeedIdempotent
    :: DBLayer IO
    -> Positive Int
    -> Property
prop_readSystemSeedIdempotent DBLayer{..} (Positive n) =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        seeds <- map show <$> replicateM n (run $ atomically readSystemSeed)
        let firstS = head seeds
        monitor $ counterexample $ show seeds
        monitor $ counterexample $ show $ filter (/= firstS) seeds
        assert (all (== firstS) seeds)

prop_determinePoolLifeCycleStatus_orderCorrect
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
    => (certificatePublicationTime, PoolRegistrationCertificate)
    -> (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_orderCorrect regData retData =
    checkCoverage
        $ cover 10 (regTime > retTime)
            "registration cert time > retirement cert time"
        $ cover 10 (regTime < retTime)
            "registration cert time < retirement cert time"
        $ cover 2 (regTime == retTime)
            "registration cert time = retirement cert time"
        $ property prop
  where
    prop
        | regTime > retTime =
            -- A re-registration always /supercedes/ a prior retirement.
            result `shouldBe` PoolRegistered regCert
        | regTime < retTime =
            -- A retirement always /augments/ the latest registration.
            result `shouldBe` PoolRegisteredAndRetired regCert retCert
        | otherwise =
            -- If a registration certificate and a retirement certificate
            -- for the same pool appear to have been published at exactly
            -- the same time, this represents a programming error.
            evaluate result `shouldThrow` anyException

    sharedPoolId = view #poolId regCertAnyPool

    (regTime, regCertAnyPool) = regData
    (retTime, retCertAnyPool) = retData

    regCert = set #poolId sharedPoolId regCertAnyPool
    retCert = set #poolId sharedPoolId retCertAnyPool

    result = determinePoolLifeCycleStatus
        (pure (regTime, regCert))
        (pure (retTime, retCert))

-- If we've never seen a registration certificate for a given pool, we /always/
-- indicate that the pool was /not registered/, /regardless/ of whether or not
-- we've seen a retirement certificate for that pool.
--
prop_determinePoolLifeCycleStatus_neverRegistered
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
    => Maybe (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_neverRegistered maybeRetData =
    checkCoverage
        $ cover 10 (isJust maybeRetData)
            "with retirement data"
        $ cover 10 (isNothing maybeRetData)
            "without retirement data"
        $ property
        $ result `shouldBe` PoolNotRegistered
  where
    result = determinePoolLifeCycleStatus Nothing maybeRetData

-- Calling 'determinePoolLifeCycleStatus' with certificates from different
-- pools is a programming error, and should result in an exception.
--
prop_determinePoolLifeCycleStatus_differentPools
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
    => (certificatePublicationTime, PoolRegistrationCertificate)
    -> (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_differentPools regData retData =
    property $ (regPoolId /= retPoolId) ==> prop
  where
    prop = evaluate result `shouldThrow` anyException

    regPoolId = view #poolId regCert
    retPoolId = view #poolId retCert

    (regTime, regCert) = regData
    (retTime, retCert) = retData

    result = determinePoolLifeCycleStatus
        (pure (regTime, regCert))
        (pure (retTime, retCert))

descSlotsPerPool :: Map PoolId [BlockHeader] -> Expectation
descSlotsPerPool pools = do
    let checkIfDesc slots =
            L.sortOn Down slots == slots
    let pools' = Map.filter checkIfDesc pools
    pools' `shouldBe` pools

noEmptyPools :: Map PoolId [BlockHeader] -> Expectation
noEmptyPools pools = do
    let pools' = Map.filter (not . null) pools
    pools' `shouldBe` pools

uniqueEpochs :: [(PoolId, BlockHeader)] -> [EpochNo]
uniqueEpochs = nubOrd . map (epochNumber . view #slotId . snd)

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotId, PoolId)]
allPoolProduction DBLayer{..} (StakePoolsFixture pairs _) = atomically $
    rearrange <$> mapM readPoolProduction (uniqueEpochs pairs)
  where
    rearrange ms = concat
        [ [ (view #slotId h, p) | h <- hs ]
        | (p, hs) <- concatMap Map.assocs ms
        ]
