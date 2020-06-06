{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Byron.Api.Server
    ( server
    ) where

import Prelude

import Cardano.Wallet
    ( ErrCreateRandomAddress (..)
    , ErrNotASequentialWallet (..)
    , ErrValidateSelection
    , addressScheme
    , genesisData
    , networkLayer
    )
import Cardano.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , ByronAddresses
    , ByronMigrations
    , ByronTransactions
    , ByronWallets
    , CoinSelections
    , Network
    , Proxy_
    , ShelleyMigrations
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( deleteTransaction
    , deleteWallet
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWallet
    , liftHandler
    , listAddresses
    , listTransactions
    , listWallets
    , migrateWallet
    , mkLegacyWallet
    , postAccountWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postTransaction
    , postTransactionFee
    , postTrezorWallet
    , putByronWalletPassphrase
    , putRandomAddress
    , putWallet
    , rndStateChange
    , selectCoins
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( ApiT (..), SomeByronWalletPostData (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState, seqGenChange )
import Control.Applicative
    ( liftA2 )
import Control.Monad.Trans.Except
    ( throwE )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn )
import Fmt
    ( Buildable )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Server, err501, throwError )

-- | A diminished servant server to serve Byron wallets only.
server
    :: forall t.  ( Buildable (ErrValidateSelection t))
    => NetworkDiscriminant
    -> ApiLayer RndState t ByronKey
    -> ApiLayer (SeqState IcarusKey) t IcarusKey
    -> NtpClient
    -> Server Api
server _n byron icarus ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> shelleyMigrations
    :<|> stakePools
    :<|> byronWallets
    :<|> byronAddresses
    :<|> byronCoinSelections
    :<|> byronTransactions
    :<|> byronMigrations
    :<|> network
    :<|> proxy
  where
    wallets :: Server Wallets
    wallets =
             (\_ -> throwError err501)
        :<|> (\_ -> throwError err501)
        :<|> throwError err501
        :<|> (\_ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ -> throwError err501)

    addresses :: Server Addresses
    addresses _ _ = throwError err501

    coinSelections :: Server CoinSelections
    coinSelections _ _ = throwError err501

    transactions :: Server Transactions
    transactions =
             (\_ _ -> throwError err501)
        :<|> (\_ _ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    shelleyMigrations :: Server ShelleyMigrations
    shelleyMigrations =
             (\_   -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    stakePools :: Server StakePools
    stakePools =
             throwError err501
        :<|> (\_ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ -> throwError err501)

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            RandomWalletFromMnemonic x -> postRandomWallet byron x
            RandomWalletFromXPrv x -> postRandomWalletFromXPrv byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
            SomeAccount x -> postAccountWallet icarus (mkLegacyWallet @_ @_ @IcarusKey) IcarusKey x
        )
        :<|> (\wid -> withLegacyLayer wid
                (byron , deleteWallet byron wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer' wid
                ( byron
                , fst <$> getWallet byron  (mkLegacyWallet @_ @RndState @ByronKey) wid
                , const (fst <$> getWallet byron  (mkLegacyWallet @_ @RndState @ByronKey) wid)
                )
                ( icarus
                , fst <$> getWallet icarus (mkLegacyWallet @_ @_ @IcarusKey) wid
                , const (fst <$> getWallet icarus (mkLegacyWallet @_ @_ @IcarusKey) wid)
                )
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  (mkLegacyWallet @_ @_ @ByronKey))
            (listWallets icarus (mkLegacyWallet @_ @_ @IcarusKey))
        :<|> (\wid name -> withLegacyLayer wid
                (byron , putWallet byron (mkLegacyWallet @_ @_ @ByronKey) wid name)
                (icarus, putWallet icarus (mkLegacyWallet @_ @_ @IcarusKey) wid name)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , getUTxOsStatistics byron wid)
                (icarus, getUTxOsStatistics icarus wid)
             )
        :<|> (\wid pwd -> withLegacyLayer wid
                (byron , putByronWalletPassphrase byron wid pwd)
                (icarus, putByronWalletPassphrase icarus wid pwd)
             )

    byronAddresses :: Server ByronAddresses
    byronAddresses =
             (\wid s -> withLegacyLayer wid
                (byron, postRandomAddress byron wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid addr -> withLegacyLayer wid
                (byron, putRandomAddress byron wid addr)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (byron , listAddresses byron (const pure) wid s)
                (icarus, listAddresses icarus (const pure) wid s)
             )

    byronCoinSelections :: Server CoinSelections
    byronCoinSelections wid x = withLegacyLayer wid
        (byron, liftHandler $ throwE ErrNotASequentialWallet)
        (icarus, selectCoins icarus (seqGenChange $ icarus ^. addressScheme) wid x)

    byronTransactions :: Server ByronTransactions
    byronTransactions =
             (\wid tx -> withLegacyLayer wid
                 (byron , do
                    let pwd = coerce (getApiT $ tx ^. #passphrase)
                    genChange <- rndStateChange @_ @RndState byron wid pwd
                    postTransaction @_ @RndState byron genChange wid tx
                 )
                 (icarus, do
                    let genChange = seqGenChange (icarus ^. addressScheme)
                    postTransaction icarus genChange wid tx
                 )
             )
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron wid r0 r1 s)
                (icarus, listTransactions icarus wid r0 r1 s)
             )
        :<|>
            (\wid tx -> withLegacyLayer wid
                (byron , postTransactionFee byron wid tx)
                (icarus, postTransactionFee icarus wid tx)
            )
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )

    byronMigrations :: Server ByronMigrations
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo byron wid)
                (icarus, getMigrationInfo icarus wid)
             )
        :<|> (\wid m -> withLegacyLayer wid
                (byron , migrateWallet byron wid m)
                (icarus, migrateWallet icarus wid m)
             )

    network :: Server Network
    network =
        getNetworkInformation genesis nl
        :<|> (getNetworkParameters genesis)
        :<|> (getNetworkClock ntp)
      where
        nl = icarus ^. networkLayer @t
        genesis = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus
