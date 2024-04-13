{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Builder
  ( As
  , Builds
  , AsPgConnPool
  , With
  , TimeoutSecs
  , MaxResources
  , type (~>)
  , DieOnError
  , ReturnEither
  , runBuilder
  ) where

import Control.Exception (Exception (..))
import Control.Monad.Except
  ( ExceptT
  , MonadError
  , runExceptT
  , throwError
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Kind (Type)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeOf)
import Database.PostgreSQL.Simple
  ( Connection
  , close
  , connectPostgreSQL
  )
import GHC.TypeLits
  ( ErrorMessage (Text)
  , KnownNat
  , KnownSymbol
  , Nat
  , Symbol
  , TypeError
  , natVal
  , symbolVal
  )
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)

data EnvError
  = KeyNotFound String
  | EnvParseFailure String String
  deriving (Show)

instance Exception EnvError where
  displayException (KeyNotFound key) =
    "Could not parse environment: Key '"
      <> key
      <> "' not found."
  displayException (EnvParseFailure key parseAs) =
    "Could not parse " <> key <> " as " <> parseAs <> "."

newtype EnvReaderM a = EnvReaderM {runEnvReaderM :: ExceptT EnvError IO a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError EnvError)

-- | Actions that must run to populate builder arguments.
data EnvReader result where
  EnvReader
    :: { envRead :: EnvReaderM envBuildResult
       , envBuild :: envBuildResult -> result
       }
    -> EnvReader result

instance Functor EnvReader where
  fmap f EnvReader{..} =
    EnvReader
      { envBuild = \r -> f $ envBuild r
      , ..
      }

addEnvRead
  :: EnvReader (a -> b) -> EnvReaderM a -> EnvReader b
addEnvRead EnvReader{..} newCheck' =
  EnvReader
    { envRead = (,) <$> envRead <*> newCheck'
    , envBuild = \(p, pNew') -> ($ pNew') $ envBuild p
    , ..
    }

runEnvReader
  :: EnvReader result -> IO (Either EnvError result)
runEnvReader = runExceptT . runEnvReaderM . runEnvReader'
 where
  runEnvReader' EnvReader{..} = do
    v <- envRead
    pure $ envBuild v

emptyEnvReader :: a -> EnvReader a
emptyEnvReader res = EnvReader r (\_ -> res)
 where
  r = return ()

data a ~> b

infixr 4 ~>

class HasEnvBuilder a where
  type EnvBuilder a :: Type

  type BuildResult a :: Type

  build
    :: Proxy a -> EnvReader (EnvBuilder a) -> BuildResult a

-- | Parse an environment variable as a type.
data As (var :: Symbol) (parseAs :: Type)

instance
  (KnownSymbol var, HasEnvBuilder rest)
  => HasEnvBuilder (var `As` String ~> rest)
  where
  type
    EnvBuilder (var `As` String ~> rest) =
      String -> EnvBuilder rest

  type
    BuildResult (var `As` String ~> rest) =
      BuildResult rest

  build Proxy builder = build (Proxy @rest) (builder `addEnvRead` parseString)
   where
    parseString = EnvReaderM $ do
      let envVar = symbolVal (Proxy @var)
      valStrMay <- liftIO $ lookupEnv envVar
      case valStrMay of
        Nothing -> throwError (KeyNotFound envVar)
        Just valStr -> pure valStr

instance
  (KnownSymbol var, HasEnvBuilder rest)
  => HasEnvBuilder (var `As` ByteString ~> rest)
  where
  type
    EnvBuilder (var `As` ByteString ~> rest) =
      ByteString -> EnvBuilder rest

  type
    BuildResult (var `As` ByteString ~> rest) =
      BuildResult rest

  build Proxy builder =
    build (Proxy @rest) (builder `addEnvRead` parseByteString)
   where
    parseByteString = do
      let envVar = symbolVal (Proxy @var)
      valStrMay <- liftIO $ lookupEnv envVar
      case valStrMay of
        Nothing -> throwError (KeyNotFound envVar)
        Just valStr -> pure $ C8.pack valStr

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol var
  , HasEnvBuilder rest
  , Read parseAs
  , Typeable parseAs
  )
  => HasEnvBuilder (var `As` parseAs ~> rest)
  where
  type
    EnvBuilder (var `As` parseAs ~> rest) =
      parseAs -> EnvBuilder rest

  type
    BuildResult (var `As` parseAs ~> rest) =
      BuildResult rest

  build Proxy builder = build (Proxy @rest) (builder `addEnvRead` parseReadable)
   where
    parseReadable = do
      let envVar = symbolVal (Proxy @var)
      valStrMay <- liftIO $ lookupEnv envVar
      case valStrMay of
        Nothing -> throwError (KeyNotFound envVar)
        Just valStr -> case readMaybe @parseAs valStr of
          Just parsed -> pure parsed
          Nothing ->
            throwError $
              EnvParseFailure
                envVar
                (show $ typeOf (undefined :: parseAs))

type TerminatingAsTypeError =
  TypeError
    ( Text
        "\"As\" cannot be used to terminate an EnvBuilder expression"
    )

instance HasEnvBuilder (var `As` parseAs) where
  type EnvBuilder (var `As` parseAs) = TerminatingAsTypeError
  type BuildResult (var `As` parseAs) = TerminatingAsTypeError
  build _ _ = undefined

-- | Build a postgresql-simple connection pool given an environment variable
-- that stores a connection string and basic config
data AsPgConnPool (pgConnStringVar :: Symbol) withConfig

-- | For verbosity
data With timeout maxResources

-- | How long should unused connections be kept alive
data TimeoutSecs (timeout :: Nat)

-- | Maximum number of connections to keep open across all stripes
data MaxResources (maxResources :: Nat)

instance
  ( KnownSymbol pgConnStringVar
  , KnownNat timeout
  , KnownNat maxResources
  , HasEnvBuilder rest
  )
  => HasEnvBuilder
      ( pgConnStringVar
          `AsPgConnPool` (With (TimeoutSecs timeout) (MaxResources maxResources))
          ~> rest
      )
  where
  type
    EnvBuilder
      ( pgConnStringVar
          `AsPgConnPool` (With (TimeoutSecs timeout) (MaxResources maxResources))
          ~> rest
      ) =
      Pool Connection -> EnvBuilder rest

  type
    BuildResult
      ( pgConnStringVar
          `AsPgConnPool` ( With
                            (TimeoutSecs timeout)
                            (MaxResources maxResources)
                         )
          ~> rest
      ) =
      BuildResult rest

  build Proxy builder = build (Proxy @rest) (builder `addEnvRead` createPool')
   where
    createPool' = do
      let envVar = symbolVal (Proxy @pgConnStringVar)
      valStrMay <- liftIO $ lookupEnv envVar
      case valStrMay of
        Nothing -> throwError (KeyNotFound envVar)
        Just (C8.pack -> pgConnString) -> do
          let timeout' = fromInteger $ natVal (Proxy @timeout)
          let maxResources' = fromInteger $ natVal (Proxy @maxResources)
          connectionPool <-
            liftIO $
              newPool
                ( defaultPoolConfig
                    (connectPostgreSQL pgConnString)
                    close
                    timeout'
                    maxResources'
                )
          pure connectionPool

type TerminatingAsPgConnPoolTypeError =
  TypeError
    ( Text
        "\"AsPgConnPool\" cannot be used to terminate an EnvBuilder expression"
    )

instance HasEnvBuilder (pgConnStringVar `AsPgConnPool` withConfig) where
  type
    EnvBuilder (pgConnStringVar `AsPgConnPool` withConfig) =
      TerminatingAsPgConnPoolTypeError
  type
    BuildResult (pgConnStringVar `AsPgConnPool` withConfig) =
      TerminatingAsPgConnPoolTypeError
  build _ _ = undefined

-- | Terminates all EnvBuilder expressions.
data Builds result onError

-- | On error, print error message to stderror and exit with an error code.
data DieOnError

-- | Return builder result as 'Either EnvError'.
data ReturnEither

instance HasEnvBuilder (Builds result DieOnError) where
  type EnvBuilder (Builds result DieOnError) = IO result

  type BuildResult (Builds result DieOnError) = IO result

  build Proxy builder = postBuild $ runEnvReader builder
   where
    postBuild builderAction = do
      result <- builderAction
      case result of
        Left err -> die $ displayException err
        Right action -> action

instance HasEnvBuilder (Builds result ReturnEither) where
  type EnvBuilder (Builds result ReturnEither) = IO result

  type
    BuildResult (Builds result ReturnEither) =
      IO (Either EnvError result)

  build Proxy builder = postBuild $ runEnvReader builder
   where
    postBuild builderAction = do
      result <- builderAction
      case result of
        Left err -> pure $ Left err
        Right action -> do
          result' <- action
          pure $ Right result'

type NonTerminatingBuildTypeError =
  TypeError
    ( Text
        "Build can only be used to terminate an EnvBuilder expression"
    )

instance HasEnvBuilder (Builds result onError ~> rest) where
  type
    EnvBuilder (Builds result onError ~> rest) =
      NonTerminatingBuildTypeError
  type
    BuildResult (Builds result onError ~> rest) =
      NonTerminatingBuildTypeError
  build _ _ = undefined

-- * High-level interface

-- | Run an 'EnvBuilder' and return its result
runBuilder
  :: forall a
   . HasEnvBuilder a
  => Proxy a
  -> EnvBuilder a
  -> BuildResult a
runBuilder proxy builder = build proxy (emptyEnvReader builder)
