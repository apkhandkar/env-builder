# env-builder

A `servant`-like way to work with environment variables.

## Usage

```haskell
data AppEnv = AppEnv
  { port :: Int
  , logToDb :: Bool
  , connectionPool :: Pool Connection
  }
```

Define the type for your "builder" function using the provided combinators:

```haskell
type BuildEnv =
  "PORT" `As` Int
    ~> "LOG_TO_DB" `As` Bool
    ~> "DB_CONN_STRING" `AsPgConnPool` With (TimeoutSecs 60) (MaxResources 20)
    ~> Builds AppEnv DieOnError
```

And a corresponding builder function:

```haskell
buildEnv :: Int -> Bool -> Pool Connection -> IO AppEnv
buildEnv port logToDb connectionPool = pure $ AppEnv{..} -- or go ham within IO
```

Use `runBuilder` to tie it all together: 

```haskell
main :: IO ()
main = do
  appEnv <- runBuilder (Proxy @BuildEnv) buildEnv
  onlyOne <- withResource (connectionPool appEnv) $ \conn -> do
    query_ @(Only Int) conn "SELECT 1"
  print onlyOne 
```
