{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Network.HTTP.Types (status400, status200)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import System.Environment (lookupEnv)

-- Corpo do request vindo do frontend
data CompoundRequest = CompoundRequest
  { principal    :: Double
  , rate         :: Double
  , timesPerYear :: Int
  , years        :: Double
  } deriving (Show, Generic)

instance FromJSON CompoundRequest

-- Corpo do response de sucesso
data CompoundResponse = CompoundResponse
  { amount :: Double
  } deriving (Show, Generic)

instance ToJSON CompoundResponse

-- Response de erro
data ErrorResponse = ErrorResponse
  { message :: Text
  } deriving (Show, Generic)

instance ToJSON ErrorResponse

-- Validação de entradas
validateRequest :: CompoundRequest -> Either Text CompoundRequest
validateRequest req
  | principal req <= 0 =
      Left "principal deve ser maior que 0."
  | rate req < 0 =
      Left "rate (taxa) não pode ser negativa."
  | timesPerYear req < 1 =
      Left "timesPerYear deve ser pelo menos 1."
  | years req <= 0 =
      Left "years deve ser maior que 0."
  | otherwise =
      Right req

-- Fórmula de juros compostos: A = P * (1 + r/n)^(n*t)
compoundAmount :: Double -> Double -> Int -> Double -> Double
compoundAmount p r n t =
  let n' = fromIntegral n
  in p * (1 + r / n') ** (n' * t)

-- CORS básico
setCors :: ActionM ()
setCors = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
  addHeader "Access-Control-Allow-Headers" "Content-Type"

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = maybe 8080 read mPort

  scotty port $ do
    -- Preflight CORS
    options "/api/compound" $ do
      setCors
      status status200

    -- Endpoint principal
    post "/api/compound" $ do
      setCors

      -- Se o JSON for inválido, o Scotty já devolve 400 automaticamente
      req <- jsonData :: ActionM CompoundRequest

      case validateRequest req of
        Left msg -> do
          status status400
          json (ErrorResponse msg)

        Right okReq -> do
          let p = principal okReq
              r = rate okReq
              n = timesPerYear okReq
              t = years okReq

              result = compoundAmount p r n t

          json (CompoundResponse result)
