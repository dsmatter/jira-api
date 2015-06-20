module Jira.API.Authentication.KeyUtils where

import           Control.Applicative
import           Crypto.Types.PubKey.RSA (PrivateKey (..), PublicKey (..))
import           Data.Maybe
import           OpenSSL.EVP.PKey        (toKeyPair, toPublicKey)
import           OpenSSL.PEM             (PemPasswordSupply (..),
                                          readPrivateKey, readPublicKey)
import           OpenSSL.RSA

openSslRsaPrivateKeyFromPem :: String -> IO RSAKeyPair
openSslRsaPrivateKeyFromPem pemString = fromJust . toKeyPair <$> readPrivateKey pemString PwNone

openSslRsaPublicKeyFromPem :: String -> IO RSAPubKey
openSslRsaPublicKeyFromPem pemString = fromJust . toPublicKey <$> readPublicKey pemString

fromOpenSslPrivateKey :: RSAKeyPair -> PrivateKey
fromOpenSslPrivateKey rsaKey =
  let publicKey = fromOpenSslPublicKey rsaKey
  in  PrivateKey publicKey (rsaD rsaKey) 0 0 0 0 0

fromOpenSslPublicKey :: RSAKey k => k -> PublicKey
fromOpenSslPublicKey rsaKey = PublicKey (rsaSize rsaKey) (rsaN rsaKey) (rsaE rsaKey)

readPemPrivateKey :: String -> IO PrivateKey
readPemPrivateKey = fmap fromOpenSslPrivateKey . openSslRsaPrivateKeyFromPem

readPemPublicKey :: String -> IO PublicKey
readPemPublicKey = fmap fromOpenSslPublicKey . openSslRsaPublicKeyFromPem
