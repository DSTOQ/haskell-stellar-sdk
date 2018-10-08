module Stellar.CryptoSpec where

import           Control.Lens
import           Control.Monad           (fail)
import           Control.Newtype         (unpack)
import qualified Crypto.PubKey.Ed25519   as ED
import           Data.Binary.Extended    (encode)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Base64  as B64
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BSL
import           Data.Text.Encoding      (encodeUtf8)
import           Hedgehog
import           Protolude
import qualified Stellar.Core.Lens       as L
import           Stellar.Core.Types
import           Stellar.Crypto
import           Stellar.Gens


run :: IO Bool
run = checkParallel $ Group "Stellar.CryptoSpec"
  [ ("Sign 'hello world'", prop_signature)
  , ("Sign transaction", prop_sign_transaction)
  , ("Sign transaction envelope", prop_sign_envelope)
  ]

prop_sign_envelope :: Property
prop_sign_envelope = property $ do
  keys <- forAll genKeyPair
  tx <- forAll genTransactionEnvelope
  network <- forAll genNetwork
  let tx' = signTransactionEnvelope network keys tx
  length (tx' ^. L.signatures) === length (tx ^. L.signatures) + 1

prop_sign_transaction :: Property
prop_sign_transaction = withShrinks 1 $ withTests 1 $ property $ do
  secret <- either (fail . show) pure
    $ parseSecretKey "SCH27VUZZ6UAKB67BDNF6FA42YMBMQCBKXWGMFD5TZ6S5ZZCZFLRXKHS"
  destination <- either (fail . show) pure
    $ parsePublicKey "GDW6AUTBXTOC7FIKUO5BOO3OGLK4SF7ZPOBLMQHMZDI45J2Z6VXRB5NR"
  let keys = keyPair' secret
      tx = Transaction
        { _sourceAccount = keys ^. L.publicKey
        , _fee = FeeStroops 100
        , _seqNum = unsafeSequenceNumber 2908908335136769
        , _timeBounds = Nothing
        , _memo = MemoNone
        , _operations =
          [ Operation
            { _sourceAccount = Nothing
            , _body = CreateAccount $ CreateAccountOp
              { _destination = destination
              , _startingBalance = Stroop 20000000000
              }
            }
          ]
        }
      envelope = signTransactionAsEnvelope Testnet keys tx
      encoded = (B64.encode . BSL.toStrict . encode) envelope
  encoded ===
    "AAAAAF7FIiDToW1fOYUFBC0dmyufJbFTOa2GQESGz+S2h5ViAAAAZAAKVaMAAAABAAAAAAAAA\
    \AAAAAABAAAAAAAAAAAAAAAA7eBSYbzcL5UKo7oXO24y1ckX+XuCtkDsyNHOp1n1bxAAAAAEqB\
    \fIAAAAAAAAAAABtoeVYgAAAEDLki9Oi700N60Lo8gUmEFHbKvYG4QSqXiLIt9T0ru2O5BphVl\
    \/jR9tYtHAD+UeDYhgXNgwUxqTEu1WukvEyYcD"


prop_signature :: Property
prop_signature = withShrinks 1 $ withTests 1 $ property $ do
  secret <- either (fail . show) pure
    $ parseSecretKey "SC6ZH63HM2AB5M2RIUNMNRKM26KQTKWIKH4MY5PMTXBZPVOHCXB5FUNX"
  public <- either (fail . show) pure
    $ parsePublicKey "GCVXHAFVVRGBKHKPODVHBE4UENYCEARM2IK355YDNX5ZMXX3V7YIWBBI"
  let sig = ED.sign (unpack secret) (unpack public) (encodeUtf8 "hello world")
  hexEdSignature sig ===
     "6d9076609c36bba3e94d6a686a2bd40c9356598c49c0a247af48ff625b444162\
     \77afdbed011cbf81970fd7cdb7c4a430d1726cdd3288ce829c2797e884dd1d0f"


hexDecoratedSignature :: DecoratedSignature -> Text
hexDecoratedSignature =  hexSignature . view L.signature

hexSignature :: Signature -> Text
hexSignature = hexEdSignature . unpack

hexEdSignature :: ED.Signature -> Text
hexEdSignature = hexByteString . BA.convert

hexByteString :: ByteString -> Text
hexByteString = toS . toLazyByteString . byteStringHex
