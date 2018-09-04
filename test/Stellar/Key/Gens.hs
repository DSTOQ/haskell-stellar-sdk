module Stellar.Key.Gens where

import           Hedgehog
import qualified Hedgehog.Gen.Extended as Gen
import           Protolude


newtype SecretKeyText = SecretKeyText Text deriving (Eq, Show)

genSecretKeyText :: Gen SecretKeyText
genSecretKeyText = SecretKeyText <$> Gen.element
 [ "SBM2P34FJWQ6L2OPB2PYYWMVELEZP3GCM62QGZ2MYDEUMDUO7LPAJVTV"
 , "SDMGML6Z7WYMNY4SEAF4UTMMYD2JJUD22Q5TSH6KCLWKNTBS5P3JSZWL"
 , "SANOKDUGKISMIGK3U33UYT2YQET4SJJZPMASK2LM44W5EINRK43IMJXO"
 , "SCQ224PKWZIPKRF6HBHG3TZEIIQKBIKXW6RVFUKIHHPK4MZFY4KBONZ2"
 , "SDF5H3CUGX7II2IZ5XMOQDFCMCWZWY7QUQT4LP4FXVCCGY5ZL737E7KO"
 , "SDOXGP6QETPUPS4F7NGDUPTXSQQ4FHPUZI7IF32YOK7FCJ4T2E7SP7JM"
 , "SBI3JQVGDSWYBTPWXTCYYURUMAZ7UOYICDM2S72EVDKRQWPRPKH3GQGM"
 , "SAY666T7IQPZIMZHT2QWNUGXZMR25CSDS6VU3SNLL7DB4GNX2JG2PKNL"
 , "SDDYVMVH5OEPAVSXFDVVBTKMCDCYNJYUQKN67VEFIIPBLS4M2PVUTCPO"
 , "SC73IPZYRC6PGS7WLHRG4X46A4Y4JZ6XBSIYPSMOYW5A4DXDVHEBMOT4"
 , "SA6OFJGVZVXRFACIJ5Y6NVLWBELGKIF4PZEYTPOJLGDYSK2K6MDV67FQ"
 ]

newtype PublicKeyText = PublicKeyText Text deriving (Eq, Show)

genPublicKeyText :: Gen PublicKeyText
genPublicKeyText = PublicKeyText <$> Gen.element
  [ "GA4IBZJDA2K3JWC3L6XOKJLHCKES63HXVI2XCDN4SZ7FODVNL2QJY6GF"
  , "GCNSGHUCG5VMGLT5RIYYZSO7VQULQKAJ62QA33DBC5PPBSO57LFWVV6P"
  , "GAOMLS5MFQNPNUP6AALYAAH2OWTATB5ICS6XWTZ364UVGSDQVMFBKRMH"
  , "GBB7JKBP5ZG7UUHAOYDOHQMIVDRKNMXTCDU3WUDVRV77NZJBEJNL4F2H"
  , "GCZ6KEBDPV6AJ7E4VISM5VKAK6ZR3CMIKRBO2TMZHT37NHLBL34OVJOZ"
  , "GAREELUB43IRHWEASCFBLKHURCGMHE5IF6XSE7EXDLACYHGRHM43RFOX"
  , "GA77B6GK5K3FH2YJ6I5VJ7VPFZKPBQUX2IIC2MJYAERQTGJI4VOPKRYJ"
  , "GCIM3EHKJAUURMEYETQ2ZG4DSP2PAJDDVLYDSCROC2ELXGYJBM6PF4EH"
  , "GBROS6JISSGRJRMWMC2GVD2YTFFZ7L24VCCE5D76P6LWO7D5GDD5ABJG"
  , "GCKTBRR7KIR34J2U6MIL3O2UCQT6LW7U25TFX7ZVBHH2MLCSDCRF64PD"
  , "GDV5U356L5MZF4J6NM5UWWXCMEJRZU35YWLWY5E7PMQKBF64VW36KCDE"
  ]
