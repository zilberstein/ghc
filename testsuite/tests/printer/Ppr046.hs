{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MagicHash, UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Test10313 where

import "b\x61se" Data.List

{-# WARNING Logic
          , solverCheckAndGetModel
          "New Z3 API support is still incomplete and fragile: \
          \you may experience segmentation faults!"
  #-}

{-# Deprecated Logic
          , solverCheckAndGetModel
          "Deprecation: \
          \you may experience segmentation faults!"
  #-}

data {-# ctype "foo\x63" "b\x61r" #-} Logic = Logic

-- Should warn
foo1 x = x
{-# RULEs "foo1\x67" [ 1] forall x. foo1 x = x #-}

foreign import prim unsafe "a\x62" a :: IO Int

{-# INLINe strictStream #-}
strictStream (Bitstream l v)
    = {-# CORe "Strict Bitstream stre\x61m" #-}
      S.concatMap stream (GV.stream v)
      `S.sized`
      Exact l

b = {-# SCc "foo\x64"   #-} 006
