{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Test stub modules for various components
module TestSupport.TestStubs (
  -- Dependencies stub
  module TestSupport.TestStubs.Dependencies,
  -- DependentTypesParser stub
  module TestSupport.TestStubs.DependentTypes,
  -- Ownership stub
  module TestSupport.TestStubs.Ownership
) where

import TestSupport.TestStubs.Dependencies
import TestSupport.TestStubs.DependentTypes
import TestSupport.TestStubs.Ownership