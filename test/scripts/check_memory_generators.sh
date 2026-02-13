#!/bin/bash

echo "Checking MemoryEfficientGenerators.hs..."
cd /home/runner/work/Typus/Typus

# Try to compile just the MemoryEfficientGenerators module
stack ghc -- -c test/TestSupport/MemoryEfficientGenerators.hs -Wall 2>&1 | head -20

echo "Exit code: $?"