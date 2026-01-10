#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestPerformanceRegressionSpec.hs', 'r') as f:
    content = f.read()

# Fix the syntax error by removing the <- and using let bindings
content = re.sub(r', testCase "SourceLocation: mergeSpans performance on many spans" \$',
                r', testCase "SourceLocation: mergeSpans performance on many spans" $', content)
content = re.sub(r'let spans = \[spanBetween \(posAt i 1\) \(posAt i 100\) \| i <- \[1\.\.1000\]\]\s+startTime <- getCPUTime\s+let result = foldl mergeSpans \(head spans\) \(tail spans\)\s+endTime <- getCPUTime\s+timeDiff = fromIntegral \(endTime - startTime\) / \(10\^12\)\s+in timeDiff < 0\.1 @\? \("mergeSpans took too long: " \+\+ show timeDiff \+\+ " seconds"\)',
                r'let spans = [spanBetween (posAt i 1) (posAt i 100) | i <- [1..1000]]\n          startTime = getCPUTime\n          result = foldl mergeSpans (head spans) (tail spans)\n          endTime = getCPUTime\n          timeDiff = fromIntegral (endTime - startTime) / (10^12)\n      in timeDiff < 0.1 @? ("mergeSpans took too long: " ++ show timeDiff ++ " seconds")', content, flags=re.MULTILINE | re.DOTALL)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestPerformanceRegressionSpec.hs', 'w') as f:
    f.write(content)

print("Fixed syntax error in TestPerformanceRegressionSpec.hs")