#!/usr/bin/env python3

import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# Replace SyntaxError with Parsing
content = content.replace('SyntaxError', 'Parsing')

# Replace ErrorFatal with Fatal
content = content.replace('ErrorFatal', 'Fatal')

# Replace execState (addError error) collector with execState (addError error) []
content = re.sub(r'execState \(addError error\) collector', 'execState (addError error) []', content)
content = re.sub(r'execState \(addWarning error\) collector', 'execState (addWarning error) []', content)
content = re.sub(r'execState \(addInfo error\) collector', 'execState (addInfo error) []', content)

# Replace foldl with execState (mapM_ ...)
content = re.sub(r'foldl \(c msg -> execState \(addError \(mkError msg\)\) c\) newErrorCollector msgs', 'execState (mapM_ (addError . mkError) msgs) []', content)

# Replace addError c msg span with addError (mkError msg)
content = re.sub(r'addError c msg span', 'addError (mkError msg)', content)
content = re.sub(r'addWarning c msg span', 'addWarning (mkWarning msg)', content)
content = re.sub(r'addInfo c msg span', 'addInfo (mkInfo msg)', content)

# Replace getErrors collector with getErrors (execState (return ()) [])
content = re.sub(r'getErrors collector', 'getErrors errors', content)
content = re.sub(r'getWarnings collector', 'getWarnings warnings', content)
content = re.sub(r'getInfo collector', 'getInfo infos', content)

# Replace teMessage with message
content = content.replace('teMessage errors', 'map (T.unpack . message) errors')
content = content.replace('teMessage warnings', 'map (T.unpack . message) warnings')
content = content.replace('teMessage infos', 'map (T.unpack . message) infos')

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CoreErrorHandlerQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed API mismatches")