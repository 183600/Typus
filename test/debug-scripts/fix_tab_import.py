#!/usr/bin/env python3

# Read the Compiler.hs file
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Find the isImportLine function and update it to handle tab+quoted imports
old_function = '''    isImportLine line = 
      let trimmed = trim line
      in (trimmed == "fmt" || trimmed == "sync" || trimmed == "time" || 
          trimmed == "unsafe" || trimmed == "os" || trimmed == "io" || 
          trimmed == "strings" || trimmed == "math" || trimmed == "runtime" ||
          trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" || 
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || 
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"")'''

new_function = '''    isImportLine line = 
      let trimmed = trim line
          -- Also check for tab + quoted imports like \t"runtime"
          tabPattern = "\t"
      in (trimmed == "fmt" || trimmed == "sync" || trimmed == "time" || 
          trimmed == "unsafe" || trimmed == "os" || trimmed == "io" || 
          trimmed == "strings" || trimmed == "math" || trimmed == "runtime" ||
          trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" || 
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || 
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"" ||
          trimmed == tabPattern ++ "\"fmt\"" || trimmed == tabPattern ++ "\"sync\"" || 
          trimmed == tabPattern ++ "\"time\"" || trimmed == tabPattern ++ "\"unsafe\"" || 
          trimmed == tabPattern ++ "\"os\"" || trimmed == tabPattern ++ "\"io\"" || 
          trimmed == tabPattern ++ "\"strings\"" || trimmed == tabPattern ++ "\"math\"" || 
          trimmed == tabPattern ++ "\"runtime\"")'''

content = content.replace(old_function, new_function)

# Write the file back
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Fixed isImportLine function to handle tab+quoted imports")