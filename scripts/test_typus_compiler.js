#!/usr/bin/env node

const { execSync } = require('child_process');

function testCompilerBugFix() {
    console.log('Testing Typus compiler bug fix...');

    // Test 1: Simple variable declaration
    const simpleTest = `
package main

import "fmt"

func main() {
    total := 0
    fmt.Printf("total = %d\n", total)
}`;

    try {
        execSync(`echo '${simpleTest}' > /tmp/simple_test.typus`);
        execSync(`./dist-newstyle/build/x86_64-linux/ghc-9.6.7/typus-0.1.0/x/typus/build/typus/typus check /tmp/simple_test.typus`, { stdio: 'pipe' });
        console.log('✅ Test 1 PASSED: Simple variable declarations compile correctly');
    } catch (error) {
        console.log('❌ Test 1 FAILED:', error.message);
        process.exit(1);
    }

    // Test 2: Complex file compilation
    try {
        execSync(`./dist-newstyle/build/x86_64-linux/ghc-9.6.7/typus-0.1.0/x/typus/build/typus/typus check 250921.typus`, { stdio: 'pipe' });
        console.log('✅ Test 2 PASSED: Complex file (250921.typus) compiles correctly');
    } catch (error) {
        console.log('❌ Test 2 FAILED:', error.message);
        process.exit(1);
    }

    // Test 3: Check for problematic "Fixed short declaration" comments
    try {
        const result = execSync(`./dist-newstyle/build/x86_64-linux/ghc-9.6.7/typus-0.1.0/x/typus/build/typus/typus check 250921.typus`, { stdio: 'pipe', encoding: 'utf8' });
        if (result.stdout.includes('Fixed short declaration')) {
            console.log('❌ Test 3 FAILED: Still generating "Fixed short declaration" comments');
            process.exit(1);
        } else {
            console.log('✅ Test 3 PASSED: No "Fixed short declaration" comments found');
        }
    } catch (error) {
        console.log('❌ Test 3 FAILED:', error.message);
        process.exit(1);
    }

    // Test 4: Verify := syntax preservation
    try {
        const result = execSync(`./dist-newstyle/build/x86_64-linux/ghc-9.6.7/typus-0.1.0/x/typus/build/typus/typus check 250921.typus`, { stdio: 'pipe', encoding: 'utf8' });
        const hasCorrectAssignment = result.stdout.includes('total := 0') &&
                                   result.stdout.includes('count := 0') &&
                                   result.stdout.includes('ch := make(chan int)');

        if (hasCorrectAssignment) {
            console.log('✅ Test 4 PASSED: := syntax correctly preserved');
        } else {
            console.log('❌ Test 4 FAILED: := syntax not properly preserved');
            process.exit(1);
        }
    } catch (error) {
        console.log('❌ Test 4 FAILED:', error.message);
        process.exit(1);
    }

    console.log('\n🎉 All tests passed! The Typus compiler bug has been successfully fixed.');
    console.log('Summary:');
    console.log('- Simple variable declarations work correctly');
    console.log('- Complex files compile without errors');
    console.log('- No "Fixed short declaration" comments generated');
    console.log('- := syntax is properly preserved in function-local variables');
}

if (require.main === module) {
    testCompilerBugFix();
}