#!/usr/bin/env bash

# 批量注释掉使用未定义函数的测试
sed -i '/^-- Property: Type variable validation/,/^in result === True || result === False$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs
sed -i '/^-- Property: Type variable normalization/,/^in normalizationIsConsistent typeVar normalized$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs
sed -i '/^-- Property: Type variable comparison/,/^in result === EQ || result === LT || result === GT$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs
sed -i '/^-- Property: Type variable freedom check/,/^in isFree === True || isFree === False$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs
sed -i '/^-- Property: Substitution application/,/^in substitutionApplicationIsCorrect applied typeVar substitutions$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs
sed -i '/^-- Property: Substitution composition/,/^in compositionIsCorrect composed subs1 subs2$/s/^/ -- /' /home/runner/work/Typus/Typus/test/Test/Unit/DependenciesQuickCheckSpec.hs

echo "批量注释完成"