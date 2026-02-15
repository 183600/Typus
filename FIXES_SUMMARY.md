# 修复总结

## 原始错误
`/home/runner/work/Typus/Typus/test/Test/Unit/AdvancedIntegrationQuickCheckTests.hs:172:52: error: [GHC-83865]`

## 修复的问题

### 1. 类型错误：formatCompilerErrors 期望 [CompilerError] 但收到 CompilerResult String
**原因**: `compile` 函数返回 `CompilerResult String`（即 `Either [CompilerError] String`），但 `formatCompilerErrors` 期望 `[CompilerError]`

**修复**: 创建了 `extractErrors` 辅助函数从 `CompilerResult` 中提取错误列表
```haskell
extractErrors :: CompilerResult a -> [CompilerError]
extractErrors (Left errs) = errs
extractErrors (Right _) = []
```

### 2. 导入问题：缺少 rights 和 guard
**修复**: 添加到导入列表
```haskell
import Data.Either (isLeft, isRight, rights)
import Control.Monad (when, unless, replicateM, guard)
```

### 3. analyzeOwnershipFile 类型错误
**原因**: `analyzeOwnershipFile :: FilePath -> IO [OwnershipError]` 期望 `FilePath`，不是 `TypusFile`

**修复**: 使用 `analyzeOwnership :: String -> [OwnershipError]` 和 `tfContents :: TypusFile -> String`
```haskell
ownershipAnalyses = map (analyzeOwnership . tfContents) validModules
```

### 4. checkDependentTypes 类型错误
**原因**: `checkDependentTypes :: TypusFile -> CompilerResult ()` 不需要 `typeSystem` 参数

**修复**: 移除不必要的参数
```haskell
dependentTypeAnalyses = map checkDependentTypes validModules
```

### 5. guard 返回类型错误
**原因**: `guard` 返回 `()` 而不是 `Property`，不能在属性测试中直接使用

**修复**: 使用 `if-then-else` 替代 `guard`

### 6. CompilerError 没有 Ord 实例
**原因**: `sort` 需要 `Ord` 实例，但 `CompilerError` 没有

**修复**: 对 `CompilerError` 的字符串表示进行排序
```haskell
sort (map show (baseErrors ++ newErrors)) == sort (map show combinedErrors)
```

### 7. 变量作用域问题
**原因**: 在 `where` 子句中引用 `let` 绑定中的变量

**修复**: 简化代码结构，避免作用域问题

## 验证
项目成功编译，表明所有类型错误已修复。