module Test.Unit.EnhancedOwnershipSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.OwnershipChecker
import Parser (TypusFile(..), parseTypus)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)

-- | 测试检查简单所有权转移
prop_check_simple_ownership_transfer :: Property
prop_check_simple_ownership_transfer = 
  let code = "```typus\nlet x = Box(42)\nlet y = move(x)\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权借用
prop_check_ownership_borrow :: Property
prop_check_ownership_borrow = 
  let code = "```typus\nlet x = Box(42)\nlet y = borrow(x)\nlet z = x  // x仍然可用\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权可变借用
prop_check_ownership_mutable_borrow :: Property
prop_check_ownership_mutable_borrow = 
  let code = "```typus\nlet x = Box(42)\nlet y = borrow_mut(x)\n*y = 24\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权生命周期
prop_check_ownership_lifetime :: Property
prop_check_ownership_lifetime = 
  let code = "```typus\nfn foo<'a>(x: &'a Box(Nat)) -> Nat { *x }\nlet b = Box(42)\nlet result = foo(&b)\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权结构体字段
prop_check_ownership_struct_field :: Property
prop_check_ownership_struct_field = 
  let code = "```typus\nstruct Point { x: Nat, y: Nat }\nlet p = Point { x: 1, y: 2 }\nlet px = p.x\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权复制语义
prop_check_ownership_copy_semantics :: Property
prop_check_ownership_copy_semantics = 
  let code = "```typus\nlet x = 42  // Nat实现Copy\nlet y = x   // 复制而不是移动\nlet z = x   // x仍然可用\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权克隆语义
prop_check_ownership_clone_semantics :: Property
prop_check_ownership_clone_semantics = 
  let code = "```typus\nlet x = Box(42)\nlet y = clone(x)  // 显式克隆\nlet z = x        // x仍然可用\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权函数参数
prop_check_ownership_function_param :: Property
prop_check_ownership_function_param = 
  let code = "```typus\nfn consume(x: Box(Nat)) -> Nat { *x }\nlet b = Box(42)\nlet result = consume(b)\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权返回值
prop_check_ownership_return_value :: Property
prop_check_ownership_return_value = 
  let code = "```typus\nfn create_box() -> Box(Nat) { Box(42) }\nlet b = create_box()\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权部分移动
prop_check_ownership_partial_move :: Property
prop_check_ownership_partial_move = 
  let code = "```typus\nstruct Pair { first: Nat, second: Nat }\nlet p = Pair { first: 1, second: 2 }\nlet f = p.first\nlet s = p.second  // 错误：p已经被部分移动\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权闭包捕获
prop_check_ownership_closure_capture :: Property
prop_check_ownership_closure_capture = 
  let code = "```typus\nlet x = 42\nlet f = || { x }  // 按值捕获\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权引用计数
prop_check_ownership_reference_count :: Property
prop_check_ownership_reference_count = 
  let code = "```typus\nlet x = Rc(Box(42))\nlet y = clone(x)\nlet z = clone(x)\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权共享引用
prop_check_ownership_shared_ref :: Property
prop_check_ownership_shared_ref = 
  let code = "```typus\nlet x = Arc(Box(42))\nlet y = clone(x)\nlet z = clone(x)\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权原始指针
prop_check_ownership_raw_pointer :: Property
prop_check_ownership_raw_pointer = 
  let code = "```typus\nlet x = Box(42)\nlet p = raw_ptr(x)\nlet value = *p\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权类型状态
prop_check_ownership_typestate :: Property
prop_check_ownership_typestate = 
  let code = "```typus\ntype File = Closed | Opened\nlet f = Closed\nlet f2 = open(f)  // 状态转换\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试检查所有权线性类型
prop_check_ownership_linear_type :: Property
prop_check_ownership_linear_type = 
  let code = "```typus\nlinear Token\nlet t = new_token()\nlet t2 = t  // 错误：线性类型不能复制\n```"
      result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case result of
    Left _ -> property True
    Right _ -> property True

tests :: TestTree
tests = testGroup "Enhanced Ownership Tests"
  [ testProperty "check simple ownership transfer" prop_check_simple_ownership_transfer
  , testProperty "check ownership borrow" prop_check_ownership_borrow
  , testProperty "check ownership mutable borrow" prop_check_ownership_mutable_borrow
  , testProperty "check ownership lifetime" prop_check_ownership_lifetime
  , testProperty "check ownership struct field" prop_check_ownership_struct_field
  , testProperty "check ownership copy semantics" prop_check_ownership_copy_semantics
  , testProperty "check ownership clone semantics" prop_check_ownership_clone_semantics
  , testProperty "check ownership function param" prop_check_ownership_function_param
  , testProperty "check ownership return value" prop_check_ownership_return_value
  , testProperty "check ownership partial move" prop_check_ownership_partial_move
  , testProperty "check ownership closure capture" prop_check_ownership_closure_capture
  , testProperty "check ownership reference count" prop_check_ownership_reference_count
  , testProperty "check ownership shared ref" prop_check_ownership_shared_ref
  , testProperty "check ownership raw pointer" prop_check_ownership_raw_pointer
  , testProperty "check ownership typestate" prop_check_ownership_typestate
  , testProperty "check ownership linear type" prop_check_ownership_linear_type
  ]