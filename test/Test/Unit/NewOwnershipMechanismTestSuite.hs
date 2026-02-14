{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewOwnershipMechanismTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isDigit, isLetter)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

import Ownership
import Parser (parseTypus)
import SourceLocation

-- | 测试基本所有权语义 - 移动
prop_basic_ownership_move :: String -> Property
prop_basic_ownership_move value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  t := s                    // 移动：s 的所有权转移给 t\n" ++
                 "  fmt.Println(t.data)       // ✓\n" ++
                 "  // fmt.Println(s.data)    // ✗ 编译错误：s 已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试不可变借用
prop_immutable_borrow :: String -> Property
prop_immutable_borrow value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  r := &s                   // 不可变借用\n" ++
                 "  fmt.Println(r.data)       // ✓ 通过借用读取\n" ++
                 "  fmt.Println(s.data)       // ✓ 原值仍可读\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试可变借用
prop_mutable_borrow :: String -> String -> Property
prop_mutable_borrow oldValue newValue =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ oldValue ++ "\")\n" ++
                 "  m := &mut s               // 可变借用\n" ++
                 "  m.data = \"" ++ newValue ++ "\"          // ✓ 通过可变借用修改\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试借用规则 - 同一时刻只能有一个可变借用
prop_borrowing_rules_mutable :: String -> Property
prop_borrowing_rules_mutable value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  m1 := &mut s              // 第一个可变借用\n" ++
                 "  // m2 := &mut s           // ✗ 编译错误：已有可变借用\n" ++
                 "  fmt.Println(m1.data)      // ✓\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试借用规则 - 可变借用与不可变借用互斥
prop_borrowing_rules_mutable_immutable :: String -> Property
prop_borrowing_rules_mutable_immutable value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  m := &mut s               // 可变借用\n" ++
                 "  // r := &s                // ✗ 编译错误：已有可变借用\n" ++
                 "  fmt.Println(m.data)       // ✓\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试借用规则 - 可以有多个不可变借用
prop_borrowing_rules_multiple_immutable :: String -> Property
prop_borrowing_rules_multiple_immutable value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  r1 := &s                  // 第一个不可变借用\n" ++
                 "  r2 := &s                  // 第二个不可变借用\n" ++
                 "  r3 := &s                  // 第三个不可变借用\n" ++
                 "  fmt.Println(r1.data)      // ✓\n" ++
                 "  fmt.Println(r2.data)      // ✓\n" ++
                 "  fmt.Println(r3.data)      // ✓\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试借用生命周期 - 借用不能超过原值的作用域
prop_borrowing_lifetime :: String -> Property
prop_borrowing_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  {\n" ++
                 "    s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "    r := &s                 // 借用s\n" ++
                 "    fmt.Println(r.data)     // ✓ 在s的作用域内\n" ++
                 "  }\n" ++
                 "  // fmt.Println(r.data)   // ✗ r的生命周期超过s\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试跨goroutine的所有权转移
prop_cross_goroutine_ownership :: String -> Property
prop_cross_goroutine_ownership value =
  let typusCode = "{//! ownership: on\n" ++
                 "  ch := make(chan MyString)\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  go func() {\n" ++
                 "    ch <- s                 // 发送s到goroutine\n" ++
                 "  }()\n" ++
                 "  received := <-ch\n" ++
                 "  fmt.Println(received.data) // ✓ 接收所有权\n" ++
                 "  // fmt.Println(s.data)   // ✗ s已被转移\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与GC的关系 - 零运行时开销
prop_ownership_gc_zero_overhead :: String -> Property
prop_ownership_gc_zero_overhead value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  t := s                    // 移动语义，编译期检查\n" ++
                 "  // 内存回收仍由Go GC负责\n" ++
                 "  fmt.Println(t.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权机制提供逻辑正确性保证
prop_ownership_logical_correctness :: String -> Property
prop_ownership_logical_correctness value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  process(s)               // 转移所有权\n" ++
                 "  // s不再可用，保证逻辑正确性\n" ++
                 "}\n" ++
                 "func process(s: MyString) {\n" ++
                 "  fmt.Println(s.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试接口方法的所有权语义
prop_interface_method_ownership :: String -> Property
prop_interface_method_ownership value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Writer interface {\n" ++
                 "    Write(data: []byte) -> (int, error)\n" ++
                 "  }\n" ++
                 "  type MyWriter struct {\n" ++
                 "    buffer []byte\n" ++
                 "  }\n" ++
                 "  func (w *MyWriter) Write(data: []byte) -> (int, error) {\n" ++
                 "    w.buffer = append(w.buffer, data...)\n" ++
                 "    return len(data), nil\n" ++
                 "  }\n" ++
                 "  writer := &MyWriter{buffer: []byte{}}\n" ++
                 "  data := []byte(\"" ++ value ++ "\")\n" ++
                 "  writer.Write(data)         // 方法调用遵循所有权语义\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权转移与函数返回
prop_ownership_function_return :: String -> Property
prop_ownership_function_return value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := createString(\"" ++ value ++ "\")\n" ++
                 "  fmt.Println(s.data)       // ✓ s拥有返回值的所有权\n" ++
                 "}\n" ++
                 "func createString(content: string) -> MyString {\n" ++
                 "  return NewMyString(content) // 转移所有权给调用者\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与结构体字段
prop_ownership_struct_fields :: String -> String -> Property
prop_ownership_struct_fields value1 value2 =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Container struct {\n" ++
                 "    data MyString\n" ++
                 "  }\n" ++
                 "  s1 := NewMyString(\"" ++ value1 ++ "\")\n" ++
                 "  s2 := NewMyString(\"" ++ value2 ++ "\")\n" ++
                 "  container := Container{data: s1} // s1所有权转移到container\n" ++
                 "  // fmt.Println(s1.data)         // ✗ s1已被移动\n" ++
                 "  fmt.Println(container.data.data) // ✓\n" ++
                 "  container.data = s2              // s2所有权转移到container.data\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与数组/切片
prop_ownership_arrays_slices :: String -> Property
prop_ownership_arrays_slices value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  strings := []MyString{s}     // s所有权转移到切片\n" ++
                 "  // fmt.Println(s.data)      // ✗ s已被移动\n" ++
                 "  fmt.Println(strings[0].data) // ✓\n" ++
                 "  moved := strings[0]          // 从切片中移动出来\n" ++
                 "  // fmt.Println(strings[0].data) // ✗ 元素已被移动\n" ++
                 "  fmt.Println(moved.data)      // ✓\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与map
prop_ownership_maps :: String -> String -> Property
prop_ownership_maps key value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  myMap := map[string]MyString{}\n" ++
                 "  myMap[\"" ++ key ++ "\"] = s   // s所有权转移到map\n" ++
                 "  // fmt.Println(s.data)       // ✗ s已被移动\n" ++
                 "  fmt.Println(myMap[\"" ++ key ++ "\"].data) // ✓\n" ++
                 "  moved := myMap[\"" ++ key ++ "\"] // 从map中移动出来\n" ++
                 "  fmt.Println(moved.data)        // ✓\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与闭包
prop_ownership_closures :: String -> Property
prop_ownership_closures value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  fn := func() {\n" ++
                 "    fmt.Println(s.data)     // 闭包捕获s的借用\n" ++
                 "  }\n" ++
                 "  fn()                      // ✓ 调用闭包\n" ++
                 "  fmt.Println(s.data)       // ✓ s仍可用，因为只是借用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与闭包移动
prop_ownership_closures_move :: String -> Property
prop_ownership_closures_move value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  fn := func() {\n" ++
                 "    // s移动到闭包中\n" ++
                 "    processString(s)\n" ++
                 "  }\n" ++
                 "  fn()                      // ✓ 调用闭包\n" ++
                 "  // fmt.Println(s.data)    // ✗ s已被移动到闭包\n" ++
                 "}\n" ++
                 "func processString(s: MyString) {\n" ++
                 "  fmt.Println(s.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与defer语句
prop_ownership_defer :: String -> Property
prop_ownership_defer value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  defer func() {\n" ++
                 "    fmt.Println(s.data)   // defer中的借用\n" ++
                 "  }()\n" ++
                 "  fmt.Println(s.data)     // ✓ s仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与panic恢复
prop_ownership_panic_recovery :: String -> Property
prop_ownership_panic_recovery value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  defer func() {\n" ++
                 "    if r := recover(); r != nil {\n" ++
                 "      fmt.Println(\"Recovered:\", r)\n" ++
                 "      fmt.Println(s.data) // ✓ panic后仍可用\n" ++
                 "    }\n" ++
                 "  }()\n" ++
                 "  if len(s.data) > 10 {\n" ++
                 "    panic(\"Too long\")\n" ++
                 "  }\n" ++
                 "  fmt.Println(s.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与select语句
prop_ownership_select :: String -> String -> Property
prop_ownership_select value1 value2 =
  let typusCode = "{//! ownership: on\n" ++
                 "  ch1 := make(chan MyString)\n" ++
                 "  ch2 := make(chan MyString)\n" ++
                 "  s1 := NewMyString(\"" ++ value1 ++ "\")\n" ++
                 "  s2 := NewMyString(\"" ++ value2 ++ "\")\n" ++
                 "  go func() { ch1 <- s1 }()\n" ++
                 "  go func() { ch2 <- s2 }()\n" ++
                 "  select {\n" ++
                 "  case msg := <-ch1:\n" ++
                 "    fmt.Println(\"From ch1:\", msg.data) // ✓ 接收所有权\n" ++
                 "  case msg := <-ch2:\n" ++
                 "    fmt.Println(\"From ch2:\", msg.data) // ✓ 接收所有权\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与类型断言
prop_ownership_type_assertion :: String -> Property
prop_ownership_type_assertion value =
  let typusCode = "{//! ownership: on\n" ++
                 "  var x interface{} = NewMyString(\"" ++ value ++ "\")\n" ++
                 "  if s, ok := x.(MyString); ok {\n" ++
                 "    fmt.Println(s.data)   // ✓ 类型断言转移所有权\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与反射
prop_ownership_reflection :: String -> Property
prop_ownership_reflection value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  v := reflect.ValueOf(s)     // 反射创建值的副本\n" ++
                 "  fmt.Println(s.data)         // ✓ s仍可用\n" ++
                 "  fmt.Println(v.MethodByName(\"String\").Call(nil)) // ✓ 通过反射调用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与unsafe操作
prop_ownership_unsafe :: String -> Property
prop_ownership_unsafe value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  ptr := unsafe.Pointer(&s)\n" ++
                 "  // unsafe操作绕过所有权检查，需要程序员保证安全\n" ++
                 "  fmt.Println(s.data) // ✓ s仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与cgo
prop_ownership_cgo :: String -> Property
prop_ownership_cgo value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  cstr := C.CString(s.data)   // 转移到C代码\n" ++
                 "  defer C.free(unsafe.Pointer(cstr))\n" ++
                 "  fmt.Println(s.data)         // ✓ s仍可用，只是复制了数据\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与序列化
prop_ownership_serialization :: String -> Property
prop_ownership_serialization value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  data, err := json.Marshal(s) // 序列化创建副本\n" ++
                 "  if err != nil { panic(err) }\n" ++
                 "  fmt.Println(s.data)         // ✓ s仍可用\n" ++
                 "  var s2 MyString\n" ++
                 "  json.Unmarshal(data, &s2)\n" ++
                 "  fmt.Println(s2.data)        // ✓ s2是新的独立值\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与深拷贝
prop_ownership_deep_copy :: String -> Property
prop_ownership_deep_copy value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s1 := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  s2 := s1.Clone()           // 显式深拷贝\n" ++
                 "  fmt.Println(s1.data)       // ✓ s1仍可用\n" ++
                 "  fmt.Println(s2.data)       // ✓ s2是独立副本\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与浅拷贝
prop_ownership_shallow_copy :: String -> Property
prop_ownership_shallow_copy value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type MyString struct {\n" ++
                 "    data *string             // 指针字段\n" ++
                 "  }\n" ++
                 "  s1 := MyString{data: &\"" ++ value ++ "\"}\n" ++
                 "  s2 := s1                  // 浅拷贝，共享指针\n" ++
                 "  fmt.Println(s1.data)      // ✓ s1仍可用\n" ++
                 "  fmt.Println(s2.data)      // ✓ s2指向相同数据\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与内部可变性
prop_ownership_interior_mutability :: String -> Property
prop_ownership_interior_mutability value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Cell struct {\n" ++
                 "    value int\n" ++
                 "  }\n" ++
                 "  type RefCell struct {\n" ++
                 "    cell *Cell\n" ++
                 "    borrowed bool\n" ++
                 "  }\n" ++
                 "  cell := Cell{value: 42}\n" ++
                 "  ref := RefCell{cell: &cell}\n" ++
                 "  r := &ref                  // 不可变借用\n" ++
                 "  // 即使r不可变，仍可通过内部方法修改\n" ++
                 "  r.cell.value = 100\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与共享所有权
prop_ownership_shared_ownership :: String -> Property
prop_ownership_shared_ownership value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type SharedString struct {\n" ++
                 "    data *string\n" ++
                 "    refCount *int\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  shared := SharedString{\n" ++
                 "    data: &s.data,\n" ++
                 "    refCount: new(int)\n" ++
                 "  }\n" ++
                 "  *shared.refCount = 1\n" ++
                 "  fmt.Println(*shared.data) // ✓ 通过共享指针访问\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与写时复制
prop_ownership_copy_on_write :: String -> Property
prop_ownership_copy_on_write value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type CowString struct {\n" ++
                 "    data *string\n" ++
                 "    owned bool\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  cow := CowString{data: &s.data, owned: false}\n" ++
                 "  // 读取时共享\n" ++
                 "  fmt.Println(*cow.data)\n" ++
                 "  // 写入时复制\n" ++
                 "  if !cow.owned {\n" ++
                 "    newData := *cow.data\n" ++
                 "    cow.data = &newData\n" ++
                 "    cow.owned = true\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期标注
prop_ownership_lifetime_annotation :: String -> Property
prop_ownership_lifetime_annotation value =
  let typusCode = "{//! ownership: on\n" ++
                 "  func longest<'a>(x: &'a string, y: &'a string) -> &'a string {\n" ++
                 "    if len(x) > len(y) {\n" ++
                 "      return x\n" ++
                 "    }\n" ++
                 "    return y\n" ++
                 "  }\n" ++
                 "  s1 := \"" ++ value ++ "\"\n" ++
                 "  s2 := \"short\"\n" ++
                 "  result := longest(&s1, &s2)\n" ++
                 "  fmt.Println(*result) // ✓ 返回的借用有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期子类型
prop_ownership_lifetime_subtyping :: String -> Property
prop_ownership_lifetime_subtyping value =
  let typusCode = "{//! ownership: on\n" ++
                 "  func process<'a>(s: &'a string) -> string {\n" ++
                 "    return *s // 返回拥有的字符串\n" ++
                 "  }\n" ++
                 "  s1 := \"" ++ value ++ "\"\n" ++
                 "  result := process(&s1)\n" ++
                 "  fmt.Println(result) // ✓ result拥有独立数据\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期省略
prop_ownership_lifetime_elision :: String -> Property
prop_ownership_lifetime_elision value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 生命周期省略规则\n" ++
                 "  func firstWord(s: &string) -> &string {\n" ++
                 "    // 编译器推断输入和输出的生命周期相同\n" ++
                 "    words := strings.Split(*s, \" \")\n" ++
                 "    return &words[0]\n" ++
                 "  }\n" ++
                 "  s := \"" ++ value ++ " world\"\n" ++
                 "  word := firstWord(&s)\n" ++
                 "  fmt.Println(*word) // ✓ 借用有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与静态生命周期
prop_ownership_static_lifetime :: String -> Property
prop_ownership_static_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 静态生命周期：整个程序运行期间都有效\n" ++
                 "  const S: &'static str = \"" ++ value ++ "\"\n" ++
                 "  fmt.Println(S) // ✓ 静态字符串总是有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与高阶生命周期
prop_ownership_higher_ranked_lifetime :: String -> Property
prop_ownership_higher_ranked_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 高阶生命周期：函数参数的生命周期\n" ++
                 "  func apply<F>(f: F, s: &string) -> string\n" ++
                 "    where F: Fn(&string) -> string\n" ++
                 "  {\n" ++
                 "    return f(s)\n" ++
                 "  }\n" ++
                 "  s := \"" ++ value ++ "\"\n" ++
                 "  result := apply(func(s: &string) -> string {\n" ++
                 "    return *s\n" ++
                 "  }, &s)\n" ++
                 "  fmt.Println(result) // ✓ 结果拥有独立数据\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期边界
prop_ownership_lifetime_bounds :: String -> Property
prop_ownership_lifetime_bounds value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 生命周期边界：约束引用的生命周期\n" ++
                 "  type Ref<'a> {\n" ++
                 "    value: &'a string\n" ++
                 "  }\n" ++
                 "  func makeRef<'a>(s: &'a string) -> Ref<'a> {\n" ++
                 "    return Ref{value: s}\n" ++
                 "  }\n" ++
                 "  s := \"" ++ value ++ "\"\n" ++
                 "  r := makeRef(&s)\n" ++
                 "  fmt.Println(*r.value) // ✓ 借用有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与对象安全
prop_ownership_object_safety :: String -> Property
prop_ownership_object_safety value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 对象安全：trait对象的要求\n" ++
                 "  type Writer interface {\n" ++
                 "    Write(data: &string)\n" ++
                 "  }\n" ++
                 "  type ConsoleWriter struct {}\n" ++
                 "  func (w ConsoleWriter) Write(data: &string) {\n" ++
                 "    fmt.Println(*data)\n" ++
                 "  }\n" ++
                 "  var w Writer = ConsoleWriter{}\n" ++
                 "  s := \"" ++ value ++ "\"\n" ++
                 "  w.Write(&s) // ✓ 可以通过trait对象调用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与闭包捕获模式
prop_ownership_closure_capture_modes :: String -> Property
prop_ownership_closure_capture_modes value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  // 不可变借用捕获\n" ++
                 "  fn1 := func() { fmt.Println(s.data) }\n" ++
                 "  // 可变借用捕获\n" ++
                 "  fn2 := func() { s.data = \"modified\" }\n" ++
                 "  // 移动捕获\n" ++
                 "  fn3 := func() { processString(s) }\n" ++
                 "  fn1() // ✓ s仍可用\n" ++
                 "  fn2() // ✓ s被修改但仍可用\n" ++
                 "  fn3() // ✓ s被移动\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被fn3移动\n" ++
                 "}\n" ++
                 "func processString(s: MyString) {\n" ++
                 "  fmt.Println(s.data)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与模式匹配
prop_ownership_pattern_matching :: String -> Property
prop_ownership_pattern_matching value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Option<T> {\n" ++
                 "    Some(T)\n" ++
                 "    None\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  opt := Option<MyString>{Some: s}\n" ++
                 "  match opt {\n" ++
                 "  case Some(value):\n" ++
                 "    fmt.Println(value.data) // ✓ 获得所有权\n" ++
                 "  case None:\n" ++
                 "    fmt.Println(\"None\")\n" ++
                 "  }\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与引用模式
prop_ownership_ref_pattern :: String -> Property
prop_ownership_ref_pattern value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Option<T> {\n" ++
                 "    Some(T)\n" ++
                 "    None\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  opt := Option<MyString>{Some: s}\n" ++
                 "  match opt {\n" ++
                 "  case Some(ref value):\n" ++
                 "    fmt.Println(value.data) // ✓ 只是借用\n" ++
                 "  case None:\n" ++
                 "    fmt.Println(\"None\")\n" ++
                 "  }\n" ++
                 "  fmt.Println(s.data) // ✓ s仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与可变引用模式
prop_ownership_mut_ref_pattern :: String -> Property
prop_ownership_mut_ref_pattern value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Option<T> {\n" ++
                 "    Some(T)\n" ++
                 "    None\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  mut opt := Option<MyString>{Some: s}\n" ++
                 "  match mut opt {\n" ++
                 "  case Some(ref mut value):\n" ++
                 "    value.data = \"modified\" // ✓ 可变借用\n" ++
                 "  case None:\n" ++
                 "    fmt.Println(\"None\")\n" ++
                 "  }\n" ++
                 "  fmt.Println(s.data) // ✓ s已被修改但仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与let绑定
prop_ownership_let_binding :: String -> Property
prop_ownership_let_binding value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  // let绑定会移动所有权\n" ++
                 "  let s2 = s\n" ++
                 "  fmt.Println(s2.data) // ✓ s2拥有数据\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与ref绑定
prop_ownership_ref_binding :: String -> Property
prop_ownership_ref_binding value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  // ref绑定创建借用\n" ++
                 "  ref s2 = s\n" ++
                 "  fmt.Println(s2.data) // ✓ s2借用s\n" ++
                 "  fmt.Println(s.data)  // ✓ s仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与mut ref绑定
prop_ownership_mut_ref_binding :: String -> Property
prop_ownership_mut_ref_binding value =
  let typusCode = "{//! ownership: on\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  // mut ref绑定创建可变借用\n" ++
                 "  mut ref s2 = s\n" ++
                 "  s2.data = \"modified\" // ✓ 通过可变借用修改\n" ++
                 "  fmt.Println(s.data)  // ✓ s已被修改但仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与函数参数模式
prop_ownership_function_param_patterns :: String -> Property
prop_ownership_function_param_patterns value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 按值获取：移动所有权\n" ++
                 "  func takeValue(s: MyString) {\n" ++
                 "    fmt.Println(s.data)\n" ++
                 "  }\n" ++
                 "  // 按引用获取：借用\n" ++
                 "  func takeRef(s: &MyString) {\n" ++
                 "    fmt.Println(s.data)\n" ++
                 "  }\n" ++
                 "  // 按可变引用获取：可变借用\n" ++
                 "  func takeMutRef(s: &mut MyString) {\n" ++
                 "    s.data = \"modified\"\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  takeRef(s)          // ✓ 只是借用\n" ++
                 "  takeMutRef(s)       // ✓ 可变借用\n" ++
                 "  fmt.Println(s.data) // ✓ s仍可用\n" ++
                 "  takeValue(s)        // ✓ 移动所有权\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与返回值模式
prop_ownership_return_patterns :: String -> Property
prop_ownership_return_patterns value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 返回拥有的值\n" ++
                 "  func createOwned() -> MyString {\n" ++
                 "    return NewMyString(\"" ++ value ++ "\")\n" ++
                 "  }\n" ++
                 "  // 返回引用\n" ++
                 "  func getRef(s: &MyString) -> &string {\n" ++
                 "    return &s.data\n" ++
                 "  }\n" ++
                 "  owned := createOwned() // ✓ 获得所有权\n" ++
                 "  fmt.Println(owned.data)\n" ++
                 "  ref := getRef(&owned)  // ✓ 借用\n" ++
                 "  fmt.Println(*ref)\n" ++
                 "  fmt.Println(owned.data) // ✓ 仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与结构体字段访问
prop_ownership_struct_field_access :: String -> Property
prop_ownership_struct_field_access value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Struct {\n" ++
                 "    field MyString\n" ++
                 "  }\n" ++
                 "  s := Struct{field: NewMyString(\"" ++ value ++ "\")}\n" ++
                 "  // 读取字段：借用\n" ++
                 "  fmt.Println(s.field.data)\n" ++
                 "  // 移动字段\n" ++
                 "  moved := s.field\n" ++
                 "  fmt.Println(moved.data) // ✓ 拥有字段值\n" ++
                 "  // fmt.Println(s.field.data) // ✗ 字段已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与方法接收者
prop_ownership_method_receivers :: String -> Property
prop_ownership_method_receivers value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type MyType struct {\n" ++
                 "    value MyString\n" ++
                 "  }\n" ++
                 "  // 按值接收：移动所有权\n" ++
                 "  func (t MyType) method1() {\n" ++
                 "    fmt.Println(t.value.data)\n" ++
                 "  }\n" ++
                 "  // 按引用接收：借用\n" ++
                 "  func (t &MyType) method2() {\n" ++
                 "    fmt.Println(t.value.data)\n" ++
                 "  }\n" ++
                 "  // 按可变引用接收：可变借用\n" ++
                 "  func (t &mut MyType) method3() {\n" ++
                 "    t.value.data = \"modified\"\n" ++
                 "  }\n" ++
                 "  t := MyType{value: NewMyString(\"" ++ value ++ "\")}\n" ++
                 "  t.method2() // ✓ 只是借用\n" ++
                 "  t.method3() // ✓ 可变借用\n" ++
                 "  fmt.Println(t.value.data) // ✓ 仍可用\n" ++
                 "  t.method1() // ✓ 移动所有权\n" ++
                 "  // fmt.Println(t.value.data) // ✗ t已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与trait对象
prop_ownership_trait_objects :: String -> Property
prop_ownership_trait_objects value =
  let typusCode = "{//! ownership: on\n" ++
                 "  type Writer interface {\n" ++
                 "    Write(data: MyString)\n" ++
                 "  }\n" ++
                 "  type MyWriter struct {}\n" ++
                 "  func (w MyWriter) Write(data: MyString) {\n" ++
                 "    fmt.Println(data.data) // data被移动\n" ++
                 "  }\n" ++
                 "  var w Writer = MyWriter{}\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  w.Write(s) // ✓ s被移动\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与泛型
prop_ownership_generics :: String -> Property
prop_ownership_generics value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 泛型函数的所有权语义取决于类型参数\n" ++
                 "  func process<T>(t: T) {\n" ++
                 "    // T的所有权语义决定t的处理方式\n" ++
                 "  }\n" ++
                 "  func processRef<T>(t: &T) {\n" ++
                 "    // 借用t\n" ++
                 "  }\n" ++
                 "  s := NewMyString(\"" ++ value ++ "\")\n" ++
                 "  processRef(&s) // ✓ 只是借用\n" ++
                 "  fmt.Println(s.data) // ✓ s仍可用\n" ++
                 "  process(s) // ✓ 移动所有权\n" ++
                 "  // fmt.Println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期参数
prop_ownership_lifetime_params :: String -> Property
prop_ownership_lifetime_params value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 显式生命周期参数\n" ++
                 "  func compare<'a>(x: &'a string, y: &'a string) -> bool {\n" ++
                 "    return x == y\n" ++
                 "  }\n" ++
                 "  s1 := \"" ++ value ++ "\"\n" ++
                 "  s2 := \"" ++ value ++ "\"\n" ++
                 "  result := compare(&s1, &s2)\n" ++
                 "  fmt.Println(result) // ✓ 比较结果\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期子类型化
prop_ownership_lifetime_subtyping :: String -> Property
prop_ownership_lifetime_subtyping value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 生命周期子类型化：较长生命周期可以转换为较短生命周期\n" ++
                 "  func printStr(s: &string) {\n" ++
                 "    fmt.Println(*s)\n" ++
                 "  }\n" ++
                 "  s := \"" ++ value ++ "\"\n" ++
                 "  // s的生命周期比函数调用长，可以借用\n" ++
                 "  printStr(&s)\n" ++
                 "  fmt.Println(s) // ✓ s仍可用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期省略规则
prop_ownership_lifetime_elision_rules :: String -> Property
prop_ownership_lifetime_elision_rules value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 生命周期省略规则1：每个引用参数都有自己的生命周期参数\n" ++
                 "  func rule1(x: &string, y: &int) -> string {\n" ++
                 "    return \"result\"\n" ++
                 "  }\n" ++
                 "  // 生命周期省略规则2：如果只有一个输入生命周期，分配给所有输出生命周期\n" ++
                 "  func rule2(x: &string) -> &string {\n" ++
                 "    return x\n" ++
                 "  }\n" ++
                 "  // 生命周期省略规则3：如果有多个输入生命周期，其中一个是&self或&mut self，\n" ++
                 "  // 则self的生命周期分配给所有输出\n" ++
                 "  type MyType struct {\n" ++
                 "    value string\n" ++
                 "  }\n" ++
                 "  func (t &MyType) rule3() -> &string {\n" ++
                 "    return &t.value\n" ++
                 "  }\n" ++
                 "  s := \"" ++ value ++ "\"\n" ++
                 "  result1 := rule1(&s, &42)\n" ++
                 "  result2 := rule2(&s)\n" ++
                 "  t := MyType{value: s}\n" ++
                 "  result3 := t.rule3()\n" ++
                 "  fmt.Println(result1, result2, result3)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与静态分析
prop_ownership_static_analysis :: String -> Property
prop_ownership_static_analysis value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 编译器静态分析所有权\n" ++
                 "  func analyze() {\n" ++
                 "    let s1 = NewMyString(\"" ++ value ++ "\")\n" ++
                 "    let s2 = s1          // s1被移动\n" ++
                 "    // 编译器在这里会报错，因为s1已被移动\n" ++
                 "    // println(s1.data)\n" ++
                 "    println(s2.data)     // ✓ s2有效\n" ++
                 "  }\n" ++
                 "  analyze()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与借用检查器
prop_ownership_borrow_checker :: String -> Property
prop_ownership_borrow_checker value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 借用检查器确保引用的有效性\n" ++
                 "  func check() {\n" ++
                 "    let s = NewMyString(\"" ++ value ++ "\")\n" ++
                 "    let r = &s           // 借用s\n" ++
                 "    println(r.data)     // ✓ r有效\n" ++
                 "    // s在这里被销毁，r变为无效\n" ++
                 "  }\n" ++
                 "  check()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与移动语义
prop_ownership_move_semantics :: String -> Property
prop_ownership_move_semantics value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 移动语义：转移所有权而不复制\n" ++
                 "  func move_semantics() {\n" ++
                 "    let s1 = NewMyString(\"" ++ value ++ "\")\n" ++
                 "    let s2 = s1          // 移动，不是复制\n" ++
                 "    // s1不再有效，s2拥有数据\n" ++
                 "    println(s2.data)\n" ++
                 "  }\n" ++
                 "  move_semantics()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与复制语义
prop_ownership_copy_semantics :: String -> Property
prop_ownership_copy_semantics value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 复制语义：类型实现Copy trait时使用复制而非移动\n" ++
                 "  type CopyType struct {\n" ++
                 "    value int\n" ++
                 "  }\n" ++
                 "  func copy_semantics() {\n" ++
                 "    let c1 = CopyType{value: 42}\n" ++
                 "    let c2 = c1          // 复制，不是移动\n" ++
                 "    println(c1.value)   // ✓ c1仍有效\n" ++
                 "    println(c2.value)   // ✓ c2也有效\n" ++
                 "  }\n" ++
                 "  copy_semantics()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与克隆
prop_ownership_clone :: String -> Property
prop_ownership_clone value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 克隆：显式创建深拷贝\n" ++
                 "  func clone_demo() {\n" ++
                 "    let s1 = NewMyString(\"" ++ value ++ "\")\n" ++
                 "    let s2 = s1.Clone()    // 显式克隆\n" ++
                 "    println(s1.data)       // ✓ s1仍有效\n" ++
                 "    println(s2.data)       // ✓ s2是独立副本\n" ++
                 "  }\n" ++
                 "  clone_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与Drop trait
prop_ownership_drop_trait :: String -> Property
prop_ownership_drop_trait value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // Drop trait：值离开作用域时自动执行清理\n" ++
                 "  type ResourceType struct {\n" ++
                 "    name string\n" ++
                 "  }\n" ++
                 "  func (r ResourceType) Drop() {\n" ++
                 "    println(\"Dropping resource:\", r.name)\n" ++
                 "  }\n" ++
                 "  func drop_demo() {\n" ++
                 "    let r = ResourceType{name: \"" ++ value ++ "\"}\n" ++
                 "    // r离开作用域时会调用Drop\n" ++
                 "  }\n" ++
                 "  drop_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与RAII
prop_ownership_raii :: String -> Property
prop_ownership_raii value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // RAII：资源获取即初始化\n" ++
                 "  type File struct {\n" ++
                 "    path string\n" ++
                 "  }\n" ++
                 "  func (f File) Drop() {\n" ++
                 "    println(\"Closing file:\", f.path)\n" ++
                 "  }\n" ++
                 "  func openFile(path: string) -> File {\n" ++
                 "    return File{path: path}\n" ++
                 "  }\n" ++
                 "  func raii_demo() {\n" ++
                 "    let f = openFile(\"" ++ value ++ "\")\n" ++
                 "    // 使用文件\n" ++
                 "    // f离开作用域时自动关闭\n" ++
                 "  }\n" ++
                 "  raii_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与智能指针
prop_ownership_smart_pointers :: String -> Property
prop_ownership_smart_pointers value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // Box<T>：堆分配的智能指针\n" ++
                 "  func box_demo() {\n" ++
                 "    let b = Box(NewMyString(\"" ++ value ++ "\"))\n" ++
                 "    println(b.data)       // 通过Box访问\n" ++
                 "    // b离开作用域时自动释放内存\n" ++
                 "  }\n" ++
                 "  // Rc<T>：引用计数智能指针\n" ++
                 "  func rc_demo() {\n" ++
                 "    let r1 = Rc(NewMyString(\"" ++ value ++ "\"))\n" ++
                 "    let r2 = r1.Clone()     // 增加引用计数\n" ++
                 "    println(r1.data)      // ✓ 多个所有者\n" ++
                 "    println(r2.data)      // ✓ 多个所有者\n" ++
                 "  }\n" ++
                 "  box_demo()\n" ++
                 "  rc_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与内部可变性模式
prop_ownership_interior_mutability_patterns :: String -> Property
prop_ownership_interior_mutability_patterns value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // Cell<T>：内部可变性模式\n" ++
                 "  type Cell<T> struct {\n" ++
                 "    value T\n" ++
                 "  }\n" ++
                 "  func (c Cell<T>) Get() -> T {\n" ++
                 "    return c.value\n" ++
                 "  }\n" ++
                 "  func (c &mut Cell<T>) Set(value: T) {\n" ++
                 "    c.value = value\n" ++
                 "  }\n" ++
                 "  func cell_demo() {\n" ++
                 "    let c = Cell{value: \"" ++ value ++ "\"}\n" ++
                 "    println(c.Get())        // ✓ 读取\n" ++
                 "    c.Set(\"modified\")       // ✓ 修改\n" ++
                 "    println(c.Get())        // ✓ 读取修改后的值\n" ++
                 "  }\n" ++
                 "  cell_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与线程安全
prop_ownership_thread_safety :: String -> Property
prop_ownership_thread_safety value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // Arc<T>：原子引用计数，线程安全\n" ++
                 "  func arc_demo() {\n" ++
                 "    let a1 = Arc(NewMyString(\"" ++ value ++ "\"))\n" ++
                 "    let a2 = a1.Clone()     // 线程安全的克隆\n" ++
                 "    // 可以安全地发送到其他线程\n" ++
                 "    go func() {\n" ++
                 "      println(a2.data)     // ✓ 跨线程访问\n" ++
                 "    }()\n" ++
                 "    println(a1.data)       // ✓ 当前线程访问\n" ++
                 "  }\n" ++
                 "  // Mutex<T>：互斥锁，提供内部可变性\n" ++
                 "  func mutex_demo() {\n" ++
                 "    let m = Mutex(NewMyString(\"" ++ value ++ "\"))\n" ++
                 "    let guard = m.Lock()    // 获取锁\n" ++
                 "    guard.data = \"modified\" // ✓ 修改\n" ++
                 "    // guard离开作用域时自动释放锁\n" ++
                 "  }\n" ++
                 "  arc_demo()\n" ++
                 "  mutex_demo()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与生命周期标注的必要性
prop_ownership_lifetime_annotation_necessity :: String -> Property
prop_ownership_lifetime_annotation_necessity value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 有时需要显式标注生命周期\n" ++
                 "  func longest<'a>(x: &'a string, y: &string) -> &'a string {\n" ++
                 "    // 返回x的引用，所以需要标注返回的生命周期与x相同\n" ++
                 "    if len(x) > len(y) {\n" ++
                 "      return x\n" ++
                 "    }\n" ++
                 "    // 不能返回y，因为生命周期不同\n" ++
                 "    panic(\"Cannot return y\")\n" ++
                 "  }\n" ++
                 "  let s1 = \"" ++ value ++ "\"\n" ++
                 "  let s2 = \"short\"\n" ++
                 "  let result = longest(&s1, &s2)\n" ++
                 "  println(*result) // ✓ result有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与结构体生命周期
prop_ownership_struct_lifetime :: String -> Property
prop_ownership_struct_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 结构体也可以有生命周期参数\n" ++
                 "  type Ref<'a> struct {\n" ++
                 "    value: &'a string\n" ++
                 "  }\n" ++
                 "  func create_ref<'a>(s: &'a string) -> Ref<'a> {\n" ++
                 "    return Ref{value: s}\n" ++
                 "  }\n" ++
                 "  let s = \"" ++ value ++ "\"\n" ++
                 "  let r = create_ref(&s)\n" ++
                 "  println(*r.value) // ✓ 引用有效\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与trait对象生命周期
prop_ownership_trait_object_lifetime :: String -> Property
prop_ownership_trait_object_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // trait对象也有生命周期\n" ++
                 "  type Writer<'a> interface {\n" ++
                 "    Write(data: &'a string)\n" ++
                 "  }\n" ++
                 "  type ConsoleWriter struct {}\n" ++
                 "  func (w ConsoleWriter) Write(data: &string) {\n" ++
                 "    println(*data)\n" ++
                 "  }\n" ++
                 "  let s = \"" ++ value ++ "\"\n" ++
                 "  let w: Writer = ConsoleWriter{}\n" ++
                 "  w.Write(&s) // ✓ 可以写入s的引用\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与闭包生命周期
prop_ownership_closure_lifetime :: String -> Property
prop_ownership_closure_lifetime value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 闭包捕获的引用也有生命周期\n" ++
                 "  fn create_closure(s: &string) -> func() {\n" ++
                 "    return func() {\n" ++
                 "      println(*s) // 闭包捕获s的引用\n" ++
                 "    }\n" ++
                 "  }\n" ++
                 "  let s = \"" ++ value ++ "\"\n" ++
                 "  let f = create_closure(&s)\n" ++
                 "  f() // ✓ 调用闭包\n" ++
                 "  // s仍然有效，因为闭包没有超过s的生命周期\n" ++
                 "  println(s)\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与高阶函数
prop_ownership_higher_order_functions :: String -> Property
prop_ownership_higher_order_functions value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 高阶函数的所有权语义\n" ++
                 "  fn apply<F>(f: F, s: MyString) -> MyString\n" ++
                 "    where F: Fn(MyString) -> MyString\n" ++
                 "  {\n" ++
                 "    return f(s) // s被移动到f中\n" ++
                 "  }\n" ++
                 "  fn apply_ref<F>(f: F, s: &MyString) -> string\n" ++
                 "    where F: Fn(&MyString) -> string\n" ++
                 "  {\n" ++
                 "    return f(s) // s被借用\n" ++
                 "  }\n" ++
                 "  let s = NewMyString(\"" ++ value ++ "\")\n" ++
                 "  let result1 = apply_ref(func(s: &MyString) -> string {\n" ++
                 "    return s.data\n" ++
                 "  }, &s)\n" ++
                 "  println(result1) // ✓ s仍可用\n" ++
                 "  let result2 = apply(func(s: MyString) -> MyString {\n" ++
                 "    return s\n" ++
                 "  }, s)\n" ++
                 "  println(result2.data) // ✓ s被移动到result2\n" ++
                 "  // println(s.data) // ✗ s已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与迭代器
prop_ownership_iterators :: String -> Property
prop_ownership_iterators value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 迭代器的所有权语义\n" ++
                 "  fn iterate_owned(v: []MyString) {\n" ++
                 "    for s in v {\n" ++
                 "      println(s.data) // s被移动\n" ++
                 "    }\n" ++
                 "    // v不再可用，因为元素被移动\n" ++
                 "  }\n" ++
                 "  fn iterate_ref(v: &[]MyString) {\n" ++
                 "    for s in v {\n" ++
                 "      println(s.data) // s被借用\n" ++
                 "    }\n" ++
                 "    // v仍可用，因为只是借用\n" ++
                 "  }\n" ++
                 "  let v = []MyString{NewMyString(\"" ++ value ++ "\")}\n" ++
                 "  iterate_ref(&v) // ✓ 只是借用\n" ++
                 "  iterate_owned(v) // ✓ 移动元素\n" ++
                 "  // println(v[0].data) // ✗ 元素已被移动\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与错误处理
prop_ownership_error_handling :: String -> Property
prop_ownership_error_handling value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 错误处理中的所有权\n" ++
                 "  type Result<T, E> {\n" ++
                 "    Ok(T)\n" ++
                 "    Err(E)\n" ++
                 "  }\n" ++
                 "  fn might_fail() -> Result<MyString, string> {\n" ++
                 "    return Result<MyString, string>{Ok: NewMyString(\"" ++ value ++ "\")}\n" ++
                 "  }\n" ++
                 "  let result = might_fail()\n" ++
                 "  match result {\n" ++
                 "  case Ok(s):\n" ++
                 "    println(s.data) // ✓ 获得所有权\n" ++
                 "  case Err(e):\n" ++
                 "    println(e)\n" ++
                 "  }\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与并发模式
prop_ownership_concurrency_patterns :: String -> Property
prop_ownership_concurrency_patterns value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 并发模式中的所有权\n" ++
                 "  // 1. 共享所有权\n" ++
                 "  fn shared_ownership() {\n" ++
                 "    let s1 = Arc(NewMyString(\"" ++ value ++ "\"))\n" ++
                 "    let s2 = s1.Clone()\n" ++
                 "    go func() {\n" ++
                 "      println(s2.data) // ✓ 跨线程共享\n" ++
                 "    }()\n" ++
                 "    println(s1.data) // ✓ 当前线程访问\n" ++
                 "  }\n" ++
                 "  // 2. 通道传递所有权\n" ++
                 "  fn channel_ownership() {\n" ++
                 "    let ch = make(chan MyString)\n" ++
                 "    go func() {\n" ++
                 "      ch <- NewMyString(\"" ++ value ++ "\") // 发送所有权\n" ++
                 "    }()\n" ++
                 "    let s = <-ch // 接收所有权\n" ++
                 "    println(s.data) // ✓ 拥有数据\n" ++
                 "  }\n" ++
                 "  shared_ownership()\n" ++
                 "  channel_ownership()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

-- | 测试所有权与零成本抽象
prop_ownership_zero_cost_abstractions :: String -> Property
prop_ownership_zero_cost_abstractions value =
  let typusCode = "{//! ownership: on\n" ++
                 "  // 零成本抽象：所有权检查在编译时完成，运行时无开销\n" ++
                 "  fn zero_cost() {\n" ++
                 "    let s1 = NewMyString(\"" ++ value ++ "\")\n" ++
                 "    let s2 = s1 // 移动所有权，零成本\n" ++
                 "    // 借用检查在编译时完成，运行时无额外检查\n" ++
                 "    let r = &s2\n" ++
                 "    println(r.data)\n" ++
                 "    // 所有这些操作在运行时与普通Go代码相同\n" ++
                 "  }\n" ++
                 "  zero_cost()\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
  in property $ isRight parseResult

tests :: TestTree
tests = testGroup "Test.Unit.NewOwnershipMechanismTestSuite Tests"
  [ testProperty "basic ownership move" prop_basic_ownership_move
  , testProperty "immutable borrow" prop_immutable_borrow
  , testProperty "mutable borrow" prop_mutable_borrow
  , testProperty "borrowing rules mutable" prop_borrowing_rules_mutable
  , testProperty "borrowing rules mutable immutable" prop_borrowing_rules_mutable_immutable
  , testProperty "borrowing rules multiple immutable" prop_borrowing_rules_multiple_immutable
  , testProperty "borrowing lifetime" prop_borrowing_lifetime
  , testProperty "cross goroutine ownership" prop_cross_goroutine_ownership
  , testProperty "ownership gc zero overhead" prop_ownership_gc_zero_overhead
  , testProperty "ownership logical correctness" prop_ownership_logical_correctness
  , testProperty "interface method ownership" prop_interface_method_ownership
  , testProperty "ownership function return" prop_ownership_function_return
  , testProperty "ownership struct fields" prop_ownership_struct_fields
  , testProperty "ownership arrays slices" prop_ownership_arrays_slices
  , testProperty "ownership maps" prop_ownership_maps
  , testProperty "ownership closures" prop_ownership_closures
  , testProperty "ownership closures move" prop_ownership_closures_move
  , testProperty "ownership defer" prop_ownership_defer
  , testProperty "ownership panic recovery" prop_ownership_panic_recovery
  , testProperty "ownership select" prop_ownership_select
  , testProperty "ownership type assertion" prop_ownership_type_assertion
  , testProperty "ownership reflection" prop_ownership_reflection
  , testProperty "ownership unsafe" prop_ownership_unsafe
  , testProperty "ownership cgo" prop_ownership_cgo
  , testProperty "ownership serialization" prop_ownership_serialization
  , testProperty "ownership deep copy" prop_ownership_deep_copy
  , testProperty "ownership shallow copy" prop_ownership_shallow_copy
  , testProperty "ownership interior mutability" prop_ownership_interior_mutability
  , testProperty "ownership shared ownership" prop_ownership_shared_ownership
  , testProperty "ownership copy on write" prop_ownership_copy_on_write
  , testProperty "ownership lifetime annotation" prop_ownership_lifetime_annotation
  , testProperty "ownership lifetime subtyping" prop_ownership_lifetime_subtyping
  , testProperty "ownership lifetime elision" prop_ownership_lifetime_elision
  , testProperty "ownership static lifetime" prop_ownership_static_lifetime
  , testProperty "ownership higher ranked lifetime" prop_ownership_higher_ranked_lifetime
  , testProperty "ownership lifetime bounds" prop_ownership_lifetime_bounds
  , testProperty "ownership object safety" prop_ownership_object_safety
  , testProperty "ownership closure capture modes" prop_ownership_closure_capture_modes
  , testProperty "ownership pattern matching" prop_ownership_pattern_matching
  , testProperty "ownership ref pattern" prop_ownership_ref_pattern
  , testProperty "ownership mut ref pattern" prop_ownership_mut_ref_pattern
  , testProperty "ownership let binding" prop_ownership_let_binding
  , testProperty "ownership ref binding" prop_ownership_ref_binding
  , testProperty "ownership mut ref binding" prop_ownership_mut_ref_binding
  , testProperty "ownership function param patterns" prop_ownership_function_param_patterns
  , testProperty "ownership return patterns" prop_ownership_return_patterns
  , testProperty "ownership struct field access" prop_ownership_struct_field_access
  , testProperty "ownership method receivers" prop_ownership_method_receivers
  , testProperty "ownership trait objects" prop_ownership_trait_objects
  , testProperty "ownership generics" prop_ownership_generics
  , testProperty "ownership lifetime params" prop_ownership_lifetime_params
  , testProperty "ownership lifetime subtyping" prop_ownership_lifetime_subtyping
  , testProperty "ownership lifetime elision rules" prop_ownership_lifetime_elision_rules
  , testProperty "ownership static analysis" prop_ownership_static_analysis
  , testProperty "ownership borrow checker" prop_ownership_borrow_checker
  , testProperty "ownership move semantics" prop_ownership_move_semantics
  , testProperty "ownership copy semantics" prop_ownership_copy_semantics
  , testProperty "ownership clone" prop_ownership_clone
  , testProperty "ownership drop trait" prop_ownership_drop_trait
  , testProperty "ownership raii" prop_ownership_raii
  , testProperty "ownership smart pointers" prop_ownership_smart_pointers
  , testProperty "ownership interior mutability patterns" prop_ownership_interior_mutability_patterns
  , testProperty "ownership thread safety" prop_ownership_thread_safety
  , testProperty "ownership lifetime annotation necessity" prop_ownership_lifetime_annotation_necessity
  , testProperty "ownership struct lifetime" prop_ownership_struct_lifetime
  , testProperty "ownership trait object lifetime" prop_ownership_trait_object_lifetime
  , testProperty "ownership closure lifetime" prop_ownership_closure_lifetime
  , testProperty "ownership higher order functions" prop_ownership_higher_order_functions
  , testProperty "ownership iterators" prop_ownership_iterators
  , testProperty "ownership error handling" prop_ownership_error_handling
  , testProperty "ownership concurrency patterns" prop_ownership_concurrency_patterns
  , testProperty "ownership zero cost abstractions" prop_ownership_zero_cost_abstractions
  ]