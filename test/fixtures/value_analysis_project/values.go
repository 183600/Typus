package main

var (
    numbersList = []int{1, 2, 3}
    lookup = map[string]int{
        "a": 1,
    }
    groupedValue, groupedRef = 5, make([]byte, 0)
)

type MyStruct struct {
    ID int
}

type MyAlias = MyStruct

type MyNumber int

type (
    ExportedGrouped struct {
        Name string
    }
    ExportedAlias = ExportedGrouped
)

func example(items []string) {
    // fake := make([]int, 0)
    value := 42
    text := "make(inside)"
    numbers := make([]int,
        0)
    first, second := 1, make([]int, 1)
    for _, entry := range items {
        inner := make([]string, 0)
        _ = entry
        _ = inner
    }
    groupedValue = value
    _ = numbers
    _ = first
    _ = second
}

func custom() {
    customStruct := MyStruct{ID: 1}
    customAlias := MyAlias{ID: 2}
    groupStruct := ExportedGrouped{Name: "ok"}
    groupAlias := ExportedAlias{Name: "alias"}
    customPointer := &MyStruct{ID: 3}
    _ = []interface{}{
        numbersList,
        lookup,
        groupedValue,
        groupedRef,
        customStruct,
        customAlias,
        groupStruct,
        groupAlias,
        customPointer,
    }
}
