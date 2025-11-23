package main

type Buffer struct {
    ID int
}

func build() {
    first, second := map[string]int{"a": 1}, Buffer{
        ID: 1,
    }
    alias := Buffer{
        ID: 2,
    }
    trailing := make([]int,
        2,
    )
    pointer := new(Buffer)
    fromMake := make([]byte, 0)
    _ = first
    _ = second
    _ = alias
    _ = trailing
    _ = pointer
    _ = fromMake
}
