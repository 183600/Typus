package main

func guard(resources []string) {
    if immediate := "value"; immediate != "" {
        pointer := &resources
        println(immediate, pointer)
    }
    switch alias := []int{1, 2}; alias[0] {
    case 1:
        ref := make([]byte, 0)
        _ = ref
    }
}
