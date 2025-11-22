package domain

type Repository[T any] struct {
    items []T
}
