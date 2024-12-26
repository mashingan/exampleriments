package main

import (
	"fmt"
	"iter"
)

// Define the generic interface
type KeyInterface[K comparable] interface {
	Key() K
}

// Define a struct that implements the generic interface
type CustomKey struct {
	ID      int
	Name    string
	notUsed []byte
}

// Implement the Key method for CustomKey
func (ck CustomKey) Key() string {
	return fmt.Sprintf("%d-%s", ck.ID, ck.Name)
}

// Define another struct that uses an integer key
type IntKey struct {
	ID int
}

// Implement the Key method for IntKey
func (ik IntKey) Key() int {
	return ik.ID
}

// Define a custom map type using generics
type CustomMap[K comparable, V any, T KeyInterface[K]] map[K]V

// Add methods for the custom map
func (cm CustomMap[K, V, T]) Set(key T, value V) {
	cm[key.Key()] = value
}

func (cm CustomMap[K, V, T]) Get(key T) (V, bool) {
	value, exists := cm[key.Key()]
	return value, exists
}

func (cm CustomMap[K, V, T]) Delete(key T) {
	delete(cm, key.Key())
}

type Custom2[K KeyInterface[H], V any, H comparable] struct {
	keymap map[H]K
	keyval map[H]V
}

func newCustom2[K KeyInterface[H], V any, H comparable]() Custom2[K, V, H] {
	mc := Custom2[K, V, H]{}
	mc.keymap = make(map[H]K)
	mc.keyval = make(map[H]V)
	return mc
}

func (cm Custom2[K, V, H]) Set(key K, value V) {
	cm.keymap[key.Key()] = key
	cm.keyval[key.Key()] = value
}

func (cm Custom2[K, V, T]) Get(key KeyInterface[T]) (V, bool) {
	value, exists := cm.keyval[key.Key()]
	return value, exists
}

func (cm Custom2[K, V, T]) Delete(key KeyInterface[T]) {
	delete(cm.keyval, key.Key())
	delete(cm.keymap, key.Key())
}

func (cm Custom2[K, V, H]) All() iter.Seq2[K, V] {
	return func(yield func(k K, v V) bool) {
		for k, k1 := range cm.keymap {
			v, _ := cm.keyval[k]
			if !yield(k1, v) {
				return
			}
		}
	}
}

func main() {
	// Example 1: Using CustomKey with string-based keys
	key1 := CustomKey{ID: 1, Name: "Alice"}
	key2 := CustomKey{ID: 2, Name: "Bob"}

	myMap := CustomMap[string, string, CustomKey]{}

	myMap.Set(key1, "Engineer")
	myMap.Set(key2, "Designer")

	if value, exists := myMap.Get(key1); exists {
		fmt.Println("Key1:", value)
	}

	if value, exists := myMap.Get(key2); exists {
		fmt.Println("Key2:", value)
	}

	// Example 2: Using IntKey with integer-based keys
	intKey1 := IntKey{ID: 101}
	intKey2 := IntKey{ID: 102}

	intMap := CustomMap[int, string, IntKey]{}

	intMap.Set(intKey1, "Manager")
	intMap.Set(intKey2, "Developer")

	if value, exists := intMap.Get(intKey1); exists {
		fmt.Println("IntKey1:", value)
	}

	if value, exists := intMap.Get(intKey2); exists {
		fmt.Println("IntKey2:", value)
	}

	myMap2 := newCustom2[CustomKey, string, string]()

	myMap2.Set(key1, "Engineer")
	myMap2.Set(key2, "Designer")
	for k, v := range myMap2.All() {
		fmt.Printf("Custom 2, key: %#v, val: %#v\n", k, v)
	}
}
