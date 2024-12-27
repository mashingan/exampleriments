package gastar

/*
Hasher is the generic constraint to be implemented
for hashable key map.
For simple object struct, it can satiesfies comparable constraint
but for more complex object or any custom object, we need to be
able to define our own hash key.
Consider this example:

	type Incomparable struct {
			Field1 int 		// comparable
			Field2 string	// comparable
			field3 []int 	// not comparable
	}

let's say we only need to compare Field1 and Field2 only with field3
will be ignored, we need a way to define that each Incomparable
object only equal to Field1 and Field2 only. Or in simple word,
hashable, so we need to provide method Hash for Incomparable
so it can be used instead of the raw object itself.
*/
type Hasher[K comparable] interface {
	Hash() K
}

// HashMap is the custom map that will record the key (together)
// with its incomparable fields and values of the map itself.
type HashMap[K Hasher[H], V any, H comparable] struct {
	keymap map[H]K
	keyval map[H]V
}

// NewHashMap initiates the internal underlying map.
func NewHashMap[K Hasher[H], V any, H comparable]() HashMap[K, V, H] {
	hm := HashMap[K, V, H]{}
	hm.keymap = make(map[H]K)
	hm.keyval = make(map[H]V)
	return hm
}

func (cm HashMap[K, V, H]) Set(key K, value V) {
	cm.keymap[key.Hash()] = key
	cm.keyval[key.Hash()] = value
}

func (cm HashMap[K, V, T]) Get(key Hasher[T]) (V, bool) {
	value, exists := cm.keyval[key.Hash()]
	return value, exists
}

func (cm HashMap[K, V, T]) Delete(key Hasher[T]) {
	delete(cm.keyval, key.Hash())
	delete(cm.keymap, key.Hash())
}

/// Disabled for now: due to using minimum of go 1.23
// All returning all keys and values pair to be used in
// for-range syntax.
// func (cm HashMap[K, V, H]) All() iter.Seq2[K, V] {
// 	return func(yield func(k K, v V) bool) {
// 		for k, k1 := range cm.keymap {
// 			v, _ := cm.keyval[k]
// 			if !yield(k1, v) {
// 				return
// 			}
// 		}
// 	}
// }
