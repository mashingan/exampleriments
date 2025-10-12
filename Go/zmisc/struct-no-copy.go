package main

// prevent struct copies, ensure run `go vet` to have it checked
// ref: https://rednafi.com/go/prevent-struct-copies/

type hmm struct{ _ nocop }

type nocop struct{}

func (*nocop) Lock()   {}
func (*nocop) Unlock() {}

func main() {
	var hyme hmm
	_ = hyme
}
