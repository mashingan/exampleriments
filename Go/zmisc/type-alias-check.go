package main

import "fmt"

type Alias1 []string
type Alias2 []string

type Aeq1 = []string
type Aeq2 = []string

func main() {
	a1 := Alias1{}
	a2 := Alias2{}
	fmt.Printf("Is type Alias1 (%T) same with type Alias2 (%T)? %t\n",
		a1, a2,
		fmt.Sprintf("%T", a1) == fmt.Sprintf("%T", a2))
	aq1 := Aeq1{}
	aq2 := Aeq2{}
	fmt.Printf("Is type Aeq1 (%T) same with type Aeq2 (%T)? %t",
		aq1, aq2,
		fmt.Sprintf("%T", aq1) == fmt.Sprintf("%T", aq2))
}
