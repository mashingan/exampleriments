package main

import (
	"log"

	"fyne.io/fyne/v2/data/binding"
)

func main() {
	boundString := binding.NewString()
	s, _ := boundString.Get()
	log.Printf("Bound = '%s'\n", s)

	myint := 5
	boundInt := binding.BindInt(&myint)
	i, _ := boundInt.Get()
	log.Printf("Source = %d, Bound = %d\n", myint, i)
}
