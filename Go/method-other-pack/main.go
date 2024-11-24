package main

import (
	"fmt"

	ad "methodpack/addmeth"
	m "methodpack/modelpack"
)

func main() {
	fmt.Println("vim-go")
	/*
		m := &m.MPack{
			field1: 10,
		}
	*/
	mm := m.New(10)
	(*ad.Aliaspack)(mm).ModifyField1(15)
	//mm.(*ad.Apack).ModifyField1(15)
	fmt.Println(*mm)
}
