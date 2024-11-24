package addmeth

import (
	"fmt"
	"reflect"

	m "methodpack/modelpack"
)

type Aliaspack m.MPack

func (mpack *Aliaspack) ModifyField1(val int) {
	ptrVal := reflect.ValueOf((*m.MPack)(mpack))
	rval := reflect.Indirect(ptrVal)
	field1 := rval.FieldByName("field1")
	//member.Set(reflect.ValueOf(val))
	//member.Addr().Set(reflect.ValueOf(val))
	fmt.Println("is field1 assignable:", field1.CanSet())
	fmt.Println(field1)
	fieldaddr := reflect.ValueOf(val)
	fmt.Println(reflect.Indirect(fieldaddr))
}
