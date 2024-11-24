package main

import (
	"log"
	"reflect"
	"strconv"
)

var fromredis = []string{"1", "one", "two", "2", "not-used"}

type strstruct struct {
	FirstElem  int
	SecondElem string
	ThirdElem  string
	FourthElem int
}

func min(nums ...int) int {
	if len(nums) == 0 {
		return 0
	}
	currmin := nums[0]
	for _, n := range nums[1:] {
		if n < currmin {
			currmin = n
		}
	}
	return currmin
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	vstru := strstruct{}
	vvalin := reflect.ValueOf(&vstru).Elem()
	vtype := vvalin.Type()
	for i := 0; i < min(vvalin.NumField(), len(fromredis)); i++ {
		field := vtype.Field(i)
		if field.PkgPath != "" {
			continue
		}
		str := fromredis[i]
		vvfield := vvalin.FieldByName(field.Name)
		if !vvfield.CanSet() {
			log.Printf("field %s cannot be set\n", field.Name)
			continue
		}
		switch field.Type.Kind() {
		case reflect.Int:
			num, err := strconv.Atoi(str)
			if err != nil {
				log.Println("int conv err:", err)
				continue
			}
			vvfield.SetInt(int64(num))
		case reflect.String:
			vvfield.SetString(str)
		}
	}
	log.Println("vstru:", vstru)
}
