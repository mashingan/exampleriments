// ref:
// https://t.me/golangID/78604
// https://play.golang.org/p/uLSZ41f86P7
package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/mashingan/smapping"
)

type mockEnvStruct struct {
	Field1 string `env:"key1"`
	Field2 string `env:"key2"`
	Field3 string `env:"key3"`
	Field4 string `env:"key4"`
	Field5 string `env:"key5"`
}

func setEnv() {
	for i := 1; i <= 5; i++ {
		_ = os.Setenv("key"+strconv.Itoa(i), "val"+strconv.Itoa(i))
	}
}

func discardEnv() {
	for i := 1; i <= 5; i++ {
		_ = os.Unsetenv("key" + strconv.Itoa(i))
	}
}

func main() {
	setEnv()
	maps := smapping.Mapped{}
	for _, kv := range os.Environ() {
		strs := strings.Split(kv, "=")
		maps[strs[0]] = strs[1]
	}
	currenv := mockEnvStruct{}
	err := smapping.FillStructByTags(&currenv, maps, "env")
	if err != nil {
		fmt.Println("mapping err:", err)
	}
	fmt.Println(currenv)
	fmt.Println(currenv.Field1 == "val1")
	fmt.Println(currenv.Field2 == "val2")
	fmt.Println(currenv.Field3 == "val3")
	fmt.Println(currenv.Field4 == "val4")
	fmt.Println(currenv.Field5 == "val5")
	discardEnv()
}
