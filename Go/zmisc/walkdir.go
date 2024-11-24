package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

type action func(arg temparg)
type temparg struct {
	info  os.FileInfo
	depth int
}

func helper(path string, currdepth, maxdepth int, fn action) {
	if currdepth >= maxdepth {
		return
	}
	f, err := ioutil.ReadDir(path)
	if err != nil {
		log.Println(err)
		return
	}
	for _, fdir := range f {
		fn(temparg{info: fdir, depth: currdepth})
		if !fdir.IsDir() {
			continue
		}
		helper(fdir.Name(), currdepth+1, maxdepth, fn)
	}
}

func walkpath(path string, maxdepth int, fn action) {
	helper(path, 0, maxdepth, fn)
}

func main() {
	walkpath(".", 2, func(f temparg) {
		fmt.Printf("%s:%s\n", strings.Repeat("==", f.depth), f.info.Name())
	})
	acc := make([]string, 0)
	walkpath(".", 2, func(f temparg) {
		acc = append(acc, f.info.Name())
	})
	fmt.Println("acc:", acc)
}
