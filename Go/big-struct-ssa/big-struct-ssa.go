// build with:
// go build -o a.exe big-struct-arg-pass.go
// dump the info with
// go tool objdump -s main.main a.exe

package main

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"os"

	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"
)

const mtpack = `
package main

type Bigs struct {
	Field1  int
	Field2  string
	Field3  float32
	Field4  float64
	Bytes   []byte
	PtrStr  *string
	Field12 int
	Field22 string
	Field32 float32
	Field42 float64
	Bytes2  []byte
	PtrStr2 *string
	Field13 int
	Field23 string
	Field33 float32
	Field43 float64
	Bytes3  []byte
	PtrStr3 *string
	//When time.Time
}

func Donothing(b Bigs) {
	for i := 0; i < 10; i++ {
		b.Field1 += i
	}
}

func PtrDonothing(b *Bigs) {
	for i := 0; i < 10; i++ {
		b.Field1 += i
	}
}

func main() {
	s := "hello"
	b := Bigs{
		Field1:  1,
		Field2:  "wan",
		Field3:  1.1,
		Field4:  1.2,
		Bytes:   []byte("wanwan"),
		PtrStr:  &s,
		Field12: 1,
		Field22: "wan",
		Field32: 1.1,
		Field42: 1.2,
		Bytes2:  []byte("wanwan"),
		PtrStr2: &s,
		Field13: 1,
		Field23: "wan",
		Field33: 1.1,
		Field43: 1.2,
		Bytes3:  []byte("wanwan"),
		PtrStr3: &s,
		//When: time.Now(),
	}
	Donothing(b)
	PtrDonothing(&b)
}`

func main() {
	// Replace interface{} with any for this test.
	//ssa.SetNormalizeAnyForTesting(true)
	//defer ssa.SetNormalizeAnyForTesting(false)
	// Parse the source files.
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "bigstruct.go", mtpack, parser.ParseComments)
	if err != nil {
		fmt.Print(err) // parse error
		return
	}
	files := []*ast.File{f}

	// Create the type-checker's package.
	pkg := types.NewPackage("bigstruct", "")

	// Type-check the package, load dependencies.
	// Create and build the SSA program.
	hello, _, err := ssautil.BuildPackage(
		&types.Config{Importer: importer.Default()}, fset, pkg, files, ssa.SanityCheckFunctions)
	if err != nil {
		fmt.Print(err) // type error in some package
		return
	}

	// Print out the package.
	hello.WriteTo(os.Stdout)

	// Print out the package-level functions.
	hello.Func("Donothing").WriteTo(os.Stdout)
	hello.Func("PtrDonothing").WriteTo(os.Stdout)
	hello.Func("main").WriteTo(os.Stdout)

}
