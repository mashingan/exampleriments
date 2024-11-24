package main

import (
	. "github.com/mmcloughlin/avo/build"
	. "github.com/mmcloughlin/avo/operand"
)

func main() {
	TEXT("Sum", NOSPLIT, "func(xs []byte) uint64")
	Doc("Sum returns the sum of the elements in xs.")

	ptr := Load(Param("xs").Base(), GP64())
	n := Load(Param("xs").Len(), GP64())

	Comment("Initialize sum register to zero.")
	s := GP64()
	XORQ(s, s)

	Label("loop")
	Comment("Loop until zero bytes remain.")
	CMPQ(n, Imm(0))
	JE(LabelRef("done"))
	// JCXZQ(LabelRef("done"))
	Comment("why this code not work in asm.go : ")
	Comment(`JCXZQ(LabelRef("done"))`)
	Comment("")

	Comment("Load from pointer and add to running sum.")
	t := GP64()
	MOVQ(Mem{Base: ptr}, t)
	ANDQ(Imm(0xFF), t)
	ADDQ(t, s)

	Comment("Advance pointer, decrement byte count.")
	INCQ(ptr)
	DECQ(n)
	JMP(LabelRef("loop"))

	Label("done")
	Comment("Store sum to return value.")
	Store(s, ReturnIndex(0))
	RET()
	Generate()
}
