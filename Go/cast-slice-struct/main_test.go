package main

import (
	"bytes"
	"testing"
	"unsafe"

	"github.com/bytedance/sonic"
)

func BenchmarkCastStruct(bs *testing.B) {
	sb := new(bytes.Buffer)
	sb.Grow(128)
	for i := 0; i < 128; i++ {
		sb.WriteByte(byte(i))
	}
	sbstr := sb.Bytes()
	bs.ResetTimer()
	var (
		b   []byte
		err error
	)
	for i := 0; i < bs.N; i++ {
		var s sink
		sa := &s
		*sa = *(*sink)(unsafe.Pointer(&sbstr))
		b, err = sonic.Marshal(s)
	}
	_ = err
	bs.Logf("json cast struct: %s", string(b))
}

func BenchmarkCastBytes(bs *testing.B) {
	sb := new(bytes.Buffer)
	sb.Grow(128)
	for i := 0; i < 128; i++ {
		sb.WriteByte(byte(i))
	}
	sbstr := sb.Bytes()
	bs.ResetTimer()
	var (
		b   []byte
		err error
	)
	for i := 0; i < bs.N; i++ {
		var s sink
		sf1 := &s.Field1
		sf2 := &s.Field2
		*sf1 = [arrlen]byte(sbstr)
		*sf2 = *(*[arrlen]byte)(unsafe.Pointer(&sbstr[arrlen]))
		b, err = sonic.Marshal(s)
	}
	_ = err
	bs.Logf("json cast bytes field: %s", string(b))
}
