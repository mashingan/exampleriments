package main

import (
	"bytes"
	"flag"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"unsafe"

	"github.com/bytedance/sonic"
)

const arrlen = 64

type ArrayByteAsString [arrlen]byte

func (ab ArrayByteAsString) MarshalJSON() ([]byte, error) {
	return sonic.Marshal(string(ab[:]))
}

type sink struct {
	Field1 ArrayByteAsString `json:"field1"`
	Field2 ArrayByteAsString `json:"field2"`
}

var (
	cpuprofile = flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile = flag.String("memprofile", "", "write memory profile to `file`")
)

func main() {
	runtime.SetCPUProfileRate(1_000_000)
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Println("could not create cpu profile:", err)
			return
		}
		defer f.Close()
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Println("could not start cpu profile:", err)
			return
		}
		defer pprof.StopCPUProfile()
	}
	flag.Parse()
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	sb := new(bytes.Buffer)
	sb.Grow(128)
	for i := 0; i < 128; i++ {
		sb.WriteByte(byte(i))
	}
	var s sink
	sbstr := sb.Bytes()
	sf1 := &s.Field1
	sf2 := &s.Field2
	*sf1 = [arrlen]byte(sbstr)
	*sf2 = *(*[arrlen]byte)(unsafe.Pointer(&sbstr[arrlen]))
	b, err := sonic.Marshal(s)
	log.Printf("sbstr q: %q\n", sbstr)
	log.Printf("sbstr v: %v\n", sbstr)
	log.Printf("sink: %#v\n", s)
	if err != nil {
		log.Println("error:", err)
	} else {
		log.Println("byte: ", string(b))
	}

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Println("could not create memory profile:", err)
			return
		}
		defer f.Close()
		runtime.GC()
		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Println("could not write memory profile:", err)
			return
		}
	}
}
