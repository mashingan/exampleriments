package main

import (
	"fmt"
	"io"
	"math"
)

type Student struct {
	name  string
	score int32
	input io.Reader
}

func (s Student) String() string {
	return fmt.Sprintf("%s (%d)", s.name, s.score)
}
func (s *Student) AskInput(i int) {
	fmt.Printf("Input %d Student’s Name : ", i)
	fmt.Fscan(s.input, &s.name)
	fmt.Print("Input " + s.name + " Score : ")
	fmt.Fscan(s.input, &s.score)
}

func main() {
	r1, w1 := io.Pipe()
	const N = 5
	go func(w io.WriteCloser) {
		for i := 1; i <= N; i++ {
			w.Write([]byte(fmt.Sprintf("Student#%d\n", i)))
			w.Write([]byte(fmt.Sprintf("%d\n", i*10)))
		}
		w.Close()
	}(w1)
	max := Student{score: math.MinInt32}
	min := Student{score: math.MaxInt32}
	avg := float64(0)
	curr := Student{input: r1}
	for i := 0; i < N; i++ {
		curr.AskInput(i)
		if max.score < curr.score {
			max = curr
		}
		if min.score > curr.score {
			min = curr
		}
		avg += float64(curr.score)
	}
	avg /= N
	fmt.Println("\n\nAvarage Score Students is", avg)
	fmt.Println("Max Score Students is :", max)
	fmt.Println("Min Score Students is :", min)
}
