package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	const xmas = "XMAS"
	// f, err := os.Open("sample.txt")
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	rexmas := regexp.MustCompile(xmas)
	letters := [][]byte{}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		txt := scanner.Text()
		b := []byte(txt)
		letters = append(letters, b)
	}

	t1 := xmasCalc(letters, rexmas)
	t2 := xshapedMas(letters)
	fmt.Println(t1)
	fmt.Println(t2)
}

func calc(line []byte, rex *regexp.Regexp) int {
	ll := slices.Clone(line)
	t1 := len(rex.FindAll(line, -1))
	slices.Reverse(ll)
	t2 := len(rex.FindAll(ll, -1))
	return t1 + t2
}

func xmasCalc(letters [][]byte, rexmas *regexp.Regexp) int {
	t1 := 0
	for _, b := range letters {
		t1 += calc(b, rexmas)
	}

	// calculate transpose
	for i := 0; i < len(letters); i++ {
		b := make([]byte, len(letters))
		for j := 0; j < len(letters[i]) && j < len(letters); j++ {
			if i < len(letters[i]) {
				b[j] = letters[j][i]
				continue
			}
			break
		}
		t1 += calc(b, rexmas)
	}

	diag := []byte{}
	for i, j := 0, 0; i < len(letters) && j < len(letters[i]); {
		diag = append(diag, letters[i][j])
		j++
		i++
	}
	t1 += calc(diag, rexmas)
	for i := 1; i < len(letters); i++ {
		dup := []byte{}
		dlow := []byte{}
		for j := 0; j < len(letters[i]); j++ {
			if i+j < len(letters) {
				dup = append(dup, letters[i+j][j])
			}
			if j-i >= 0 {
				dlow = append(dlow, letters[j-i][j])
			}
		}
		t1 += calc(dup, rexmas)
		t1 += calc(dlow, rexmas)
	}
	diag = []byte{}
	for i, j := 0, len(letters[0])-1; i < len(letters) && j >= 0; {
		diag = append(diag, letters[i][j])
		i++
		j--
	}
	t1 += calc(diag, rexmas)
	for i := 1; i < len(letters); i++ {
		dright := []byte{}
		dleft := []byte{}
		for j := 0; j < len(letters[i]); j++ {
			if i+j < len(letters[i]) {
				dright = append(dright, letters[i+j][len(letters[i])-j-1])
			}
			if j-i >= 0 {
				dleft = append(dleft, letters[j-i][len(letters[i])-j-1])
			}
		}
		t1 += calc(dright, rexmas)
		t1 += calc(dleft, rexmas)
	}
	return t1

}

func xshapedMas(letters [][]byte) int {
	total := 0
	for i := 1; i < len(letters)-1; i++ {
		for j := 1; j < len(letters[i])-1; j++ {
			if letters[i][j] != 'A' {
				continue
			}
			c1 := []byte{letters[i-1][j-1], letters[i][j], letters[i+1][j+1]}
			c2 := []byte{letters[i+1][j-1], letters[i][j], letters[i-1][j+1]}
			slices.Sort(c1)
			slices.Sort(c2)
			if string(c1) == "AMS" && string(c2) == "AMS" {
				total++
			}
			// drawMap(c1, c2)

		}
	}
	return total
}

func drawMap(c1, c2 []byte) {
	m := [3][3]byte{
		{c1[0], '.', c2[2]},
		{'.', c1[1], '.'},
		{c2[0], '.', c1[2]},
	}
	for _, b := range m {
		fmt.Printf("%q\n", b)
	}
}
