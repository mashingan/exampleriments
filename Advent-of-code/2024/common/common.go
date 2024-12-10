package common

import (
	"bufio"
	"os"
)

func ReadLines(fname string, f func(text string)) error {
	file, err := os.Open(fname)
	if err != nil {
		return err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		text := scanner.Text()
		f(text)
	}
	return nil
}
