package main

import (
	"fmt"
	"log"
	"os"
	"sync"
)

type Logger struct {
	*log.Logger
	curriter uint
	currline uint
	file     *os.File
}

func logger(lg *Logger, msgchan <-chan string) {
	for msg := range msgchan {
		lg.Print(msg)
		lg.currline += 1
		if lg.currline%1000 == 0 {
			lg.curriter += 1
			if err := newFileLogger(lg); err != nil {
				break
			}
		}

	}
}

func newFileLogger(lg *Logger) error {
	if lg.file != nil {
		lg.file.Close()
	}
	newfile, err := os.OpenFile(fmt.Sprintf("log_%03d.log", lg.curriter),
		os.O_WRONLY|os.O_CREATE, 0755)
	if err != nil {
		return fmt.Errorf("newFileLogger: %s", err.Error())
	}
	lg.SetOutput(newfile)
	lg.file = newfile
	return nil
}

func newLogger(prefix string) (*Logger, error) {
	result := new(Logger)
	result.Logger = new(log.Logger)
	result.SetPrefix(prefix)
	if err := newFileLogger(result); err != nil {
		return nil, fmt.Errorf("newLogger: %s", err.Error())
	}
	result.SetFlags(log.LstdFlags | log.LUTC)
	return result, nil
}

func main() {
	lg, err := newLogger("File Logger (UTC):")
	if err != nil {
		panic(err)
	}
	//msg := make(chan string, 100)
	msg := make(chan string)
	go logger(lg, msg)
	var wg sync.WaitGroup
	for i := 0; i < 1500; i++ {
		wg.Add(1)
		go func(msg string, msgchan chan<- string, w *sync.WaitGroup) {
			defer w.Done()
			msgchan <- msg
		}(fmt.Sprintf("msg %d", i), msg, &wg)
	}
	wg.Wait()
	//lg.Print("this is test message")
	close(msg)
}
