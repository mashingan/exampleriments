package main

import (
	"context"
	"errors"
	"fmt"
	"sync"
)

type data struct {
	Tipe string
	Msg  string
}

var msg = make(chan *data, 1)
var errChan = make(chan error, 1)

func main() {
	var wg sync.WaitGroup
	wg.Add(4)

	go func() {
		wg.Wait()
		close(msg)
	}()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go runJob(ctx, "work", "ayo kerjaaa", &wg)
	go runJob(ctx, "olahraga", "ayo sepak bola", &wg)
	go runJob(ctx, "istirahat", "ayo tidur", &wg)
	go runJob(ctx, "olahraga", "ayo sepedaan", &wg)

	go func() {
		fmt.Println("got error:", <-errChan)
		cancel()
		close(msg)
	}()

	for m := range msg {
		fmt.Println(m)
	}
}

// this function should called with go routine
func runJob(ctx context.Context, tipe, job string, wg *sync.WaitGroup) {
	defer wg.Done()

	select {
	case <-ctx.Done():
		return
	default:

		if tipe == "work" {
			errChan <- errors.New("gamau kerja")
			return
		}
		d := &data{
			Tipe: tipe,
			Msg:  job,
		}

		msg <- d
	}
}
