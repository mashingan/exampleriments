package main

import (
	"context"
	"log"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"golang.org/x/time/rate"
)

func main() {
	ctx, cancel := context.WithCancel(context.Background())

	// Mock function to grab all the serials to use in upcoming requests.
	serials := getAllSerials(ctx)

	// Set up for concurrent processing.
	go requestResetter()
	jobC := make(chan string)            // job queue
	delayC := make(chan int)             // channel to receive delay
	resultC := make(chan *http.Response) // channel for results

	var wg sync.WaitGroup
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	// Set up rate limiter.
	limiter := rate.NewLimiter(workers, 1)

	for i := 0; i < workers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()

			for s := range jobC {
				limiter.Wait(ctx)
				res, err := doSomeRequest(s)
				if err != nil {
					// Handle error.
					log.Println(err)
				}

				// Handle rate limit.
				if res.StatusCode == 429 {
					delay, _ := strconv.Atoi(res.Header.Get("Retry-After"))
					log.Println("rate limit hit, backing off")

					// Back off.
					delayC <- delay

					// Put serial back into job queue.
					jobC <- s
				}

				resultC <- res
			}
		}()
	}

	go processResults(ctx, resultC) // call goroutine to read results
	go backOffProcess(ctx, delayC)  // call goroutine to handle backing off

	for _, s := range serials {
		jobC <- s
	}
	close(jobC)

	wg.Wait()
	close(resultC)
	cancel()

	log.Println("Finished process")
	log.Println("serial length:", len(serials))
	log.Println("total process:", totalProcess)
}

// Rate-limit => 3 req/s

const (
	workers = 3
)

var (
	countRequest int32 = 0
	totalProcess int32 = 0
)

func requestResetter() {
	for {
		select {
		case <-time.Tick(1 * time.Second):
			atomic.StoreInt32(&countRequest, 0)
			log.Println("reset count")
		}

	}
}

func doSomeRequest(serial string) (*http.Response, error) {
	// do the request and send back the results
	// ...
	// handle error

	// mock response

	log.Println("Request count tick:", atomic.LoadInt32(&countRequest), "with arg:", serial)
	res := &http.Response{}
	if countRequest >= workers {
		res.StatusCode = http.StatusTooManyRequests
		res.Header = http.Header{
			"Retry-After": {"1"},
		}
	} else {
		res.StatusCode = http.StatusOK
		atomic.AddInt32(&countRequest, 1)
	}
	return res, nil
}

func getAllSerials(ctx context.Context) []string {
	// Some stuff
	return strings.Split("abcdefghijklmnopqrstuvwxyz", "")
}

func processResults(ctx context.Context, resultC chan *http.Response) {
	for {
		select {
		case _ = <-resultC:
			log.Println("Processed result")
			atomic.AddInt32(&totalProcess, 1)
		case <-ctx.Done():
			//close(resultC)
			return
		}
	}
}

func backOffProcess(ctx context.Context, delayC chan int) {
	for {
		select {
		case d := <-delayC:
			log.Println("Sleeping for", d, "seconds")
			time.Sleep(time.Duration(d) * time.Second)
		case <-ctx.Done():
			close(delayC)
			return
		}
	}
}
