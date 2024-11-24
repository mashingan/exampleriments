package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math/rand"
	"os"
	"path/filepath"
	"runtime"
	"sync"
	"time"
)

const (
	totalFile     = 1000
	contentLength = 5 << 20
	oneMb         = 1 << 20
)

var tempPath = filepath.Join(os.TempDir(), "temp-concurrency-pipeline")

func init() {
	rand.Seed(time.Now().UnixNano())
}

func main() {
	log.Println("start")
	start := time.Now()

	generateFiles(&start)

	duration := time.Since(start)
	log.Println("done concurent in", duration.Seconds(), "seconds")

	generateFiles2(&start)
	duration = time.Since(start)
	log.Println("done sequential in", duration.Seconds(), "seconds")

	generateFiles3(&start)
	duration = time.Since(start)
	log.Println("done concurent 3 in", duration.Seconds(), "seconds")

	os.RemoveAll(tempPath)

	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	log.Println("one mb:", oneMb)
	log.Println("heap memory used:", ms.HeapAlloc/oneMb, " mb")
	//time.Sleep(6 * time.Minute)
	//runtime.ReadMemStats(&ms)
	//log.Println("heap memory used after 6 minutes:", ms.HeapAlloc)
}

func randomString(length int) string {
	letters := []byte("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

	b := make([]byte, length)
	ll := int32(len(letters))
	for i := range b {
		b[i] = letters[rand.Int31n(ll)]
	}

	return string(b)
}

func generateFiles2(start *time.Time) {
	os.RemoveAll(tempPath)
	os.MkdirAll(tempPath, os.ModePerm)

	content := randomString(contentLength)
	*start = time.Now()
	for i := 0; i < totalFile; i++ {
		filename := filepath.Join(tempPath, fmt.Sprintf("file-%d.txt", i))
		err := ioutil.WriteFile(filename, []byte(content), os.ModePerm)
		if err != nil {
			log.Println("Error writing file", filename)
		}

		if i%100 == 0 && i > 0 {
			log.Println(i, "files created")
		}
	}

	log.Printf("%d of total files created", totalFile)
}

func generateFiles(start *time.Time) {
	os.RemoveAll(tempPath)
	os.MkdirAll(tempPath, os.ModePerm)
	var wg sync.WaitGroup
	worker := 10
	ch := make(chan int, worker)

	content := randomString(contentLength)
	*start = time.Now()
	for i := 0; i < worker; i++ {
		go func(i <-chan int, wg *sync.WaitGroup) {
			for j := range i {
				filename := filepath.Join(tempPath, fmt.Sprintf("file-%d.txt", j))
				err := ioutil.WriteFile(filename, []byte(content), os.ModePerm)
				if err != nil {
					log.Println("Error writing file", filename)
				}
				wg.Done()
			}
		}(ch, &wg)
	}

	for i := 0; i < totalFile; i++ {
		wg.Add(1)
		ch <- i
		if i%100 == 0 && i > 0 {
			log.Println(i, "files created")
		}
	}

	wg.Wait()
	close(ch)

	log.Printf("%d of total files created", totalFile)
}

func generateFiles3(start *time.Time) {
	os.RemoveAll(tempPath)
	os.MkdirAll(tempPath, os.ModePerm)
	var wg sync.WaitGroup
	worker := 100
	ch := make(chan int, worker)
	content := randomString(contentLength)
	*start = time.Now()
	go func(ch chan int, wg *sync.WaitGroup) {
		for j := range ch {
			go func(i int, wg *sync.WaitGroup) {
				filename := filepath.Join(tempPath, fmt.Sprintf("file-%d.txt", i))
				err := ioutil.WriteFile(filename, []byte(content), os.ModePerm)
				if err != nil {
					log.Println("Error writing file", filename)
				}
				wg.Done()
			}(j, wg)
		}
	}(ch, &wg)
	for i := 0; i < totalFile; i++ {
		wg.Add(1)
		ch <- i
		if i%100 == 0 && i > 0 {
			log.Println(i, "files created")
		}
	}
	wg.Wait()
	close(ch)
	log.Printf("%d of total files created", totalFile)
}
