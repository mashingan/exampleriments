package main

import (
	"log"
	"os/exec"
	"sync"
)

func main() {
	// go build -o out/producer producer/producer.go
	// go build -o out/consumer consumer/consumer.go
	log.SetFlags(log.LstdFlags | log.Llongfile)
	runProd := exec.Command("./out/producer", "getting-started.properties")
	runCons := exec.Command("./out/consumer", "getting-started.properties")
	var wg sync.WaitGroup
	wg.Add(2)
	go func(w *sync.WaitGroup) {
		defer wg.Done()
		if err := runProd.Run(); err != nil {
			log.Fatal(err)
		}
	}(&wg)
	go func(w *sync.WaitGroup) {
		defer wg.Done()
		if err := runCons.Run(); err != nil {
			log.Fatal(err)
		}
	}(&wg)
	wg.Wait()
}
