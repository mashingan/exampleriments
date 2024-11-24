package main

import (
	"log/slog"
	"os"
	"os/exec"
	"sync"
)

func main() {
	// go build -o out/producer producer/producer.go
	// go build -o out/consumer consumer/consumer.go
	cwd, _ := os.Getwd()
	slog.Info("cwd", "where", cwd)
	buildProd := exec.Command("go", "build", "-o out/producer", "producer/producer.go")
	buildCons := exec.Command("go", "build", "-o out/consumer", "consumer/consumer.go")
	buildProd.Stdout = os.Stdout
	buildCons.Stdout = os.Stdout
	var wg sync.WaitGroup
	go func(w *sync.WaitGroup) {
		defer w.Done()
		wg.Add(1)
		if err := buildProd.Run(); err != nil {
			out, _ := buildProd.Output()
			slog.Error("Build Producer", "error", err.Error(), "stdout", string(out))
		}
	}(&wg)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		wg.Add(1)
		if err := buildCons.Run(); err != nil {
			out, _ := buildCons.Output()
			slog.Error("Build Consumer", "error", err.Error(), "stdout", string(out))
		}
	}(&wg)
	wg.Wait()
}
