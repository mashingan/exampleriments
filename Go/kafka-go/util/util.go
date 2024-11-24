package util

import (
	"bufio"
	"log"
	"os"
	"strings"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

func ReadConfig(cfgFile string) kafka.ConfigMap {
	m := make(map[string]kafka.ConfigValue)
	file, err := os.Open(cfgFile)
	if err != nil {
		log.Fatal("Failed to open file:", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		if !strings.HasPrefix(line, "#") && len(line) != 0 {
			before, after, found := strings.Cut(line, "=")
			if !found {
				continue
			}
			parameter := strings.TrimSpace(before)
			value := strings.TrimSpace(after)
			m[parameter] = value
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Failed to read file:", err)
	}
	return m
}
