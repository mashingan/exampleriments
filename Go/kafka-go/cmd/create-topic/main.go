package main

import (
	"log"
	"os/exec"
)

func main() {
	// podman-compose exec broker \
	//   kafka-topics --create \
	//     --topic purchases \
	//     --bootstrap-server localhost:9092 \
	//     --replication-factor 1 \
	//     --partitions 1
	log.SetFlags(log.LstdFlags | log.Llongfile)
	topicCmd := []string{
		"exec", "broker",
		"kafka-topics", "--create",
		"--topic purchases",
		"--bootstrap-server localhost:9002",
		"--replication-factor 1",
		"--partitions 1",
	}
	topicCreate := exec.Command("docker-compose", topicCmd...)
	if err := topicCreate.Run(); err != nil {
		log.Fatal(err)
	}
}
