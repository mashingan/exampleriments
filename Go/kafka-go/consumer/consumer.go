package main

import (
	"kafka-go/util"
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	if len(os.Args) < 2 {
		log.Fatalf("Usage: %s <config-file-path>\n", os.Args[0])
	}
	cfgFile := os.Args[1]
	cfg := util.ReadConfig(cfgFile)
	cfg["group.id"] = "kafka-go-getting-started"
	cfg["auto.offset.reset"] = "earliest"
	c, err := kafka.NewConsumer(&cfg)
	if err != nil {
		log.Fatal("Failed to create consumer:", err)
	}
	defer c.Close()
	topic := "purchases"
	err = c.SubscribeTopics([]string{topic}, nil)
	if err != nil {
		log.Fatal("cannot subscribe topic:", err)
	}
	sigchan := make(chan os.Signal, 2)
	signal.Notify(sigchan, syscall.SIGINT, syscall.SIGTERM)
	run := true
	for run {
		select {
		case sig := <-sigchan:
			log.Printf("caugh signal %#v. Terminating\n", sig)
			run = false
		default:
			ev, err := c.ReadMessage(-1)
			if err != nil {
				log.Println("some error happens:", err)
				continue
			}
			if string(ev.Key) == "stop" {
				log.Println("getting stop listening, stop!")
				run = false
				continue
			}
			log.Printf("Consumed event from topic %s: key = %-10s value = %s\n",
				*ev.TopicPartition.Topic, string(ev.Key), string(ev.Value))
		}
	}

}
