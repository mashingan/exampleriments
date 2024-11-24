package main

import (
	"kafka-go/util"
	"log"
	"math/rand"
	"os"
	"sync"

	"github.com/confluentinc/confluent-kafka-go/kafka"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	if len(os.Args) < 2 {
		log.Fatalf("Usage: %s <config-file-path>\n", os.Args[0])
	}
	cfgFile := os.Args[1]
	cfg := util.ReadConfig(cfgFile)
	topic := "purchases"
	p, err := kafka.NewProducer(&cfg)
	if err != nil {
		log.Fatal("Failed to create producer:", err)
	}
	// go func(c *kafka.Consumer, e kafka.Event) error {}
	go func() {
		for e := range p.Events() {
			switch ev := e.(type) {
			case *kafka.Message:
				if ev.TopicPartition.Error != nil {
					log.Printf("Failed to deliver message: %#v\n", ev.TopicPartition)
				} else {
					log.Printf("Produced event to topic %s: key=%-10s value = %s\n",
						*ev.TopicPartition.Topic, string(ev.Key), string(ev.Value))
				}
			}
		}
	}()

	users := [...]string{"eabara", "jsmith", "sgarcia", "jbernard", "htanaka", "awalther"}
	items := [...]string{"book", "alarm clock", "t-shirts", "gift card", "batteries"}
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		key := users[rand.Intn(len(users))]
		data := items[rand.Intn(len(items))]
		p.Produce(&kafka.Message{
			TopicPartition: kafka.TopicPartition{
				Topic:     &topic,
				Partition: kafka.PartitionAny,
			},
			Value: []byte(key),
			Key:   []byte(data),
		}, nil)
		wg.Done()
	}
	p.Produce(&kafka.Message{
		TopicPartition: kafka.TopicPartition{
			Topic:     &topic,
			Partition: kafka.PartitionAny,
		},
		Value: []byte("stop"),
		Key:   []byte("stop"),
	}, nil)
	wg.Wait()
	p.Close()
}
