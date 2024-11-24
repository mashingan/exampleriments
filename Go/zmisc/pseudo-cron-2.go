package main

import (
	"log"
	"time"
)

func untilHour(hour, minute, second int) time.Duration {
	now := time.Now()
	diff := 24 * time.Hour
	year, month, day := now.Date()
	targetTime := time.Date(year, month, day, hour, minute, second, 0, time.Local)
	if now.After(targetTime) {
		diff = now.Sub(targetTime) + 24*time.Hour
	} else if now.Before(targetTime) {
		diff = targetTime.Sub(now)
	} else {
		diff = 0
	}

	return time.Duration(diff)
}

func eachTens(tens int, eventName string, event chan string) {
	for {
		now := time.Now()
		diff := time.Duration(tens) * time.Second
		rest := now.Second() % tens
		if rest != 0 {
			diff = time.Duration(tens-rest) * time.Second
		}
		time.Sleep(diff)
		event <- eventName
	}
}

type Executor func()

var eventExecutor map[string]Executor = make(map[string]Executor)

func main() {
	log.SetFlags(log.LstdFlags)
	log.Println("ready to run!")
	eventListener := make(chan string)
	eventExecutor["each10s"] = func() { log.Println("just in case time is ticking away.") }
	eventExecutor["each20s"] = func() { log.Println("ticking away each 20 seconds.") }
	eventExecutor["cinderellaTimeout"] = func() { log.Println("Cinderelly (not really), you're late!") }
	go eachTens(10, "each10s", eventListener)
	go eachTens(20, "each20s", eventListener)
	go func(ev chan string) {
		hour, minute, second := time.Now().Clock()
		time.Sleep(untilHour(hour, minute+1, second))
		ev <- "cinderellaTimeout"
	}(eventListener)
	go func() {
		for event := range eventListener {
			ex, ok := eventExecutor[event]
			if !ok {
				continue
			}
			ex()
		}
	}()
	time.Sleep(1 * time.Minute)
}
