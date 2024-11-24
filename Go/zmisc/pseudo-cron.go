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

func each10s() time.Duration {
	return time.Duration(10-(time.Now().Second()%10)) * time.Second
}

func main() {
	log.Println("ready to run!")
	hour, minute, second := time.Now().Clock()
	go func() {
		for {
			select {
			case <-time.After(each10s()):
				log.Println("Just in case time is ticking away.")
			//case <-time.After(untilHour(0, 0, 0)):
			case <-time.After(untilHour(hour, minute+1, second)):
				log.Println("Cinderella, you're late!")

			}
		}
	}()
	time.Sleep(1 * time.Minute)
}
