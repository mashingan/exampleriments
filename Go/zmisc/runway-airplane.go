package main

import (
	"log"
	"sync"
	"time"
)

func launchAndLand(w *sync.WaitGroup, airplane int, runwayReady chan struct{}) {
	defer func() {
		w.Done()
		runwayReady <- struct{}{}
	}()
	log.Printf("airplane %d wait for taking off clearance\n", airplane)
	select {
	case <-runwayReady:
		log.Printf("airplane %d got clearance, taking off.\n", airplane)
	case <-time.After(2 * time.Second):
		log.Printf("airplane %d never took off. Canceled.\n", airplane)
		return
	}

	time.Sleep(200 * time.Millisecond)
	log.Printf("airplane %d is flying.\n", airplane)
	startedFlying := time.Now()
	runwayReady <- struct{}{}
	runOutOfFuel := make(chan struct{}, 1)
	landed := make(chan struct{}, 1)
	go func(id int, landed, runOutOfFuel chan struct{}) {
		select {
		case <-landed:
			return
		case <-time.After(3 * time.Second):
			runOutOfFuel <- struct{}{}
		}
	}(airplane, landed, runOutOfFuel)
	time.Sleep(1 * time.Second)
flying:
	for {
		select {
		case <-runwayReady:
			log.Printf("airplane %d got cleareance after flying %s\n",
				airplane, time.Now().Sub(startedFlying))
			break flying
		case <-runOutOfFuel:
			log.Printf("AIRPLANE %d RAN OUT OF FUEL, CRASHED!! FLIGHT TIME %s\n",
				airplane, time.Now().Sub(startedFlying))
			return
		case <-time.Tick(500 * time.Millisecond):
			log.Printf("airplane %d is holding pattern, flying time %s\n",
				airplane, time.Now().Sub(startedFlying))
		}
	}
	log.Printf("airplane %d is landing with flight time %s.\n", airplane, time.Now().Sub(startedFlying))
	landed <- struct{}{}
	time.Sleep(200 * time.Millisecond)
	//runwayReady <- struct{}{}
}

func main() {
	log.SetFlags(log.Ltime | log.Lmicroseconds | log.Lshortfile)
	airplanes := make([]int, 10)
	for i := range airplanes {
		airplanes[i] = i
	}
	var wg sync.WaitGroup
	runwayAccess := make(chan struct{}, 1)
	runwayAccess <- struct{}{} // runway is clear to be used rn
	start := time.Now()
	log.Println("starting out airplanes")
	for _, airplane := range airplanes {
		wg.Add(1)
		go launchAndLand(&wg, airplane, runwayAccess)
	}
	wg.Wait()
	log.Println("Time spending for runway access:", time.Now().Sub(start))
}
