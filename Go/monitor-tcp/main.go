package main

import (
	"fmt"
	"log"
	"net"
	"sync"
	"time"
)

func monitor(l net.Listener, w *sync.WaitGroup) {
	defer w.Done()
	connlabel := new(sync.Map)
	for {
		c, err := l.Accept()
		if err != nil {
			log.Println(err)
			break
		}

		go func(cc net.Conn) {
			buff := make([]byte, 10)
			n, err := cc.Read(buff)
			if err != nil {
				log.Println(err)
				return
			}
			currlabel := string(buff[:n])
			connlabel.Store(currlabel, cc)
			_, err = cc.Read(buff)
			if err != nil {
				// log.Println(err)
				log.Printf("server %s is shutted down\n", currlabel)
				return
			}
		}(c)
	}
}

func clients(i int, w *sync.WaitGroup) {
	defer w.Done()
	client, err := net.Dial("tcp", ":9191")
	if err != nil {
		log.Println(err)
	}
	defer client.Close()
	client.Write([]byte(fmt.Sprintf("conn%02d", i)))
	time.Sleep(2 * time.Second)
}

func main() {
	log.SetFlags(log.Ltime | log.Lmicroseconds | log.Lshortfile)
	var wg, wclient sync.WaitGroup
	wg.Add(1)
	l, err := net.Listen("tcp", "localhost:9191")
	if err != nil {
		log.Fatal(err)
	}
	go monitor(l, &wg)
	for i := 0; i < 100; i++ {
		wclient.Add(1)
		go clients(i, &wclient)
	}
	wclient.Wait()
	l.Close()
	wg.Wait()
}
