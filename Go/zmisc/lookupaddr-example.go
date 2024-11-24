package main

import (
	"flag"
	"log"
	"net"
)

var url = flag.String("url", "google.com", "The url name")

func main() {
	flag.Parse()
	/*
		ips, err := net.LookupIP(*url)
		if err != nil || len(ips) == 0 {
			log.Fatalf("Failed to lookup: %v", err)
		}
		log.Println(ips)
		log.Println(ips[0].String())
		names, err := net.LookupAddr(ips[0].String())
		if err != nil {
			log.Fatalf("Failed to dial: %v", err)
		}
		log.Println(names)
	*/
	//names, err := net.LookupHost("google.com")
	names, err := net.LookupHost(*url)
	if err != nil {
		log.Fatalf("Failed to dial: %v", err)
	}
	for _, name := range names {
		ipnames, err := net.LookupAddr(name)
		if err != nil {
			log.Println("name:", name, "error:", err)
			continue
		}
		log.Println(name, ":", ipnames)
	}
}
