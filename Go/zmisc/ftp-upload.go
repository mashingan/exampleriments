package main

import (
	"log"
	"net"
	"time"
)

func main() {
	tcpaddr := net.TCPAddr{
		IP:   net.IPv4(0, 0, 0, 0),
		Port: 3000,
	}
	tcplistener, err := net.ListenTCP("tcp", &tcpaddr)
	if err != nil {
		log.Fatal("cannot listen tcp port 3000: ", err)
	}
	go func() {
		recvcon, err := tcplistener.Accept()
		if err != nil {
			log.Fatal("cannot accept from listener: ", err)
		}
		defer recvcon.Close()
		var b []byte
		readn, err := recvcon.Read(b)
		if err != nil {
			log.Fatal("cannot read recvconn:", err)
		}
		log.Printf("read %d bytes\n", readn)
		log.Println(string(b))
	}()
	conn, err := net.Dial("tcp", "localhost:8021")

	if err != nil {
		log.Fatal("cannot dial tcp to ftp: ", err)
	}
	defer conn.Close()
	writen, err := conn.Write([]byte("PORT 3000"))
	//writen, err := conn.Write([]byte("USER anonymous PASS anonymous\r\n\r\n"))
	/*
		writen, err := conn.Write([]byte("PASV\r\n\r\n"))
		if err != nil {
			log.Fatal("cannot write conn:", err)
		}
	*/
	log.Printf("write %d bytes\n", writen)
	//time.Sleep(50 * time.Millisecond)
	time.Sleep(5 * time.Second)
	/*
		var b []byte
		readn, err := conn.Read(b)
		if err != nil {
			log.Fatal("cannot read conn:", err)
		}
		log.Printf("read %d bytes\n", readn)
		log.Println(string(b))
	*/
}
