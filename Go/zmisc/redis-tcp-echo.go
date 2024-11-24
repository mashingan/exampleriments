package main

import (
	"fmt"
	"log"
	"net"
	"strconv"
)

const listenAddr = ":3000"

func ok(c net.Conn) (int, error) {
	return c.Write([]byte("+OK\r\n"))
}

func errReply(c net.Conn, err error) (int, error) {
	return c.Write([]byte(fmt.Sprintf("-%s\r\n", err.Error())))
}

func main() {
	l, err := net.Listen("tcp", listenAddr)
	if err != nil {
		log.Fatal(err)
	}
	db := map[string][]byte{}
	for {
		c, err := l.Accept()
		if err != nil {
			log.Println(err)
			continue
		}
		ok(c)
		go func(c net.Conn) {
			msgbuf := []byte{}
			for {
				buffer := make([]byte, 512)
				bytesread, err := c.Read(buffer)
				if bytesread == 0 {
					break
				}
				if err != nil {
					log.Println("Read connection error:", err)
					continue
				}
				//log.Println("got bytes read:", bytesread)
				//log.Println(string(buffer[:bytesread-1]))
				for i := 0; i < bytesread; i++ {
					msgbuf = append(msgbuf, buffer[i])
				}

			}
			log.Println("msgbuf:", string(msgbuf))
			lengthmsg := msgbuf[0:3]
			log.Println("lengthmsg:", string(lengthmsg))
			length, _ := strconv.Atoi(string(lengthmsg[1]))
			log.Println("length:", length)
			if length == 1 {
				//ok(c)
				c.Write([]byte("+PONG\r\n"))
				return
			}
			if length == 2 {
				content := msgbuf[13 : len(msgbuf)-2]
				log.Println("content:", string(content))
				key := []byte{}
				isStartCounting := false
				for _, b := range content {
					if b == '$' {
						isStartCounting = true
						continue
					} else if b == '\r' || b == '\n' {
						if isStartCounting {
							isStartCounting = false
						}
						continue
					}
					if isStartCounting {
						continue
					}
					key = append(key, b)
				}
				val, ok := db[string(key)]
				if !ok {
					errReply(c, fmt.Errorf("%s not available", string(key)))
					return
				}
				c.Write([]byte(fmt.Sprintf("$%d\r\n%s\r\n", len(val), string(val))))
			}
			if length == 3 {
				content := msgbuf[13 : len(msgbuf)-2]
				log.Println("content:", string(content))
				keyval := make([][]byte, 2)
				count := -1
				isStartCounting := false
				for _, b := range content {
					if b == '$' {
						count++
						isStartCounting = true
						continue
					} else if b == '\r' || b == '\n' {
						if isStartCounting {
							isStartCounting = false
						}
						continue
					}
					if isStartCounting {
						continue
					}
					keyval[count] = append(keyval[count], b)
				}
				db[string(keyval[0])] = keyval[1]
				log.Println("key:", string(keyval[0]))
				log.Println("val:", string(keyval[1]))
			}
			ok(c)
			defer c.Close()
		}(c)
		//go io.Copy(c, c)
	}
}
