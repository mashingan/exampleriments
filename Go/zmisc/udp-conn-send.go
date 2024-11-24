package main

import (
    "fmt"
    "net"
)


var list []*net.UDPAddr

func sendResponse(c *net.UDPConn, b *net.UDPAddr, h string) {
    _,u := c.WriteToUDP([]byte(h), b)
    if u != nil {
        fmt.Printf("eror %v", u)
    }
}


func main() {
  p := make([]byte, 2048)
  d := net.UDPAddr{Port: 1234,IP: net.ParseIP("127.0.0.1"),}

  s, e := net.ListenUDP("udp", &d)
  if e != nil {
    fmt.Printf("error : %v\n", e)
    return
  }

  //for i := 0; i < 5; i++ {
  for {
    _,z,k := s.ReadFromUDP(p)

    list = append(list, z)

    fmt.Printf("Read a message from %v %s \n", z, p)
    if k !=  nil {
      fmt.Printf("read data error  %v", k)
      continue
    }
    fmt.Println(list)
    go sendResponse(s, z, string(p))
  }
}
