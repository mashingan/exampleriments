package main

import (
	"context"
	//"fmt"
	"log"
	"net"

	"google.golang.org/grpc"
)

type server struct {
	UnimplementedEchoServer
}

func (*server) Echo(ctx context.Context, tm *TheMap) (*RetMsg, error) {
	kvint64 := tm.GetInt64Kv()
	kvstr := tm.GetStrkv()
	for k, v := range kvint64 {
		log.Printf("key %s: val %v\n", k, v)
	}
	for k, v := range kvstr {
		log.Printf("key %s: val %v\n", k, v)
	}
	return &RetMsg{Msg: "ok", Error: ""}, nil
}

func newServer() EchoServer {
	return &server{}
}

func setupServer(ready chan bool) {
	lis, err := net.Listen("tcp", ":3000")
	if err != nil {
		log.Fatal(err)
	}
	s := newServer()
	srv := grpc.NewServer()
	RegisterEchoServer(srv, s)
	ready <- true
	log.Fatal(srv.Serve(lis))
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	readyServer := make(chan bool, 1)
	go setupServer(readyServer)
	<-readyServer
	conn, err := grpc.Dial(":3000", grpc.WithInsecure())
	if err != nil {
		log.Fatal(err)
	}
	client := NewEchoClient(conn)
	param := &TheMap{
		Int64Kv: map[string]int64{},
		Strkv:   map[string]string{},
	}
	/*
		param.Keyval["field1"] = any.New("field si string")
		param.Keyval["field2"] = any.New(42)
	*/
	param.Int64Kv["field1"] = 42
	param.Strkv["field1"] = "field si string"
	ret, err := client.Echo(context.Background(), param)
	if err != nil {
		log.Println(err)
	}
	log.Println("ret:", ret)
}
