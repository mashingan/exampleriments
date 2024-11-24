package main

import (
	//"crypto/tls"
	"log"

	"github.com/go-ldap/ldap"
)

func main() {
	server := "10.0.0.122"
	user := "wireless\\" + "px.chandra"
	pass := "K0lorM3l@RXXL"

	l, err := ldap.DialURL("ldap://" + server)
	//l, err := ldap.DialURL("ldaps://"+server,
	//ldap.DialWithTLSConfig(&tls.Config{InsecureSkipVerify: true}))
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()
	log.Println("connect success")
	if err := l.Bind(user, pass); err != nil {
		log.Fatal(err)
	}
	log.Println("authenticated")
}
