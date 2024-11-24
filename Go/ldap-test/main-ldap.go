package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"sync"

	"github.com/go-ldap/ldap/v3"
	"github.com/jimlambrt/gldap"
)

type Role string
type User struct {
	name, pass string
	roles      []Role
}

const (
	roleApprover Role = "APPROVER"
	roleMaker    Role = "MAKER"
	roleChecker  Role = "CHECKER"
)

var (
	quit  context.Context
	stop  context.CancelFunc
	users = [...]User{
		{name: "alice", pass: "alicepass", roles: []Role{roleApprover, roleChecker}},
		{name: "bob", pass: "bobpass", roles: []Role{roleChecker, roleMaker}},
	}
)

func runLdapServer(w *sync.WaitGroup, ready chan<- struct{}) {
	defer w.Done()
	// create a new server
	s, err := gldap.NewServer()
	if err != nil {
		log.Fatalf("unable to create server: %s", err.Error())
	}

	// create a router and add a bind handler
	r, err := gldap.NewMux()
	if err != nil {
		log.Fatalf("unable to create router: %s", err.Error())
	}
	r.Bind(bindHandler)
	s.Router(r)
	go s.Run(":10389") // listen on port 10389
	ready <- struct{}{}

	// stop server gracefully when ctrl-c, sigint or sigterm occurs
	quit, stop = signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()
	<-quit.Done()
	log.Printf("\nstopping directory")
	s.Stop()
}

func bindHandler(w *gldap.ResponseWriter, r *gldap.Request) {
	resp := r.NewBindResponse(
		gldap.WithResponseCode(gldap.ResultInvalidCredentials),
	)
	defer func() {
		w.Write(resp)
	}()

	m, err := r.GetSimpleBindMessage()
	if err != nil {
		log.Printf("not a simple bind message: %s", err)
		return
	}
	if m.UserName == "quit" {
		resp.SetResultCode(gldap.ResultSuccess)
		log.Println("stopping ldap server")
		stop()
		return
	}

	for _, user := range users {
		if m.UserName == user.name && m.Password == gldap.Password(user.pass) {
			resp.SetResultCode(gldap.ResultSuccess)
			log.Printf("bind success for %s\n", m.UserName)
			return
		}
	}
}

func connectLdap(username, password, url string) (err error) {
	var lcon *ldap.Conn
	lcon, err = ldap.DialURL(url)
	if err != nil {
		return
	}
	defer lcon.Close()
	if err = lcon.Bind(username, password); err != nil {
		return
	}
	return
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	ready := make(chan struct{}, 1)
	var wg sync.WaitGroup
	wg.Add(1)
	go runLdapServer(&wg, ready)
	const ldapurl = "ldap://localhost:10389"
	<-ready
	if err := connectLdap("alice", "alicepass", ldapurl); err != nil {
		stop()
		log.Fatal(err)
	}
	if err := connectLdap("bob", "bobpass", ldapurl); err != nil {
		stop()
		log.Fatal(err)
	}
	if err := connectLdap("quit", "quit", ldapurl); err != nil {
		stop()
		log.Fatal(err)
	}
	wg.Wait()
}
