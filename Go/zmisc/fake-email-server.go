package main

import (
	"errors"
	"io"
	"io/ioutil"
	"log"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/emersion/go-smtp"
)

type fakeMailBackend struct {
	t *testing.T
}

func (bk *fakeMailBackend) Login(state *smtp.ConnectionState, username, password string) (smtp.Session, error) {
	return &fakeMailSession{bk.t}, nil
}
func (bk *fakeMailBackend) AnonymousLogin(state *smtp.ConnectionState) (smtp.Session, error) {
	return &fakeMailSession{bk.t}, nil
}

type fakeMailSession struct {
	t *testing.T
}

func (s *fakeMailSession) Mail(from string, opts smtp.MailOptions) error {
	s.t.Log("Mail from: ", from)
	return nil
}

func (s *fakeMailSession) Rcpt(to string) error {
	s.t.Log("Rcpt to:", to)
	return nil
}

func (s *fakeMailSession) Data(r io.Reader) error {
	b, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	s.t.Log(string(b))

	return nil
}

func (s *fakeMailSession) Reset()        {}
func (s *fakeMailSession) Logout() error { return nil }

func fakeMailServer(wg *sync.WaitGroup, t *testing.T) {
	err := os.Mkdir("./temp", 0755)
	if err != nil && !errors.Is(err, os.ErrExist) {
		wg.Done()
		t.Fatalf("Cannot write temp folder for fake SFTP server: %v", err)
		return
	}
	be := &fakeMailBackend{t}
	s := smtp.NewServer(be)
	s.Addr = ":3025"
	s.Domain = "localhost"
	s.ReadTimeout = 10 * time.Second
	s.WriteTimeout = 10 * time.Second
	s.MaxMessageBytes = 1024 * 1024
	s.MaxRecipients = 50
	s.AllowInsecureAuth = true
	log.Println("Starting server at:", s.Addr)
	wg.Done()
	if err := s.ListenAndServe(); err != nil {
		log.Fatal(err)
	}
}

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	t := &testing.T{}
	go fakeMailServer(&wg, t)
	wg.Wait()
}
