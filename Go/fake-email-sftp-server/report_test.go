package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/emersion/go-smtp"
	"github.com/pkg/sftp"
	"golang.org/x/crypto/ssh"
)

func generatePrivateKey(bitsize int) (privkey, pubkey []byte, err error) {
	return
}

func fakeMtrassServer(wg *sync.WaitGroup, t *testing.T) {
	config := &ssh.ServerConfig{
		PasswordCallback: func(c ssh.ConnMetadata, pass []byte) (*ssh.Permissions, error) {
			if c.User() == "rdruffy" && string(pass) == "rdruffy" {
				return nil, nil
			}
			return nil, fmt.Errorf("Wrong username or password")
		},
	}

	// generate key-pair with 'ssh-keygen -t rsa'
	privateByte, err := ioutil.ReadFile("id_rsa")
	if err != nil {
		log.Fatal("Failed to load private key (./id_rsa")
	}
	private, err := ssh.ParsePrivateKey(privateByte)
	if err != nil {
		log.Fatal("Failed to parse private key")
	}

	config.AddHostKey(private)

	listener, err := net.Listen("tcp", ":3022")
	if err != nil {
		log.Fatalf("Failed to listen on 2200: %s", err.Error())
	}
	log.Println("Listening on 3022...")
	wg.Done()
	for {
		tcpconn, err := listener.Accept()
		if err != nil {
			log.Printf("Failed to accept incoming connection: %s\n", err.Error())
			continue
		}
		sshconn, chans, reqs, err := ssh.NewServerConn(tcpconn, config)
		if err != nil {
			log.Printf("Failed to handshake: %s", err.Error())
			continue
		}
		log.Printf("New ssh connection from %s (%s)\n", sshconn.RemoteAddr(), sshconn.ClientVersion())
		go ssh.DiscardRequests(reqs)
		//go handleChannels(chans)
		for newchan := range chans {
			if t := newchan.ChannelType(); t != "session" {
				newchan.Reject(ssh.UnknownChannelType, fmt.Sprintf("unknown channel type: %s", t))
				return
			}
			conn, reqs, err := newchan.Accept()
			if err != nil {
				log.Printf("Couldn't accept channel: %s\n", err.Error())
			}

			go func(reqs <-chan *ssh.Request) {
				for req := range reqs {
					ok := false
					switch req.Type {
					case "subsystem":
						if string(req.Payload[4:]) == "sftp" {
							ok = true
						}
					}
					req.Reply(ok, nil)
				}
			}(reqs)

			svcopts := []sftp.ServerOption{
				sftp.WithDebug(os.Stdout),
			}
			server, err := sftp.NewServer(conn, svcopts...)
			if err != nil {
				log.Fatal(err)
			}
			if err := server.Serve(); err == io.EOF {
				server.Close()
				log.Println("sftp client exited session.")
			} else if err != nil {
				log.Fatal("sftp server completed with error:", err)
			}
		}
	}
}

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
	if err != nil {
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
