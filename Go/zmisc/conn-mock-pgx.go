package main

import (
	"bytes"
	"context"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"time"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type mockonn struct {
	buffers [][]byte
	bufpos  int
	closed  bool
	*mockaddr
	deadline time.Time
}

func (m *mockonn) Write(p []byte) (int, error) {
	fmt.Println("conn write:", string(p))
	// m.buffers = append(m.buffers, p)
	return len(p), nil
}

func (m *mockonn) Read(p []byte) (int, error) {
	fmt.Println("mbuf.len:", len(m.buffers))
	fmt.Println("mbufpos:", m.bufpos)
	if m.bufpos >= len(m.buffers) {
		return 0, io.EOF
	}
	defer func() { m.bufpos++ }()
	return copy(p, m.buffers[m.bufpos]), nil
}

func (m *mockonn) Close() error {
	if !m.closed {
		return nil
	}
	return fmt.Errorf("already closed")
}

type mockaddr struct{}

func (m *mockaddr) Network() string {
	return "tcp"
}
func (m *mockaddr) String() string {
	return "127.0.0.1"
}

func (m *mockonn) LocalAddr() net.Addr {
	return m.mockaddr
}

func (m *mockonn) RemoteAddr() net.Addr {
	return m.mockaddr
}

func (m *mockonn) SetDeadline(t time.Time) error {
	m.deadline = t
	return nil
}

func (m *mockonn) SetReadDeadline(t time.Time) error {
	m.deadline = t
	return nil
}

func (m *mockonn) SetWriteDeadline(t time.Time) error {
	m.deadline = t
	return nil
}

func newmockonn() *mockonn {
	m := new(mockonn)
	m.buffers = make([][]byte, 0)
	return m
}

var dummyconstr = "postgres://jack:secret@pg.example.com:5432/mydb?sslmode=verify-ca&pool_max_conns=10"

func mockPgxpool(conn net.Conn) (*pgxpool.Pool, error) {
	pgconnCfg, err := pgconn.ParseConfig(dummyconstr)
	if err != nil {
		return nil, err
	}
	pgconnCfg.Host = "localhost"
	pgconnCfg.Port = 0
	pgconnCfg.Database = "dummy"
	pgconnCfg.User = ""
	pgconnCfg.Password = ""
	pgconnCfg.DialFunc = func(ctx context.Context, network string, addr string) (net.Conn, error) {
		return conn, nil
	}
	pgconnCfg.LookupFunc = func(ctx context.Context, host string) ([]string, error) {
		return []string{"::1", "127.0.0.1"}, nil
	}
	pgxcfg, _ := pgx.ParseConfig("")
	pgxcfg.Config = *pgconnCfg
	poolcfg, _ := pgxpool.ParseConfig("")
	poolcfg.ConnConfig = pgxcfg
	poolcfg.MaxConnLifetime = 120
	poolcfg.MaxConnIdleTime = 120
	poolcfg.MaxConns = 1
	poolcfg.MinConns = 1
	poolcfg.LazyConnect = true
	poolcfg.BeforeAcquire = nil
	poolcfg.BeforeConnect = nil
	poolcfg.AfterConnect = nil
	poolcfg.AfterRelease = nil
	poolcfg.ConnConfig.TLSConfig = nil
	// poolcfg.ConnConfig.BuildFrontend = nil
	return pgxpool.ConnectConfig(context.Background(), poolcfg)
}

func authok() []byte {
	buf := new(bytes.Buffer)
	var (
		msglen int32 = 8
		ok     int32
	)
	buf.Read([]byte{'R'})
	binary.Write(buf, binary.BigEndian, msglen)
	binary.Write(buf, binary.BigEndian, ok)
	return buf.Bytes()
}

func readyForQuery(status byte) []byte {
	buf := new(bytes.Buffer)
	var (
		msglen int32 = 5
	)
	buf.Read([]byte{'Z'})
	binary.Write(buf, binary.BigEndian, msglen)
	buf.Read([]byte{status})
	fmt.Println("ready:", buf)
	return buf.Bytes()
}
