// old SO page ref:
// https://stackoverflow.com/questions/35804884/sqlite-concurrent-writing-performance
// to run the test:
// go test -bench=. -benchmem .

package main

import (
	"database/sql"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"path"
	"runtime"
	"strings"
	"sync"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

const (
	msglength = 64
	alphanum  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
	alphalen  = len(alphanum)
)

var (
	db  *sql.DB
	buf = sync.Pool{
		New: func() interface{} {
			b := make([]byte, msglength)
			return &b
		},
	}
)

func migrate(db *sql.DB) error {
	const (
		q = `
create table if not exists testlogs (
  id integer primary key autoincrement,
  msg text,
  func_line text,
  at text default (strftime('%Y-%m-%dT%H:%M:%SZ', 'now'))
);
`
		view = `
create view if not exists last20 as
select * from testlogs order by id desc limit 20
;
`
		wal = `pragma journal_mode=WAL`
	)
	if _, err := db.Exec(q); err != nil {
		log.Println(err)
		return err
	}
	if _, err := db.Exec(view); err != nil {
		log.Println(err)
		return err
	}
	if _, err := db.Exec(wal); err != nil {
		log.Println(err)
	}
	return nil
}

func reinitTable(db *sql.DB) error {
	const drop = `drop table if exists testlogs`
	if _, err := db.Exec(drop); err != nil {
		log.Println(err)
		return err
	}
	if _, err := db.Exec(`drop view if exists last20`); err != nil {
		log.Println(err)
		return err
	}
	if err := migrate(db); err != nil {
		log.Println(err)
		return err
	}
	return nil
}

func randomMsg() *[]byte {
	result := buf.Get().(*[]byte)
	for i := 0; i < msglength; i++ {
		(*result)[i] = alphanum[rand.Intn(alphalen)]
	}
	return result
}

func randomMsgSimple() []byte {
	result := make([]byte, msglength)
	for i := 0; i < msglength; i++ {
		result[i] = alphanum[rand.Intn(alphalen)]
	}
	return result
}

func randomMsgBuilder() string {
	var b strings.Builder
	b.Grow(msglength)
	for i := 0; i < msglength; i++ {
		b.WriteByte(alphanum[rand.Intn(alphalen)])
	}
	return b.String()
}

var (
	cleandb   = flag.Bool("clean", false, "use to clean existing database")
	whichrand = flag.String("rand", "simple", "choose random method: default: simple, available:pool builder")
)

func main() {
	//rand.Seed(time.Now().Unix())
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	flag.Parse()
	//log.Println("whichrand:", *whichrand)
	var err error
	db, err = sql.Open("sqlite3", "file:test.db?cache=shared&journal_mode=WAL")
	if err != nil {
		log.Fatal(err)
	}
	db.SetMaxOpenConns(1)
	if cleandb != nil && *cleandb {
		log.Println("reinit table")
		err = reinitTable(db)
	} else {
		log.Println("migrate table")
		err = migrate(db)
	}
	if err != nil {
		log.Fatal(err)
	}

	initLog(db)
	const logcount = 10000
	startLog := time.Now()
	for i := 0; i < logcount; i++ {
		if whichrand != nil && *whichrand == "pool" {
			msg := randomMsg()
			writeLog(fmt.Sprintf("logs-%04d-%s", i, *msg))
			buf.Put(msg)
		} else if whichrand != nil && *whichrand == "builder" {
			msg := randomMsgBuilder()
			writeLog(fmt.Sprintf("logs-%04d-%s", i, msg))
		} else {
			msg := randomMsgSimple()
			writeLog(fmt.Sprintf("logs-%04d-%s", i, msg))
		}
	}
	flushLog()
	stopRunningLog()
	log.Println("done")
	log.Println("elapsed after ", time.Since(startLog))
	//time.Sleep(5 * time.Second)
}

// log API
// in this example we imaginarily export:
//   initLog, flushLog, writeLog, stopRunningLog

type logbody struct {
	msg      string
	funcLine string
}

var (
	stoplog = make(chan struct{}, 1)
	logmsg  = make(chan logbody, 100)
	logDone sync.WaitGroup
)

func initLog(db *sql.DB) {
	const writequery = `insert into testlogs(msg, func_line) values(?, ?)`
	go func(w *sync.WaitGroup, db *sql.DB) {
		for {
			select {
			case <-stoplog:
				return
			case msg := <-logmsg:
				w.Add(1)
				go func(msg logbody, wg *sync.WaitGroup) {
					defer wg.Done()
					if _, err := db.Exec(writequery, msg.msg, msg.funcLine); err != nil {
						log.Println("err log:", err)
					}
				}(msg, w)
			}
		}
	}(&logDone, db)
}

func flushLog() {
	logDone.Wait()
}

func writeLog(msg string) {
	fp, filepath, line, _ := runtime.Caller(1)
	resource := runtime.FuncForPC(fp).Name()
	_, file := path.Split(filepath)
	msgf := fmt.Sprintf("%s@%s:%d ", resource, file, line)
	logmsg <- logbody{msg, msgf}
}

func stopRunningLog() {
	stoplog <- struct{}{}
}
