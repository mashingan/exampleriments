package main

import (
	"database/sql"
	"database/sql/driver"
	"flag"
	"fmt"
	"log"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

type SqliteTime struct {
	time.Time
	Valid bool
}

func (sq *SqliteTime) Scan(src interface{}) error {
	if src == nil {
		sq.Valid = false
		return nil
	}
	switch src.(type) {
	case string:
		//thetime, err := time.Parse("2006-01-02 15:04:05", src.(string))
		thetime, err := time.Parse("2006-01-02 15:04:05.999Z07:00", src.(string))
		if err != nil {
			return err
		}
		sq.Time = thetime
	case int, int64:
		sq.Time = time.Unix(src.(int64), 0)
	case time.Time:
		sq.Time = src.(time.Time)
	default:
		return fmt.Errorf("SQL scan: invalid src %v conversion to string/int64", src)
	}
	sq.Valid = true
	return nil
}

func (sq SqliteTime) Value() (driver.Value, error) {
	return sq.Time, nil
}

func createTable(db *sql.DB) error {
	const t = `
create table if not exists checktime(
label text,
created_at int default current_timestamp,
updated_at int default current_timestamp,
created_at2 datetime default current_timestamp,
updated_at2 datetime default current_timestamp
)
`
	_, err := db.Exec(t)
	if err != nil {
		return err
	}
	return nil
}

func insertNow(db *sql.DB, label string, when time.Time) error {
	if _, err := db.Exec(
		`
insert into checktime(label, created_at, updated_at, created_at2, updated_at2)
values (?, ?, ?, ?, ?)`,
		label, when, when, when, when); err != nil {
		return err
	}
	return nil
}

func fetchRows(db *sql.DB) {
	rows, err := db.Query(`select label, created_at, created_at2 from checktime`)
	if err != nil {
		log.Fatal("error query select:", err)
	}

	defer rows.Close()
	for rows.Next() {
		var (
			label    string
			thetime  SqliteTime
			thetime2 SqliteTime
		)
		if err := rows.Scan(&label, &thetime, &thetime2); err != nil {
			log.Println("error scanning:", err)
			continue
		}
		fmt.Printf("the '%s' is created_at '%v' with format rfc9993 '%s'\n",
			label, thetime, thetime.Format(time.RFC3339))
		fmt.Printf("the '%s' (2) is created_at2 '%v' with format rfc9993 '%s'\n",
			label, thetime2, thetime2.Format(time.RFC3339))
	}

}

var (
	cleandb = flag.Bool("clean", false, "use to clean existing database")
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	flag.Parse()
	db, err := sql.Open("sqlite3", "check-time.db")
	if err != nil {
		log.Fatal(err)
	}
	if cleandb != nil && *cleandb {
		db.Exec("drop table if exists checktime")
	}
	if err = createTable(db); err != nil {
		log.Fatal(err)
	}

	when := time.Now()
	for i := 0; i < 5; i++ {
		label := fmt.Sprintf("label-%02d", i)
		when = when.Add(time.Duration(i) * time.Minute)
		if err = insertNow(db, label, when); err != nil {
			log.Printf("error insert '%s', %v\n", label, err)
		}
	}

	fetchRows(db)
}
