// https://play.golang.org/p/YBH3M5FSpFN
package main

import (
	. "context"
	"database/sql"
	"fmt"
	"log"
	"time"

	smock "github.com/DATA-DOG/go-sqlmock"
)

var query = "select exists(select 1)"

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile | log.Lmicroseconds)

	db, mock, err := smock.New()
	if err != nil {
		log.Fatal(err)
	}
	prepare(mock)

	dberr := make(chan error, 1)
	timeout, cancel := WithTimeout(Background(), 1*time.Second)
	defer cancel()
	successCase(db, dberr, timeout)
	failedCase(db, dberr, timeout)
	timeoutCase(db, dberr, timeout)

}

func prepare(mock smock.Sqlmock) {

	column := []string{"exists"}
	// success case
	mock.ExpectQuery("select(.+)").
		WillReturnRows(smock.NewRows(column).AddRow(true))

	// error case
	mock.ExpectQuery("select(.+)").
		WillReturnError(fmt.Errorf("premeditated"))

	// delay case
	mock.ExpectQuery("select(.+)").
		WillDelayFor(5 * time.Second).
		WillReturnError(fmt.Errorf("no, no way"))
}

func successCase(db *sql.DB, dberr chan error, timeout Context) {
	go func(db *sql.DB, errc chan error, ctx Context) {
		dum := false
		errc <- db.QueryRowContext(ctx, query).Scan(&dum)
	}(db, dberr, timeout)

	select {
	case err := <-dberr:
		if err != nil {
			log.Println("this should be nil, got:", err)
		} else {
			log.Println("case success")
		}
	case <-timeout.Done():
		log.Println("oh no, no way")
	}
}

func failedCase(db *sql.DB, dberr chan error, timeout Context) {
	go func(db *sql.DB, errc chan error, ctx Context) {
		dum := false
		errc <- db.QueryRowContext(ctx, query).Scan(&dum)
	}(db, dberr, timeout)

	select {
	case err := <-dberr:
		if err == nil {
			log.Println("this should not be nil")
		} else {
			log.Println("yay, error:", err)
		}
	case <-timeout.Done():
		log.Println("oh no, no way")
	}
}

func timeoutCase(db *sql.DB, dberr chan error, timeout Context) {
	go func(db *sql.DB, errc chan error, ctx Context) {
		dum := false
		errc <- db.QueryRowContext(ctx, query).Scan(&dum)
	}(db, dberr, timeout)

	select {
	case err := <-dberr:
		log.Println("this should not be reachable, but ...", err)
	case <-timeout.Done():
		log.Println("oh no, timeout! anyway")
	}
}
