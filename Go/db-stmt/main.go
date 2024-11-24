package main

import (
	"database/sql"
	"fmt"
	"log"
	"strings"

	_ "github.com/mattn/go-sqlite3"
)

var (
	db   *sql.DB
	stmt *sql.Stmt
)

const (
	createTable = `
create table if not exists dbtest(
	id integer primary key autoincrement not null,
	label text
)`
	fetchLabelById = `select label from dbtest where id = ?`
	populateQuery  = `insert into dbtest(label) values __VALUES__`
	poplength      = 10
)

func prepareDB() error {
	var err error
	db, err = sql.Open("sqlite3", "file:test.db?cache=shared&mode=memory")
	if err != nil {
		return err
	}
	if _, err = db.Exec(createTable); err != nil {
		return err
	}

	return nil
}

func prepareStmt(db *sql.DB) (err error) {
	stmt, err = db.Prepare(fetchLabelById)
	return
}

func populateTable(db *sql.DB, labelPrefix string) error {
	labels := make([]interface{}, poplength)
	vals := make([]string, poplength)
	for i := 0; i < poplength; i++ {
		labels[i] = fmt.Sprintf("%s-%02d", labelPrefix, i)
		vals[i] = "(?)"
	}
	query := strings.Replace(populateQuery, "__VALUES__",
		fmt.Sprintf("%s", strings.Join(vals, ",")), 1)
	//log.Println("query:", query)
	if _, err := db.Exec(query, labels...); err != nil {
		return err
	}
	return nil
}

func fetchAllRows(db *sql.DB) error {
	rows, err := db.Query("select * from dbtest")
	if err != nil {
		return err
	}
	defer rows.Close()
	for rows.Next() {
		var (
			id    int
			label string
		)
		if err := rows.Scan(&id, &label); err != nil {
			log.Println("err:", err)
			continue
		}
		log.Printf("label with id (%d) is %s\n", id, label)
	}
	return nil
}

func fetchId(id int) (label string, err error) {
	err = stmt.QueryRow(id).Scan(&label)
	return
}

func main() {
	log.SetFlags(log.Lshortfile | log.LstdFlags)
	if err := prepareDB(); err != nil {
		log.Fatal(err)
	}
	if err := prepareStmt(db); err != nil {
		log.Fatal(err)
	}
	if err := populateTable(db, "test-stmt"); err != nil {
		log.Fatal(err)
	}
	if stmt == nil {
		log.Fatal("stmt is nil")
	}

	fetchAllRows(db)

	for i := 1; i <= 3; i++ {
		label, err := fetchId(i)
		if err != nil {
			log.Println("err:", err)
			continue
		}
		log.Printf("label for id (%d) is %s\n", i, label)
	}
}
