package main

import (
	"database/sql"
	"log"

	_ "github.com/mattn/go-sqlite3"
)

var db *sql.DB

func connectDatabase() {
	log.Println("Database connecting...")
	var err error
	db, err = sql.Open("sqlite3", "./app.db")
	if err != nil {
		log.Fatalf("Cannot open database %v", err)
	}
	tblq := `
CREATE TABLE IF NOT EXISTS users(
	name text,
	email text unique not null,
	pass text
)`
	if _, err := db.Exec(tblq); err != nil {
		log.Fatalf("Cannot create table err: %v", err)
	}
	log.Println("Database connected.")
}
