package main

import (
	"database/sql"
	"log"

	_ "github.com/mattn/go-sqlite3"
)

func test(db *sql.DB, email string) {
	exists := 1
	err := db.QueryRow(`select count(*) from usersNameEmail where email = ?`, email).Scan(&exists)

	if err != nil {
		log.Fatal(err)
	}
	if exists > 0 {
		log.Println(email, "already registered")
	} else {
		log.Println(email, "not registered yet")
	}
	log.Println(exists)
}

func main() {
	db, err := sql.Open("sqlite3", "./dummy.db")
	if err != nil {
		panic(err)
	}
	defer db.Close()
	_, err = db.Exec(`
drop table if exists usersNameEmail;
create table usersNameEmail(name text, email text);
insert into usersNameEmail(name, email) values
('user1', 'email1'),
('user2', 'email2');`)
	if err != nil {
		panic(err)
	}
	test(db, "email2")
	test(db, "email3")
}
