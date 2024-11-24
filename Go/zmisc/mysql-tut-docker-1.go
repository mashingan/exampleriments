package main

/*
Need to run docker first, also docker daemon must be running too
$ docker run --name=mysql -dp 33060:3306 -v mysql-data:/etc/mysql-data --env "MYSQL_ROOT_PASSWORD=secret" mysql:5.7

or
$ docker run --rm --name=mysql -dp 33060:3306 -v mysql-data:/etc/mysql-data --env "MYSQL_ROOT_PASSWORD=secret" mysql:5.7
this --rm is automatically delete the mysql container when it stops

check whether the docker is running or not with
$ docker ps

This version with already created users for public database and rdruffy:monkeydruffy@localhost:33060 which granted ALL only for public database.
*/

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/go-sql-driver/mysql"
)

func connect(user, pass, dbname, opts string) (db *sql.DB, err error) {
	local := "127.0.0.1:33060"
	db, err = sql.Open("mysql", fmt.Sprintf("%s:%s@tcp(%s)/%s?%s",
		user, pass, local, dbname, opts))
	if err != nil {
		err = fmt.Errorf("Mysql connect %s:%s of %s error: %s",
			user, pass, local, err.Error())
	}
	return
}

func main() {
	db, err := connect("rdruffy", "monkeydruffy", "public", "multiStatements=true")
	if err != nil {
		log.Printf("Error when opening mysql: %s", err.Error())
		return
	}
	defer db.Close()

	count := 0
	for {
		if count > 5 {
			log.Fatal("Failed to connect mysql-db after 5 attempts")

		}
		count++
		err := db.Ping()
		if err == nil {
			break
		}
		fmt.Printf("Attempt %d to connect\n", count)
	}

	res, err := db.Exec(`
drop table if exists usersNameEmail;
create table usersNameEmail(name text, email text);
insert into usersNameEmail (name, email) values
('user1', 'email1'),
('user2', 'email2');`)

	if err != nil {
		log.Fatal(err)
		return
	}
	log.Println("res:", res)
	test(db, "email2")
	test(db, "email3")
}

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
