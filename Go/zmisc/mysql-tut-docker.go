package main

/*
Need to run docker first, also docker daemon must be running too
$ docker run --name=mysql --env "MYSQL_ROOT_PASSWORD -dp 33060:3306 mysql:5.7

or
$ docker run --rm --name=mysql --env "MYSQL_ROOT_PASSWORD -dp 33060:3306 mysql:5.7
this --rm is automatically delete the mysql container when it stops

check whether the docker is running or not with
$ docker ps
*/

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"time"

	_ "github.com/go-sql-driver/mysql"
)

func connect(user, pass, dbname string) (db *sql.DB, err error) {
	local := "127.0.0.1:33060"
	db, err = sql.Open("mysql", fmt.Sprintf("%s:%s@tcp(%s)/%s",
		user, pass, local, dbname))
	if err != nil {
		err = fmt.Errorf("Mysql connect %s:%s of %s error: %s",
			user, pass, local, err.Error())
	}
	return
}

func main() {
	db, err := connect("root", "secret", "")
	if err != nil {
		log.Printf("Error when opening mysql: %s", err.Error())
		return
	}
	defer db.Close()

	/*
			_, err = db.Exec(`
		drop table if exists usersNameEmail;

		create table usersNameEmail(name text, email text);

		insert into usersNameEmail (name, email) values
		('user1', 'email1'),
		('user2', 'email2');
		`)
	*/

	ctx, cancelfunc := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancelfunc()
	res, err := db.ExecContext(ctx, "create database if not exists public")
	//res, err := db.ExecContext(ctx, "drop table if exists usersNameEmail;")
	if err != nil {
		log.Fatal(err)
		return
	}
	log.Println("res:", res)
	db.Close()
	db, err = connect("root", "secret", "public")
	if err != nil {
		log.Fatalf("Error when reopening mysql: %s", err.Error())
		return
	}
	defer db.Close()
	res, err = db.ExecContext(ctx, "drop table if exists usersNameEmail")
	if err != nil {
		log.Fatal(err)
		return
	}
	log.Println("res:", res)
	res, err = db.ExecContext(ctx, "create table usersNameEmail(name text, email text)")
	if err != nil {
		log.Fatal(err)
		return
	}
	log.Println("res:", res)
	insertq := `
insert into usersNameEmail (name, email) values
('user1', 'email1'), ('user2', 'email2')`
	res, err = db.ExecContext(ctx, insertq)
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
