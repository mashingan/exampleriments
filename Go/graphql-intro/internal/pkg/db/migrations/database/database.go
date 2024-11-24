package database

import (
	"database/sql"
	"log"
	"time"

	_ "github.com/go-sql-driver/mysql" // Used for database/sql
	"github.com/golang-migrate/migrate"
	"github.com/golang-migrate/migrate/database/mysql"
	"github.com/golang-migrate/migrate/database/sqlite3"
	_ "github.com/golang-migrate/migrate/source/file" // Used for migrate
	_ "github.com/mattn/go-sqlite3"
)

// Db is the connection to mysql server that will be used
// throughout the app
var Db *sql.DB

var dbtype = "sqlite3"

func init() {
	var err error
	log.Println("dbtype:", dbtype)
	if Db == nil {
		log.Println("Trying to open Db")
		switch dbtype {
		case "mysql":
			Db, err = sql.Open("mysql",
				"rdruffy:rdruffy@tcp(localhost:33060)/hackernews")
			if err != nil {
				log.Fatal(err)
			}
		case "sqlite3":
			Db, err = sql.Open("sqlite3", "./app.db")
			if err != nil {
				log.Fatal(err)
			}
		default:
			log.Fatalf("not supported database: %s", dbtype)

		}
	}

	var count = 0
	for count < 5 {
		if err = Db.Ping(); err == nil {
			log.Printf("Database connected at %d try\n", count)
			return
		}
		count++
		log.Printf("Database failed to connect at %d try, %s\n", count+1, err.Error())
		time.Sleep(2 * time.Second)
	}
	log.Panic("Cannot connect to database after 10 seconds:", err)
}

// Migrate as its name shown, for migrating into mysql
func Migrate() {
	if err := Db.Ping(); err != nil {
		log.Fatal("Migrate: ", err)
	}
	switch dbtype {
	case "mysql":
		driver, _ := mysql.WithInstance(Db, &mysql.Config{})
		m, _ := migrate.NewWithDatabaseInstance(
			"file://internal/pkg/db/migrations/database/mysql",
			"mysql",
			driver,
		)
		if err := m.Up(); err != nil && err != migrate.ErrNoChange {
			log.Fatal("migrate.Up:", err)
		}
	case "sqlite3":
		driver, _ := sqlite3.WithInstance(Db, &sqlite3.Config{})
		m, _ := migrate.NewWithDatabaseInstance(
			"file://internal/pkg/db/migrations/database/sqlite3",
			"sqlite3",
			driver,
		)
		if err := m.Up(); err != nil && err != migrate.ErrNoChange {
			log.Fatal("migrate up:", err)
		}
	default:
		log.Fatalf("Cannot migrate unknown db type %s", dbtype)
	}
}
