package main

import (
	"database/sql"
	"database/sql/driver"
	"encoding/json"
	"fmt"
	"log"
	"time"

	"github.com/mashingan/smapping"
	_ "github.com/mattn/go-sqlite3"
)

type book struct {
	Author author `json:"author"`
}

type author struct {
	Num           int            `json:"num"`
	ID            sql.NullString `json:"id"`
	Name          sql.NullString `json:"name"`
	CreatedAt     SqliteTime     `json:"createdAt"`
	UnixCreatedAt SqliteTime     `json:"unixCreatedAt"`
}

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
		thetime, err := time.Parse("2006-01-02 15:04:05", src.(string))
		if err != nil {
			return err
		}
		sq.Time = thetime
	case int, int64:
		sq.Time = time.Unix(src.(int64), 0)
	default:
		return fmt.Errorf("SQL scan: invalid src %v conversion to string/int64", src)
	}
	sq.Valid = true
	return nil
}

func (sq SqliteTime) Value() (driver.Value, error) {
	return sq.Time, nil
}

func (a author) MarshalJSON() ([]byte, error) {
	mapres := map[string]interface{}{}
	if !a.ID.Valid {
		//if a.ID == nil || !a.ID.Valid {
		mapres["id"] = nil
	} else {
		mapres["id"] = a.ID.String
	}
	//if a.Name == nil || !a.Name.Valid {
	if !a.Name.Valid {
		mapres["name"] = nil
	} else {
		mapres["name"] = a.Name.String
	}
	mapres["num"] = a.Num
	if !a.CreatedAt.Valid {
		mapres["createdAt"] = nil
	} else {
		mapres["createdAt"] = a.CreatedAt.Time
	}
	return json.Marshal(mapres)
}

func getAuthor(db *sql.DB, id string) author {
	res := author{}
	err := db.QueryRow("select * from author where id = ?", id).
		Scan(&res.Num, &res.ID, &res.Name, &res.CreatedAt,
			&res.UnixCreatedAt)
	if err != nil {
		log.Fatal(err)
	}
	return res
}

func getAuthor12(db *sql.DB, id string) author {
	result := author{}
	err := smapping.SQLScan(
		db.QueryRow("select * from author where id = ?", id),
		&result,
		"json")
	if err != nil {
		log.Fatal(err)
	}
	return result
}

func getAuthor13(db *sql.DB, id string) author {
	result := author{}
	fields := []string{"num", "name"}
	err := smapping.SQLScan(
		db.QueryRow("select num, name from author where id = ?", id),
		&result,
		"json",
		fields...)
	if err != nil {
		log.Fatal(err)
	}
	return result
}

func getAllAuthor(db *sql.DB) []author {
	result := []author{}
	rows, err := db.Query("select * from author")
	if err != nil {
		log.Fatal(err)
	}
	for rows.Next() {
		res := author{}
		if err := smapping.SQLScan(rows, &res, "json"); err != nil {
			fmt.Println("error scan:", err)
			break
		}
		result = append(result, res)
	}
	return result
}

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	db, err := sql.Open("sqlite3", "./dummy.db")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()
	_, err = db.Exec(`
drop table if exists author;
create table author(
num integer primary key autoincrement,
id text,
name text,
createdAt text,
unixCreatedAt integer);
insert into author(id, name, createdAt, unixCreatedAt) values
('id1', 'name1', null, datetime('now')),
('this-nil', null, datetime('now'), datetime('now'));`)
	if err != nil {
		log.Fatal(err)
	}
	auth1 := author{ID: sql.NullString{String: "id1"}}
	auth1 = getAuthor(db, auth1.ID.String)
	fmt.Println("auth1:", auth1)
	jsonbyte, _ := json.Marshal(auth1)
	fmt.Println("json auth1:", string(jsonbyte))
	b1 := book{Author: auth1}
	fmt.Println(b1)
	jbook1, _ := json.Marshal(b1)
	fmt.Println("json book1:", string(jbook1))
	auth2 := getAuthor(db, "this-nil")
	fmt.Println("auth2:", auth2)
	jbyte, _ := json.Marshal(auth2)
	fmt.Println("json auth2:", string(jbyte))
	b2 := book{Author: auth2}
	fmt.Println("book2:", b2)
	jbook2, _ := json.Marshal(b2)
	fmt.Println("json book2:", string(jbook2))
	fmt.Println("author12:", getAuthor12(db, auth1.ID.String))
	fmt.Println("author13:", getAuthor13(db, auth1.ID.String))
	fmt.Println("all author1:", getAllAuthor(db))
}
