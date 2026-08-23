package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"maps"
	"strings"

	"github.com/mashingan/smapping"
	_ "github.com/mattn/go-sqlite3"
)

type book struct {
	Author author `json:"author"`
}

type author struct {
	Num  int            `json:"num"`
	ID   sql.NullString `json:"id"`
	Name sql.NullString `json:"name"`
}

func (a author) MarshalJSON() ([]byte, error) {
	mapres := map[string]any{}
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
	return json.Marshal(mapres)
}

func getAuthor(db *sql.DB, id string) author {
	res := author{}
	err := db.QueryRow("select * from author where id = ?", id).
		Scan(&res.Num, &res.ID, &res.Name)
	if err != nil {
		panic(err)
	}
	return res
}

func getAuthor12(db *sql.DB, id string) author {
	result := author{}
	fields := []string{"num", "id", "name"}
	err := smapping.SQLScan(
		db.QueryRow("select * from author where id = ?", id),
		&result,
		"json",
		fields...)
	if err != nil {
		panic(err)
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
		panic(err)
	}
	return result
}

func getAllAuthor(db *sql.DB) []author {
	result := []author{}
	rows, err := db.Query("select * from author")
	if err != nil {
		panic(err)
	}
	defer rows.Close()
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

func mapAnyDirectly(db *sql.DB) {
	r := map[string]any{}
	rows, err := db.Query("select name, type from pragma_table_info('author')")
	if err != nil {
		panic(err)
	}
	defer rows.Close()
	keys := make([]string, 0, 16)
	for rows.Next() {
		name := ""
		tp := ""
		if err := rows.Scan(&name, &tp); err != nil {
			log.Println("error scan:", err)
			continue
		}
		var v any = nil
		// _ = tp
		switch tp {
		case "INTEGER":
			v = 0
		case "TEXT":
			v = sql.NullString{}
		default:
			v = nil
		}
		keys = append(keys, name)
		r[name] = v
	}
	q := fmt.Sprintf("select %s from author", strings.Join(keys, ","))
	log.Println("keys:", keys)
	log.Println("q:", q)
	r2, err := db.Query(q)
	if err != nil {
		log.Fatal(err)
	}
	defer r2.Close()
	for r2.Next() {
		rm := map[string]any{}
		maps.Copy(rm, r)
		if err := smapping.SQLScan(r2, rm, "", keys...); err != nil {
			log.Println("error smapping map:", err)
			continue
		}
		log.Printf("the map is: %v\n", rm)
	}
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	db, err := sql.Open("sqlite3", "./dummy.db")
	if err != nil {
		panic(err)
	}
	defer db.Close()
	_, err = db.Exec(`
drop table if exists author;
create table author(num integer primary key autoincrement, id text, name text);
insert into author(id, name) values
('id1', 'name1'),
('this-nil', null);`)
	if err != nil {
		panic(err)
	}
	//auth1 := author{ID: &sql.NullString{String: "id1"}}
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
	mapAnyDirectly(db)
}
