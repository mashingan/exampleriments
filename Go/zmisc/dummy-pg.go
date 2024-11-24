package main

import (
	"database/sql"
	"encoding/json"
	"fmt"

	_ "github.com/lib/pg"
)

type book struct {
	Author author `json:"author"`
}

type author struct {
	ID   sql.NullString `json:"id"`
	Name sql.NullString `json:"name"`
}

func (a author) MarshalJSON() ([]byte, error) {
	mapres := map[string]interface{}{}
	if !a.ID.Valid {
		mapres["id"] = nil
	} else {
		mapres["id"] = a.ID.String
	}
	if !a.Name.Valid {
		mapres["name"] = nil
	} else {
		mapres["name"] = a.Name.String
	}
	return json.Marshal(mapres)
}

func getAuthor(db *sql.DB, id string) author {
	res := author{}
	err := db.QueryRow("select * from author where id = ?", id).
		Scan(&res.ID, &res.Name)
	if err != nil {
		panic(err)
	}
	return res
}

func main() {
	db, err := sql.Open("postgres", "./dummy.db")
	if err != nil {
		panic(err)
	}
	defer db.Close()
	_, err = db.Exec(`
drop table if exists author;
create table author(id text, name text);
insert into author(id, name) values
('id1', 'name1'),
('this-nil', null);`)
	if err != nil {
		panic(err)
	}
	auth1 := author{ID: sql.NullString{String: "id1"}}
	auth1 = getAuthor(db, auth1.ID.String)
	fmt.Println(auth1)
	jsonbyte, _ := json.Marshal(auth1)
	fmt.Println(string(jsonbyte))
	b1 := book{Author: auth1}
	fmt.Println(b1)
	jbook1, _ := json.Marshal(b1)
	fmt.Println(string(jbook1))
	auth2 := getAuthor(db, "this-nil")
	fmt.Println(auth2)
	jbyte, _ := json.Marshal(auth2)
	fmt.Println(string(jbyte))
	b2 := book{Author: auth2}
	fmt.Println(b2)
	jbook1, _ := json.Marshal(b2)
	fmt.Println(string(jbook2))
}
