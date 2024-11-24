package links

import (
	"log"

	"graphql-intro/graph/model"
	"graphql-intro/internal/pkg/db/migrations/database"
)

// Link is hacker news link
/*
type Link struct {
	ID      string
	Title   string
	Address string
	User    *model.User
}
*/

// Saved is to put the link into database.
func Saved(link model.Link) int64 {
	stmt, err := database.Db.Prepare("INSERT INTO Links(Title, Address, UserID) VALUES (?, ?, ?)")
	if err != nil {
		log.Fatal(err)
	}
	res, err := stmt.Exec(link.Title, link.Address, link.User.ID)
	if err != nil {
		log.Fatal(err)
	}
	id, err := res.LastInsertId()
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Link saved with id:", id)
	return id
}

// GetAll returns all saved links.
func GetAll(ch chan<- *model.Link) {
	defer close(ch)
	rows, err := database.Db.Query(`
	select L.id, L.title, L.address, L.UserID, U.Username
	from Links L inner JOIN Users U on L.UserID = U.ID`)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()
	//var links []model.Link
	for rows.Next() {
		id := ""
		username := ""
		link := model.Link{}
		err := rows.Scan(&link.ID, &link.Title, &link.Address, &id, &username)
		if err != nil {
			log.Println("Getall db scan err:", err)
			continue
		}
		link.User = &model.User{
			ID:   id,
			Name: username,
		}
		ch <- &link
		//links = append(links, link)
	}
	if err := rows.Err(); err != nil {
		log.Println(err)
	}
	//return links
}
