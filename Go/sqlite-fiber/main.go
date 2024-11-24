package main

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"strconv"
	"syscall"

	"github.com/gofiber/fiber/v2"
	_ "github.com/mattn/go-sqlite3"
)

type User struct {
	Id    int    `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

func handlerFunc(msg string) fiber.Handler {
	return func(c *fiber.Ctx) error {
		return c.Status(http.StatusOK).SendString(msg)
	}
}

func allUsers(db *sql.DB) fiber.Handler {
	return func(c *fiber.Ctx) error {
		var users []User
		const q = `select * from users`
		rows, err := db.Query(q)
		if err != nil {
			return fiber.NewError(http.StatusInternalServerError, err.Error())
		}
		defer rows.Close()
		for rows.Next() {
			var user User
			if err := rows.Scan(&user.Id, &user.Name, &user.Email); err != nil {
				log.Println(err)
				continue
			}
			users = append(users, user)
		}
		log.Println("{}", users)
		return c.Status(http.StatusOK).JSON(map[string]interface{}{
			"users": users,
		})
	}
}

func newUser(db *sql.DB) fiber.Handler {
	return func(c *fiber.Ctx) error {
		name := c.Params("name")
		email := c.Params("email")
		const q = `insert into users (name, email) values(?, ?)`
		n, err := db.Exec(q, name, email)
		if err != nil {
			return fiber.NewError(http.StatusInternalServerError, err.Error())
		}
		affected, _ := n.RowsAffected()
		if affected < 1 {
			return fiber.NewError(http.StatusInternalServerError,
				fmt.Sprintf("user (%s) with email (%s) is not inserted", name, email))
		}
		return c.Status(http.StatusOK).SendString(name + " user succesfully entered")
	}
}

func deleteUser(db *sql.DB) fiber.Handler {
	return func(c *fiber.Ctx) error {
		name := c.Params("name")
		const q = `drop users where name = ?`
		n, err := db.Exec(q, name)
		if err != nil {
			return fiber.NewError(http.StatusInternalServerError, err.Error())
		}
		aff, _ := n.RowsAffected()
		if aff < 1 {
			return fiber.NewError(http.StatusInternalServerError, name+" cannot be deleted")
		}
		return c.Status(http.StatusOK).SendString(name + " user successfully deleted")
	}
}

func updateUser(db *sql.DB) fiber.Handler {
	return func(c *fiber.Ctx) error {
		name := c.Params("name")
		email, err := url.QueryUnescape(c.Params("email"))
		if err != nil {
			return fiber.NewError(http.StatusBadRequest, err.Error())
		}
		const q = `update users set email = ? where name = ?`
		n, err := db.Exec(q, email, name)
		if err != nil {
			return fiber.NewError(http.StatusInternalServerError, err.Error())
		}
		affected, _ := n.RowsAffected()
		if affected < 1 {
			return fiber.NewError(http.StatusInternalServerError, name+" user not updated")
		}
		return c.Status(http.StatusOK).
			SendString(name + " user successfully update email to " + email)
	}
}

func usersByPage(db *sql.DB) fiber.Handler {
	return func(c *fiber.Ctx) error {
		limit, err := strconv.Atoi(c.Query("limit", "5"))
		if err != nil {
			return fiber.NewError(http.StatusBadRequest, err.Error())
		}
		page, err := strconv.Atoi(c.Query("page", "1"))
		if err != nil {
			return fiber.NewError(http.StatusBadRequest, err.Error())
		}
		const q = "select * from users limit ? offset ?"
		rows, err := db.Query(q, limit, limit*(page-1))
		if err != nil {
			return fiber.NewError(http.StatusInternalServerError, err.Error())
		}
		defer rows.Close()
		var result []User
		for rows.Next() {
			var user User
			if err := rows.Scan(&user.Id, &user.Name, &user.Email); err != nil {
				log.Println(err)
				continue
			}
			result = append(result, user)
		}
		return c.Status(http.StatusOK).JSON(map[string]interface{}{
			"data": result,
		})
	}
}

func handleRequest(db *sql.DB) *fiber.App {
	app := fiber.New()

	app.Get("/users", allUsers(db))
	ugroup := app.Group("/user")
	{
		ugroup.Get("", usersByPage(db))
		ugroup.Post("new/:name/:email", newUser(db))
		ugroup.Delete("remove/:name", deleteUser(db))
		ugroup.Put("edit/:name/with-new-email/:email", updateUser(db))
	}

	return app
}

func migrate(db *sql.DB) error {
	q := `
create table if not exists users (
id integer primary key autoincrement,
name text,
email text)
`
	if _, err := db.Exec(q); err != nil {
		return err
	}
	return nil
}

var (
	quit    chan os.Signal
	sqlOpen = sql.Open
)

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	log.Println("Go fiber tutorials")
	db, err := sqlOpen("sqlite3", "file.db")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()
	if err := migrate(db); err != nil {
		log.Fatal(err)
	}
	app := handleRequest(db)

	go func() {
		app.Listen("localhost:3000")
	}()

	quit = make(chan os.Signal, 3)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit

	log.Printf("shutdown error: %v", app.Shutdown())
}
