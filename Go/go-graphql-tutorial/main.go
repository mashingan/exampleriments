// ref:
// https://tutorialedge.net/golang/go-graphql-beginners-tutorial/
//
// to reset db:
// sqlite3 tut.db < tut.sql
//
// to check head over to for opening graphql client:
// http://localhost:8080/graphql
package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/graphql-go/graphql"
	"github.com/graphql-go/graphql/language/ast"
	"github.com/graphql-go/handler"
	"github.com/mashingan/goql"
	"github.com/mashingan/smapping"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	db, err := sql.Open("sqlite3", "./tut.db")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		_ = db.Close()
	}()
	// tutorialType.AddFieldConfig("created_at", &graphql.Field{
	// 	Type: graphql.DateTime,
	// })
	type (
		tutId struct {
			Id int `json:"id"`
		}
		listArg struct {
			View      int    `gql:"view,default=20"`
			Page      int    `gql:"page,default=1"`
			Order     string `gql:"order,default=created_at"`
			Direction string `gql:"direction,default=desc"`
		}
	)

	fields := graphql.Fields{
		"tutorial": &graphql.Field{
			Type:        tutorialType,
			Description: "Get tutorial by ID",
			Args:        goql.ArgsStruct(tutId{}, "json"),
			Resolve:     resolveTutorial(db),
		},

		"list": &graphql.Field{
			Type:        graphql.NewList(tutorialType),
			Description: "Get tutorial list",
			Args:        goql.ArgsStruct(listArg{}, "gql"),
			Resolve:     resolveList(db),
		},
	}

	rootquery := graphql.ObjectConfig{Name: "RootQuery", Fields: fields}
	schemacfg := graphql.SchemaConfig{
		Query:    graphql.NewObject(rootquery),
		Mutation: newMut(db),
	}
	schema, err := graphql.NewSchema(schemacfg)
	if err != nil {
		// log.Fatalf("failed to create a new schema, error: %v", err)
		log.Printf("failed to create a new schema, error: %v", err)
	}
	// 	query := `
	// 	{
	// 	    list (view: 5) {
	// 			id
	// 			title
	// 			created_at
	// 			comments {
	// 				body
	// 			}
	// 			author {
	// 				Name
	// 				Tutorials
	// 			}
	// 	    }
	// 	}
	// 	`
	// 	printQuery(schema, query)
	// 	printQuery(schema, `
	// {
	//     tutorial (id: 2) {
	// 	id
	// 	title
	// 	author {
	// 	    Name
	// 	}
	//     }

	// }
	// 	`)
	// 	// #create(title: "Hello 異世界 at %s", author: "rahshingan) {
	// 	printQuery(schema, fmt.Sprintf(`
	// mutation {
	//     create(title: "Hello 異世界 at %s") {
	// 	title
	//     }
	// }
	// 	    `, time.Now().In(time.UTC).Format("2006-01-02 15:04:05")))
	// 	printQuery(schema, `
	// mutation {
	// 	comment(
	// 		body_comment: "Hehehe, good tut!",
	// 		tutorial_id: 1
	// 	) {
	// 		title
	// 		author {
	// 			Name
	// 		}
	// 		comments {
	// 			commenter
	// 		}
	// 	}
	// }`)
	// 	printQuery(schema, `
	// {
	//     list(view: 2) {
	// 	id
	// 	title
	// 	created_at
	//     }
	// }
	// `)

	h := handler.New(&handler.Config{
		Schema:     &schema,
		Pretty:     true,
		GraphiQL:   true,
		Playground: true,
	})
	mux := http.NewServeMux()
	mux.Handle("/graphql", h)
	server := http.Server{
		Addr:              "localhost:8080",
		ReadHeaderTimeout: time.Second * 5,
		WriteTimeout:      time.Second * 2,
	}
	server.Handler = mux
	go func() {
		log.Println("server listening at:", server.Addr)
		server.ListenAndServe()
	}()
	quit := make(chan os.Signal, 3)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit
	log.Println("server is shutting down")
	ctx := context.Background()
	server.Shutdown(ctx)
}

//lint:ignore U1000 will be used when it's used
func printQuery(schema graphql.Schema, q string) {
	r := graphql.Do(graphql.Params{
		Schema:        schema,
		RequestString: q,
	})
	if len(r.Errors) > 0 {
		//log.Fatalf("failed graphql op error: %+v", r.Errors[0])
		log.Printf("failed graphql op error: %+v", r.Errors[0])
		return
	}
	rJSON, _ := json.Marshal(r)
	fmt.Println(string(rJSON))
}

func resolveTutorial(db *sql.DB) graphql.FieldResolveFn {
	return func(p graphql.ResolveParams) (interface{}, error) {
		//fmt.Println("tutorial resolve params:", p)
		id, ok := p.Args["id"].(int)
		if ok {
			var tut Tutorial
			err := db.
				QueryRow("SELECT ID, Title from tutorials where ID = ?", id).
				Scan(&tut.ID, &tut.Title)
			if err != nil {
				fmt.Println(err)
				return tut, err
			}
			return tut, nil
		}
		return nil, fmt.Errorf("id %d tutorial not found", id)
	}
}

func resolveList(db *sql.DB) graphql.FieldResolveFn {
	return func(params graphql.ResolveParams) (interface{}, error) {
		//fmt.Println("list resolve params:", params)
		log.Println("list params:", params.Args)
		var tuts []Tutorial
		limiter := strings.Replace(fmt.Sprintf("order by tutorials.%s %s limit %d",
			params.Args["order"], params.Args["direction"], params.Args["view"]), "'", "''", -1)
		log.Println("limiter:", limiter)
		res, err := db.Query(strings.Replace(`
SELECT tutorials.id, title, strftime('%Y-%m-%dT%H:%M:%SZ', tutorials.created_at),
author_name
FROM tutorials, authors
where tutorials.author = authors.id
__LIMIT__`, "__LIMIT__", limiter, 1))
		if err != nil {
			log.Println(err)
			return tuts, fmt.Errorf("DB failed: %s",
				err.Error())
		}
		defer res.Close()
		for res.Next() {
			emptystr := ""
			tut := Tutorial{Title: &emptystr, CreatedAt: &TimeString{}}
			err = res.Scan(&tut.ID, tut.Title, tut.CreatedAt, &tut.Author.Name)
			if err != nil {
				log.Println(err)
				continue
			}
			cmts := make([]*Comment, 0)
			rowcmt, err := db.Query("select body_comment, commenter from comments where tutorial = ?",
				tut.ID)
			if err != nil {
				log.Println(err)
			} else {
				cmt := Comment{}
				for rowcmt.Next() {
					if err := rowcmt.Scan(&cmt.Body, &cmt.Commenter); err != nil {
						log.Println(err)
						continue
					}
					log.Println("cmt:", cmt)
					cmts = append(cmts, &cmt)
				}
				rowcmt.Close()
			}
			tut.Comments = cmts
			log.Println("tut:", tut)
			tuts = append(tuts, tut)
		}
		return tuts, nil
	}
}

type TimeString struct {
	time.Time
}

func (ts *TimeString) Scan(v interface{}) error {
	vstr, ok := v.(string)
	if !ok {
		return fmt.Errorf("invalid string time")
	}
	t, err := time.Parse(time.RFC3339, vstr)
	if err != nil {
		return err
	}
	ts.Time = t
	return nil
}

func (ts TimeString) String() string {
	return ts.Time.Format(time.RFC3339)
}

func (ts TimeString) MarshalJSON() ([]byte, error) {
	tbyte, err := json.Marshal(ts.Time)
	if err != nil {
		return nil, err
	}
	tbyte = append([]byte(`"`), tbyte...)
	tbyte = append(tbyte, '"')
	return tbyte, err
}

func (ts *TimeString) UnmarshalJSON(b []byte) error {
	strtrim := strings.Trim(string(b), `"`)
	t, err := time.Parse(time.RFC3339, strtrim)
	if err != nil {
		return err
	}
	ts.Time = t
	return nil
}

func (ts TimeString) Name() string {
	return "TimeString"
}
func (ts TimeString) Description() string {
	return "TimeString that implement sql.Scanner"
}
func (ts TimeString) Serialize(gotptr bool) graphql.SerializeFn {
	return func(v interface{}) interface{} {
		// return ts.t.Format(time.RFC3339)
		var t time.Time
		switch vv := v.(type) {
		case TimeString:
			t = vv.Time
		case *TimeString:
			t = vv.Time
		default:
			return nil
		}
		return t.Format(time.RFC3339)
	}
}
func (ts TimeString) ParseValue(gotptr bool) graphql.ParseValueFn {
	return func(v interface{}) interface{} {
		vstr, ok := v.(string)
		if !ok {
			return nil
		}
		t, err := time.Parse(time.RFC3339, vstr)
		if err != nil {
			// log.Println(err)
			return nil
		}
		if gotptr {
			return &TimeString{t}
		}
		return TimeString{t}
	}
}
func (ts TimeString) ParseLiteral(gotptr bool) graphql.ParseLiteralFn {
	return func(value ast.Value) interface{} {
		valstr, ok := value.(*ast.StringValue)
		if !ok {
			return nil
		}
		t, err := time.Parse(time.RFC3339, valstr.Value)
		if err != nil {
			return nil
		}
		if gotptr {
			return &TimeString{t}
		}
		return TimeString{t}
	}
}

// Tutorial is struct that record author and
// the comments in the article.
type Tutorial struct {
	ID       int        `json:"id"`
	Title    *string    `json:"title"`
	Comments []*Comment `json:"comments"`
	Author   `json:"author"`
	// CreatedAt time.Time `json:"created_at"`
	// CreatedAt *time.Time `json:"created_at"`
	// CreatedAt *timestring `json:"created_at"`
	// CreatedAt string `json:"created_at"`
	// CreatedAt TimeString `json:"created_at"`
	CreatedAt *TimeString `json:"created_at"`
}

// Author is the one who write the tutorial.
type Author struct {
	Name      string `json:"Name"`
	Tutorials []int  `json:"Tutorials"`
}

// Comment is member of how many comments made
// at a tutorial.
type Comment struct {
	Body      string `json:"body"`
	Commenter string `json:"commenter"`
}

// func populate() []Tutorial {
// 	author := Author{Name: "Eliot Forbes", Tutorials: []int{1}}
// 	tutorial := Tutorial{
// 		ID:     1,
// 		Title:  "Go GraphQL Tutorial",
// 		Author: author,
// 		Comments: []Comment{
// 			Comment{Body: "First Comment"},
// 		},
// 	}
// 	tuts := make([]Tutorial, 10)
// 	tuts[0] = tutorial
// 	for i := 2; i <= 10; i++ {
// 		//stri := string(i)
// 		stri := fmt.Sprintf("%d", i)
// 		tuts[i-1] = Tutorial{
// 			ID:    i,
// 			Title: "tut" + stri,
// 			Author: Author{
// 				Name:      "author" + stri,
// 				Tutorials: []int{i},
// 			},
// 			Comments: []Comment{
// 				Comment{Body: "comment " + stri},
// 			},
// 		}
// 	}
// 	return tuts
// }

var tutorialType = goql.CreateObject(Tutorial{}, "json", "Tutorial")

//var tutorialType = CreateObject(&Tutorial{}, "", "Tutorial")

type titleArg struct {
	Title  string `json:"title" gql:"title"`
	Author string `json:"author" gql:"author,default=system"`
}

func resolveCreatingTitle(db *sql.DB) graphql.FieldResolveFn {
	return func(p graphql.ResolveParams) (interface{}, error) {
		//fmt.Println("create mut resolve params:", p)
		var (
			authid int64
			tut    Tutorial
		)
		if err := db.QueryRow("select id from authors where author_name = ?",
			p.Args["author"]).Scan(&authid); err != nil {
			if !errors.Is(err, sql.ErrNoRows) {
				log.Println(err)
				return tut, err
			}
			res, err := db.Exec("INSERT INTO authors(author_name) VALUES(?)", p.Args["author"])
			if err != nil {
				log.Println(err)
				return tut, err
			}
			authid, _ = res.LastInsertId()
		}
		title := p.Args["title"].(string)
		res, err := db.Exec("INSERT INTO TUTORIALS (title, author) VALUES (?, ?)",
			title, authid)
		if err != nil {
			fmt.Println(err)
			return tut, err
		}
		id, err := res.LastInsertId()
		if err != nil {
			fmt.Println(err)
			return tut, err
		}
		tut.ID = int(id)
		// tut.Title = title
		tut.Title = &title
		// tut.Author = Author{p.Args["author"].(string)
		tut.Author = Author{
			Name: p.Args["author"].(string),
		}
		return tut, nil
	}
}

type commentArg struct {
	BodyComment string `gql:"body_comment"`
	TutorialId  int    `gql:"tutorial_id"`
	Commenter   string `gql:"commenter"`
}

func resolveCommentArg(db *sql.DB) graphql.FieldResolveFn {
	return func(p graphql.ResolveParams) (interface{}, error) {
		var (
			comarg commentArg
			tut    Tutorial
		)
		if err := smapping.FillStructByTags(&comarg, p.Args, "gql"); err != nil {
			log.Println(err)
			return nil, err
		}
		if comarg.TutorialId == 0 {
			return nil, fmt.Errorf("not supplied tutorial id")
		}
		if comarg.Commenter == "" {
			comarg.Commenter = "system"
		}
		q := `
INSERT INTO comments(body_comment, commenter, tutorial)
VALUES(?,?,?)`
		if _, err := db.Exec(q, comarg.BodyComment, comarg.Commenter,
			comarg.TutorialId); err != nil {
			log.Println(err)
			return tut, err
		}
		tut.ID = comarg.TutorialId
		row, err := db.Query(`
select title, author_name, body_comment, commenter
from comments, authors, tutorials
where tutorials.id = tutorial
and tutorial = ?
and authors.id = author`, tut.ID)
		if err != nil {
			log.Println(err)
			return tut, err
		}
		defer row.Close()
		tut.Comments = make([]*Comment, 0)
		for row.Next() {
			cmt := &Comment{}
			if err = row.Scan(&tut.Title, &tut.Author.Name, &cmt.Body,
				&cmt.Commenter); err != nil {
				log.Println(err)
				break
			}
			log.Println("cmt:", cmt)
			tut.Comments = append(tut.Comments, cmt)
		}
		return tut, err
	}
}

// start of tutorial part 2
func newMut(db *sql.DB) *graphql.Object {
	return graphql.NewObject(graphql.ObjectConfig{
		Name: "mutation",
		Fields: graphql.Fields{
			"create": &graphql.Field{
				Type:        tutorialType,
				Description: "Create a new tutorial",

				Args:    goql.ArgsStruct(titleArg{}, "gql"),
				Resolve: resolveCreatingTitle(db),
			},
			"comment": &graphql.Field{
				Type:        tutorialType,
				Description: "Add new comment",
				Args:        goql.ArgsStruct(commentArg{}, "gql"),
				Resolve:     resolveCommentArg(db),
			},
		},
	})
}

// var gtuts []Tutorial

// func init() {
// 	gtuts = populate()
// }
