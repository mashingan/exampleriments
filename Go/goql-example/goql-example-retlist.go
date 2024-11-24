package main

import (
	"database/sql"

	"github.com/graphql-go/graphql"
	"github.com/mashingan/goql"
	"github.com/mashingan/smapping"
)

type PostsQuery struct {
	Limit int            `json:"limit"`
	Query sql.NullString `json:"query"`
	Tags  []string       `json:"tags"`
}
type Post struct {
	Id   int    `json:"id"`
	Body string `json:"body"`
}

func main() {
	_ = graphql.Fields{
		"GetPosts": &graphql.Field{
			Type:        goql.CreateObject(Post{}, "json", "Posts"),
			Description: "Return posts",
			Args:        goql.ArgsStruct(PostsQuery{}, "json"),
			Resolve: func(p graphql.ResolveParams) (any, error) {
				var pq PostsQuery
				if err := smapping.FillStructByTags(&pq, p.Args, "json"); err != nil {
					return nil, err
				}
				return nil, nil
			},
		},
	}
}
