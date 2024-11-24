package graph

// This file will be automatically regenerated based on the schema, any resolver implementations
// will be copied through when generating and any unknown code will be moved to the end.

import (
	"context"
	"fmt"
	"log"

	"graphql-intro/graph/generated"
	"graphql-intro/graph/model"
	"graphql-intro/internal/auth"
	"graphql-intro/internal/links"
	"graphql-intro/internal/pkg/jwt"
	usr "graphql-intro/internal/users"
)

func (r *mutationResolver) CreateLink(ctx context.Context, input model.NewLink) (*model.Link, error) {
	user := auth.ForContext(ctx)
	if user == nil {
		return &model.Link{}, fmt.Errorf("access denied")
	}
	link := &model.Link{
		Address: input.Address,
		Title:   input.Title,
		User:    user,
	}
	id := links.Saved(*link)
	log.Println("Link saved with id:", id)
	link.ID = fmt.Sprintf("%d", id)
	return link, nil
}

func (r *mutationResolver) CreateUser(ctx context.Context, input model.NewUser) (string, error) {
	user, err := usr.Create(&input)
	if err != nil {
		log.Println("failed to mutation create user:", err)
		return "", err
	}
	token, err := jwt.GenerateToken(user.Name)
	if err != nil {
		log.Println("failed to mutation create user generate token:", err)
		return "", err
	}
	return token, nil
}

func (r *mutationResolver) Login(ctx context.Context, input model.Login) (string, error) {
	if !usr.Authenticate(input) {
		return "", fmt.Errorf("Incorrect username or password")
	}
	token, err := jwt.GenerateToken(input.Username)
	if err != nil {
		return "", err
	}
	return token, nil
}

func (r *mutationResolver) RefreshToken(ctx context.Context, input model.RefreshTokenInput) (string, error) {
	username, err := jwt.ParseToken(input.Token)
	if err != nil {
		return "", fmt.Errorf("access denied: %s", err.Error())
	}
	token, err := jwt.GenerateToken(username)
	if err != nil {
		return "", err
	}
	return token, nil
}

func (r *queryResolver) Links(ctx context.Context) ([]*model.Link, error) {
	var res []*model.Link
	log.Println("query links")
	ch := make(chan *model.Link)
	go links.GetAll(ch)
	for link := range ch {
		log.Println("buffer channel link:", link)
		res = append(res, link)
	}
	return res, nil
}

// Mutation returns generated.MutationResolver implementation.
func (r *Resolver) Mutation() generated.MutationResolver { return &mutationResolver{r} }

// Query returns generated.QueryResolver implementation.
func (r *Resolver) Query() generated.QueryResolver { return &queryResolver{r} }

type mutationResolver struct{ *Resolver }
type queryResolver struct{ *Resolver }
