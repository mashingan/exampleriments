package auth

import (
	"context"
	"log"
	"net/http"
	"strconv"

	"graphql-intro/graph/model"
	"graphql-intro/internal/pkg/jwt"
	usr "graphql-intro/internal/users"
)

var userCtxKey = &contextKey{"uesr"}

type contextKey struct {
	name string
}

// Middleware is authorization interceptor for checking jwt token
//func Middleware() func(http.Handler) http.Handler {
func Middleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		tokenStr := r.Header.Get("Authorization")

		// allow unauthorized request in
		if tokenStr == "" {
			next.ServeHTTP(w, r)
			return
		}

		username, err := jwt.ParseToken(tokenStr)
		if err != nil {
			log.Println("Parse token err:", err)
			//http.Error(w, "Invalid token", http.StatusForbidden)
			next.ServeHTTP(w, r)
			return
		}

		user := model.User{Name: username}
		id, err := usr.GetUserIDByUsername(username)
		if err != nil {
			log.Println("get user id err:", err)
			next.ServeHTTP(w, r)
			return
		}
		user.ID = strconv.Itoa(id)
		ctx := context.WithValue(r.Context(), userCtxKey, &user)

		r = r.WithContext(ctx)
		next.ServeHTTP(w, r)
	})
}

// ForContext finds the user from the context. Requires MIddleware to run.
func ForContext(ctx context.Context) *model.User {
	raw, _ := ctx.Value(userCtxKey).(*model.User)
	return raw
}
