package jwt

import (
	"log"
	"time"

	"github.com/dgrijalva/jwt-go"
)

var secret = []byte("secret")

// GenerateToken as its name meaning.
func GenerateToken(username string) (string, error) {
	token := jwt.New(jwt.SigningMethodHS256)
	claims := token.Claims.(jwt.MapClaims)
	claims["username"] = username
	claims["exp"] = time.Now().Add(time.Minute * 5).Unix()
	tokenString, err := token.SignedString(secret)
	if err != nil {
		log.Fatal("Error in generating key:", err)
		return "", err
	}
	return tokenString, nil
}

// ParseToken is parsing the jwt token.
func ParseToken(tokenStr string) (string, error) {
	token, err := jwt.Parse(tokenStr, func(token *jwt.Token) (interface{}, error) {
		return secret, nil
	})
	if claims, ok := token.Claims.(jwt.MapClaims); ok && token.Valid {
		username := claims["username"].(string)
		return username, nil
	}
	return "", err
}
