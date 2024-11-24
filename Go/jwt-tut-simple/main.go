package main

import (
	"log"
	"time"

	"github.com/golang-jwt/jwt"
)

var jwtSecretKey = []byte("jwt_secret_key")

type claims struct {
	Email string
	jwt.StandardClaims
}

func createJwt(email string) (string, error) {
	expirationTime := time.Now().Add(5 * time.Minute)
	claims := &claims{
		Email: email,
		StandardClaims: jwt.StandardClaims{
			ExpiresAt: expirationTime.Unix(),
		},
	}
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	tokenStr, err := token.SignedString(jwtSecretKey)
	if err != nil {
		return "", err
	}
	return tokenStr, nil
}

func verifyToken(tokenStr string) (string, error) {
	claims := new(claims)
	token, err := jwt.ParseWithClaims(tokenStr, claims,
		func(token *jwt.Token) (interface{}, error) {
			return jwtSecretKey, nil
		})
	if token != nil {
		return claims.Email, err
	}
	return claims.Email, err
}

type jiuclaims struct {
	Issuer     string
	Expiration string
	Username   string
	Role       string
	jwt.StandardClaims
}

func main() {
	token, err := createJwt("my-email")
	log.Println(token)
	log.Println(err)
	const todecode = "eyJhbGciOiJIUzI1NiJ9.eyJSb2xlIjoiQWRtaW4iLCJJc3N1ZXIiOiJJc3N1ZXIiLCJVc2VybmFtZSI6IkphdmFJblVzZSIsImV4cCI6MTY1MDk4MzU3OSwiaWF0IjoxNjUwOTgzNTc5fQ.VYKquFkDliN84E_iej3CpJxadhpyyeIs0MaBEL1bO5U"
	javainusekey := []byte("javainuse-secret-key")
	jc := new(jiuclaims)
	tkndecode, err := jwt.ParseWithClaims(todecode, jc, func(token *jwt.Token) (interface{}, error) {
		return javainusekey, nil
	})
	log.Printf("is token valid? %t\n", tkndecode.Valid)
	if tkndecode != nil {
		log.Printf("%#v\n", *jc)
		log.Printf("issued at: %v\n", time.Unix(jc.IssuedAt, 0))
		log.Printf("expired at: %v\n", time.Unix(jc.ExpiresAt, 0))
	} else {
		log.Println(err)
	}

}
