package main

import (
	"time"

	"github.com/dgrijalva/jwt-go"
)

var jwtSecretKey = []byte("jwt_secret_key")

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
