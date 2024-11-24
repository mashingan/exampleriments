package main

import (
	"github.com/dgrijalva/jwt-go"
)

type errorResponse struct {
	Code    int
	Message string
}

type successResponse struct {
	Code     int
	Message  string
	Response interface{}
}

type claims struct {
	Email string
	jwt.StandardClaims
}

type params struct {
	Name     string `json:"name"`
	Email    string `json:"email" login:"email" loginSuccess:"email"`
	Password string `json:"password" login:"password" loginSuccess:"authToken"`
}
