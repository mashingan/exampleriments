package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strings"
	"time"

	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	"github.com/mashingan/smapping"
	"golang.org/x/crypto/bcrypt"
)

func addAppRoutes(route *mux.Router) {
	log.Println("Loading routes...")
	route.HandleFunc("/signin", signInUser).Methods("POST")
	route.HandleFunc("/signup", signUpUser).Methods("POST")
	route.HandleFunc("/userDetails", getUserDetails).Methods("GET")
	log.Println("Routes are loaded.")
}

func writeJSON(w http.ResponseWriter, x interface{}) {
	raw, _ := json.Marshal(x)
	w.Write(raw)
}

func signInUser(response http.ResponseWriter, request *http.Request) {
	var loginReq params
	var result params
	var errResp = errorResponse{
		Code: http.StatusBadRequest,
	}
	response.Header().Set("Content-Type", "application/json")
	rawquery, _ := ioutil.ReadAll(request.Body)
	defer request.Body.Close()
	if err := json.Unmarshal(rawquery, &loginReq); err != nil {
		code := http.StatusBadRequest
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	if loginReq.Email == "" || loginReq.Password == "" {
		errResp.Message = "Email or passowrd cannot be empty"
		response.WriteHeader(errResp.Code)
		writeJSON(response, errResp)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 2*time.Second)
	defer cancel()
	/*
		pass, err := bcrypt.GenerateFromPassword([]byte(loginReq.Password), 0)
		if err != nil {
			errResp.Code = http.StatusInternalServerError
			errResp.Message = fmt.Sprintf("Password generator: %s", err.Error())
			response.WriteHeader(errResp.Code)
			writeJSON(response, errResp)
			return
		}
	*/
	hashpash := []byte{}
	if err := db.QueryRowContext(ctx, "SELECT name, email, pass FROM users WHERE email = ?",
		loginReq.Email).Scan(&result.Name, &result.Email, &hashpash); err != nil {
		errResp.Code = http.StatusUnauthorized
		errResp.Message = fmt.Sprintf("No email %s or password incorrect", loginReq.Email)
		response.WriteHeader(errResp.Code)
		writeJSON(response, errResp)
		return
	}
	if err := bcrypt.CompareHashAndPassword(hashpash, []byte(loginReq.Password)); err != nil {
		code := http.StatusUnauthorized
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	token, _ := createJwt(loginReq.Email)
	if token == "" {
		errResp.Code = http.StatusInternalServerError
		errResp.Message = "Something happened when creating token"
		response.WriteHeader(errResp.Code)
		writeJSON(response, errResp)
		return
	}
	succ := successResponse{
		Code:    http.StatusOK,
		Message: "success",
	}
	data := params{
		Name:     result.Name,
		Email:    result.Email,
		Password: token,
	}
	succ.Response = smapping.MapTags(&data, "loginSuccess")
	response.WriteHeader(succ.Code)
	writeJSON(response, succ)
	return
}

func signUpUser(response http.ResponseWriter, request *http.Request) {
	var regReq params
	//var result params
	var errResp = errorResponse{
		Code: http.StatusBadRequest,
	}
	response.Header().Set("Content-Type", "application/json")
	rawquery, _ := ioutil.ReadAll(request.Body)
	defer request.Body.Close()
	if err := json.Unmarshal(rawquery, &regReq); err != nil {
		code := http.StatusBadRequest
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	if regReq.Email == "" || regReq.Name == "" || regReq.Password == "" {
		errResp.Message = "Name, email or passowrd cannot be empty"
		response.WriteHeader(errResp.Code)
		writeJSON(response, errResp)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 2*time.Second)
	defer cancel()
	token, _ := createJwt(regReq.Email)
	if token == "" {
		code := http.StatusInternalServerError
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, "Cannot creating token"})
		return
	}
	pass, err := bcrypt.GenerateFromPassword([]byte(regReq.Password), 0)
	if err != nil {
		code := http.StatusInternalServerError
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	insQ := `INSERT INTO users (name, email, pass) VALUES (?, ?, ?) `
	if _, err := db.ExecContext(ctx, insQ, regReq.Name, regReq.Email, pass); err != nil {
		code := http.StatusInternalServerError
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	succ := successResponse{
		Code:    http.StatusOK,
		Message: "success",
	}
	data := params{
		Name:     regReq.Name,
		Email:    regReq.Email,
		Password: token,
	}
	succ.Response = smapping.MapTags(&data, "loginSuccess")
	response.WriteHeader(succ.Code)
	writeJSON(response, succ)
	return
}

func getUserDetails(response http.ResponseWriter, request *http.Request) {
	var user params
	response.Header().Set("Content-Type", "application/json")
	bearer := request.Header.Get("Authorization")
	if bearer == "" {
		code := http.StatusUnauthorized
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, "Not authorized, no token provided"})
		return
	}
	authToken := strings.Split(bearer, " ")
	if len(authToken) < 2 {
		code := http.StatusBadRequest
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, "Bad token request"})
		return
	}
	email, err := verifyToken(authToken[1])
	if err != nil {
		validErr, ok := err.(jwt.ValidationError)
		var code int
		if !ok {
			code = http.StatusInternalServerError

		} else if validErr.Errors == jwt.ValidationErrorExpired {
			code = http.StatusUnauthorized
		}
		log.Println("validation err code:", validErr.Errors)
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	if email == "" {
		code := http.StatusForbidden
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, "forbidden, token is invalid"})
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 2*time.Second)
	defer cancel()
	if err := db.QueryRowContext(ctx,
		"select name, email from users where email = ?", email).Scan(
		&user.Name, &user.Email); err != nil {
		if err == sql.ErrNoRows {
			code := http.StatusNotFound
			response.WriteHeader(code)
			msg := fmt.Sprintf("No user found for email %s", email)
			writeJSON(response, errorResponse{code, msg})
			return
		}
		code := http.StatusInternalServerError
		response.WriteHeader(code)
		writeJSON(response, errorResponse{code, err.Error()})
		return
	}
	resp := successResponse{
		Code:    http.StatusOK,
		Message: "OK",
	}
	user.Password = authToken[1]
	resp.Response = smapping.MapTagsWithDefault(&user, "loginSuccess", "json")
	response.WriteHeader(resp.Code)
	writeJSON(response, resp)
	return
}
