package users

import (
	"database/sql"
	"fmt"
	"log"

	"golang.org/x/crypto/bcrypt"

	"graphql-intro/graph/model"
	"graphql-intro/internal/pkg/db/migrations/database"
)

// User will be used to identify the link submitter for hacker news
/*
type User struct {
	ID       string `json:"id"`
	Username string `json:"username"`
	Password string `json:"password"`
}
*/

// Create new user which receive username and password and
// returns the user info which supply id and name
func Create(user *model.NewUser) (*model.User, error) {
	statement, err := database.Db.Prepare("INSERT INTO Users (Username, Password) VALUES(?, ?)")
	if err != nil {
		log.Fatal(err)
		return nil, err
	}
	//hashedPassword, err := bcrypt.HashPassword(user.Password)
	hashedPassword, err := HashPassword(user.Password)
	if err != nil {
		log.Println("failed to hash password:", err)
		return nil, err
	}
	res, err := statement.Exec(user.Username, hashedPassword)
	if err != nil {
		log.Println("failed to create user:", err)
		return nil, err
	}
	id, err := res.LastInsertId()
	userRet := &model.User{
		Name: user.Username,
	}
	if err != nil {
		log.Println("last insert id err:", err)
		userRet.ID = "-1"
	} else {
		userRet.ID = fmt.Sprintf("%d", id)
	}
	return userRet, nil
}

// HashPassword is hashing using bcrypt.
func HashPassword(pass string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(pass), 14)
	return string(bytes), err
}

// CheckPasswordHash is to check whether the hash of supplied password
// and the saved hash is same.
func CheckPasswordHash(pass, hash string) bool {
	err := bcrypt.CompareHashAndPassword([]byte(hash), []byte(pass))
	if err != nil {
		log.Println("password hash not same:", err)
		return false
	}
	return true
}

// GetUserIDByUsername is fetching the ID by supplying the username.
func GetUserIDByUsername(username string) (int, error) {
	id := -1
	err := database.Db.QueryRow("Select ID from Users where Username = ?", username).Scan(&id)
	if err != nil {
		log.Printf("cannot fetch user %s err: %s", username, err.Error())
		return -1, err
	}
	return id, nil
}

// Authenticate user login whether the password is same.
func Authenticate(login model.Login) bool {
	hashpass := ""
	err := database.Db.
		QueryRow("select Password from Users WHERE Username = ?", login.Username).
		Scan(&hashpass)
	if err != nil {
		if err == sql.ErrNoRows {
			log.Printf("no available users %s", login.Username)
			return false
		}
		log.Println("retrieve hash error:", err)
		return false
	}
	return CheckPasswordHash(login.Password, hashpass)
}
