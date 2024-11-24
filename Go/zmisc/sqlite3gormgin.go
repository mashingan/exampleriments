package main

import (
	"log"
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

type User struct {
	gorm.Model `json:"model"`
	Name       string `json:"name"`
	Email      string `json:"email"`
}

func handlerFunc(msg string) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.String(http.StatusOK, msg)
	}
}

func allUsers(db *gorm.DB) gin.HandlerFunc {
	return func(c *gin.Context) {
		var users []User
		db.Find(&users)
		log.Println("{}", users)
		c.JSON(http.StatusOK, gin.H{
			"users": users,
		})
	}
}

func newUser(db *gorm.DB) gin.HandlerFunc {
	return func(c *gin.Context) {
		name := c.Param("name")
		email := c.Param("email")
		db.Create(&User{Name: name, Email: email})
		c.String(http.StatusOK, name+" user succesfully entered")
	}
}

func deleteUser(db *gorm.DB) gin.HandlerFunc {
	return func(c *gin.Context) {
		name := c.Param("name")
		var user User
		db.Where("name = ?", name).Find(&user)
		db.Delete(&user)
		c.String(http.StatusOK, name+" user successfully deleted")
	}
}

func updateUser(db *gorm.DB) gin.HandlerFunc {
	return func(c *gin.Context) {
		name := c.Param("name")
		email := c.Param("email")
		var user User
		db.Where("name = ?", name).Find(&user)
		user.Email = email
		db.Save(&user)
		c.String(http.StatusOK, name+" user successfully update email to "+email)
	}
}

func usersByPage(db *gorm.DB) gin.HandlerFunc {
	return func(c *gin.Context) {
		limit, err := strconv.Atoi(c.DefaultQuery("limit", "5"))
		if err != nil {
			c.String(http.StatusBadRequest, err.Error())
			return
		}
		page, err := strconv.Atoi(c.DefaultQuery("page", "1"))
		if err != nil {
			c.String(http.StatusBadRequest, err.Error())
			return
		}
		var result []User
		db.Limit(limit).Offset(limit * (page - 1)).Find(&result)
		c.JSON(http.StatusOK, gin.H{
			"data": result,
		})
	}
}

func handleRequest(db *gorm.DB) {
	r := gin.Default()

	r.GET("/users", allUsers(db))
	ugroup := r.Group("/user")
	{
		ugroup.GET("", usersByPage(db))
		ugroup.POST("/:name/:email", newUser(db))
		ugroup.DELETE("/:name", deleteUser(db))
		ugroup.PUT("/:name/:email", updateUser(db))
	}

	r.Run(":3000")
}

func initialMigration(db *gorm.DB) {
	db.AutoMigrate(&User{})
}

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	log.Println("Go gin orm tutorials")
	db, err := gorm.Open(sqlite.Open("sqlite3gorm.db"), &gorm.Config{})
	if err != nil {
		log.Fatal(err.Error())
	}
	//defer db.Close()
	initialMigration(db)
	handleRequest(db)
}
