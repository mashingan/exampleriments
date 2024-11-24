package main

import (
	"log"

	"github.com/gin-gonic/gin"
	"github.com/mashingan/smapping"
)

type Person struct {
	Name    string `json:"name"`
	Address string `json:"address"`
}

func main() {
	route := gin.Default()
	route.Any("/testing", startPage)
	route.Run(":8085")
}

func startPage(c *gin.Context) {
	person := smapping.Mapped{}
	newperson := Person{}
	//if err := c.ShouldBindQuery(&newperson); err != nil {
	//if err := c.BindQuery(&newperson); err != nil {
	if err := c.BindQuery(&person); err != nil {
		log.Println(err)
		c.Abort()
		return
	}
	log.Println("====== Only Bind Query String ======")
	//log.Println(person.Name)
	//log.Println(person.Address)
	log.Println("person:", person)
	log.Println("newperson:", newperson)
	c.String(200, "Success")
}
