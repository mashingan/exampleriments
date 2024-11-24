package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"time"

	"github.com/gin-gonic/gin"
)

type captchaRequest struct {
	Secret   string `json:"secret"`
	Response string `json:"response"`
	RemoteIP string `json:"remote-ip"`
}

type captchaResponse struct {
	Success     bool      `json:"success"`
	ChallengeTS time.Time `json:"challenge_ts"`
	Hostname    string    `json:"hostname"`
	ErrorCodes  []string  `json:"error-codes"`
}

type grecaptchaResponse struct {
	Response string `form:"g-recaptcha-response"`
}

const siteverify = "https://www.google.com/recaptcha/api/siteverify"

func captcha(c *gin.Context) {
	var gcresp grecaptchaResponse
	/*
		//if err := c.ShouldBindJSON(&gcresp); err != nil {
		if err := c.ShouldBindQuery(&gcresp); err != nil {
			log.Println(err)
			c.JSON(http.StatusBadRequest, gin.H{
				"message": "Error bad request:" + err.Error(),
			})
			return
		}
	*/
	gcresp.Response = c.PostForm("g-recaptcha-response")
	log.Println("gcresp:", gcresp)
	creq := captchaRequest{
		Secret:   "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
		Response: gcresp.Response,
	}
	log.Println("creq:", creq)
	creqjson, err := json.Marshal(creq)
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusInternalServerError, gin.H{
			"message": "Error:" + err.Error(),
		})
		return
	}
	log.Println("creqjson:", string(creqjson))
	v := url.Values{}
	v.Set("secret", creq.Secret)
	v.Set("response", creq.Response)
	resp, err := http.PostForm(siteverify, v)
	/*
		resp, err := http.Post("https://www.google.com/recaptcha/api/siteverify",
			"application/json", bytes.NewBuffer(creqjson))
	*/
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusInternalServerError, gin.H{
			"message": "Error post verify: " + err.Error(),
		})
		return
	}
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)
	cresp := captchaResponse{}
	if err = json.Unmarshal(body, &cresp); err != nil {
		log.Println(err)
		c.JSON(resp.StatusCode, gin.H{
			"message": "err unmarshal:" + err.Error(),
		})
		return

	}
	if resp.StatusCode >= http.StatusBadRequest || !cresp.Success {
		c.JSON(resp.StatusCode, gin.H{
			"message": cresp.ErrorCodes,
		})
		return
	}
	log.Println("challenge_ts:", cresp.ChallengeTS.Format(time.RFC3339))

	c.JSON(http.StatusOK, gin.H{
		"message": "ok",
	})
}

func cors() gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Writer.Header().Set("Access-Control-Allow-Origin", "*")
		c.Writer.Header().Set("Access-Control-Allow-Credentials", "true")
		//c.Writer.Header().Set("Access-Control-Allow-Headers", "Content-Type, Content-Length, Accept-Encoding, X-CSRF-Token, Authorization, accept, origin, Cache-Control, X-Requested-With")
		c.Writer.Header().Set("Access-Control-Allow-Headers", "*")
		c.Writer.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, GET, PUT, DELETE")

		if c.Request.Method == "OPTIONS" {
			c.AbortWithStatus(204)
			return
		}
		c.Next()
	}
}

/*
URL: https://www.google.com/recaptcha/api/siteverify
POST:
{
	secret: required, secret-key,
	response: required, response-fe,
	remote-ip: optional
}
{
  "success": true|false,
  "challenge_ts": timestamp,  // timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
  "hostname": string,         // the hostname of the site where the reCAPTCHA was solved
  "error-codes": [...]        // optional
}
*/

func main() {
	r := gin.New()
	r.Use(cors())
	r.StaticFile("/", "./index.html")
	r.StaticFile("/index.html", "./index.html")
	r.POST("/validate-recaptcha", captcha)
	log.Fatal(r.Run(":3000"))
}
