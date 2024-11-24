package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"fmt"
)

func main() {
	msg := "lalayeyeye"
	key := "keybla"
	hash := hmac.New(sha256.New, []byte(key))
	hash.Write([]byte(msg))
	fmt.Println(string(hash.Sum(nil)))
}
