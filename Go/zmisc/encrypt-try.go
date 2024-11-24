package main

import (
    "crypto/aes"
    "crypto/cipher"
    "crypto/rand"
    "fmt"
    "io"
    "io/ioutil"
    "log"
)

func main() {
    fmt.Println("program encryption v1.0")

    fmt.Print("masukan text flag :")
    var inputText string
    fmt.Scanln(&inputText)

    fmt.Print("masukan key encryption :")
    var inputKey string
    fmt.Scanln(&inputKey)

    fmt.Print("masukan nama file :")
    var inputFile string
    fmt.Scanln(&inputFile)

    inputChip := ([]byte(inputKey))

    c, err := aes.NewCipher(inputChip)

    if err != nil {
        log.Println(err)
    }

    gcm, err := cipher.NewGCM(c)

    if err != nil {
        fmt.Println("error")
    }

    nonce := make([]byte, gcm.NonceSize())

    if _, err := io.ReadFull(rand.Reader, nonce); err != nil {
        fmt.Println("error")
    }

    fmt.Println(gcm.Seal(nonce, nonce, []byte(inputText), nil))

    err = ioutil.WriteFile(inputFile, gcm.Seal(nonce, nonce, []byte(inputText), nil), 0777)
    if err != nil {
        fmt.Println("error")
    }
}
