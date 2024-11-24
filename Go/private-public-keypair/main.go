package main

// reference:
// https://dev.to/elioenaiferrari/asymmetric-cryptography-with-golang-2ffd

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"log"
	"os"
)

func generateKeyPair() (err error) {
	privateKey, err1 := rsa.GenerateKey(rand.Reader, 2048)
	if err1 != nil {
		return err1
	}
	publicKey := &privateKey.PublicKey

	privateKeyBytes := x509.MarshalPKCS1PrivateKey(privateKey)
	privateKeyPem := pem.EncodeToMemory(&pem.Block{
		Type:  "RSA PRIVATE KEY",
		Bytes: privateKeyBytes,
	})

	if err = os.WriteFile("private.pem", privateKeyPem, 0644); err != nil {
		return
	}

	publicKeyBytes, err1 := x509.MarshalPKIXPublicKey(publicKey)
	if err1 != nil {
		return err1
	}
	publicKeyPem := pem.EncodeToMemory(&pem.Block{
		Type:  "RSA PUBLIC KEY",
		Bytes: publicKeyBytes,
	})
	err = os.WriteFile("public.pem", publicKeyPem, 0644)
	return
}

func encode(content string, pubkey []byte) (ciphertext []byte, err error) {
	publicKeyBlock, _ := pem.Decode(pubkey)
	publickey, err1 := x509.ParsePKIXPublicKey(publicKeyBlock.Bytes)
	if err1 != nil {
		err = err1
		return
	}
	ciphertext, err = rsa.EncryptPKCS1v15(rand.Reader, publickey.(*rsa.PublicKey), []byte(content))
	return
}

func decode(ciphertext []byte, privkey []byte) (plaintext []byte, err error) {
	privkeyBlock, _ := pem.Decode(privkey)
	privateKey, err1 := x509.ParsePKCS1PrivateKey(privkeyBlock.Bytes)
	if err1 != nil {
		err = err1
		return
	}
	plaintext, err = rsa.DecryptPKCS1v15(rand.Reader, privateKey, ciphertext)
	return
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	if err := generateKeyPair(); err != nil {
		log.Fatal(err)
	}
	privkeypem, _ := os.ReadFile("private.pem")
	pubkeypem, _ := os.ReadFile("public.pem")
	plaintext := "hello world!"
	ciphertext, err := encode(plaintext, pubkeypem)
	if err != nil {
		log.Fatal(err)
	}
	content, err := decode(ciphertext, privkeypem)
	if err != nil {
		log.Fatal(err)
	}
	log.Printf("plaintext: %s\n", plaintext)
	log.Printf("content: %s\n", content)
	log.Printf("is plaintext == content? %t\n", plaintext == string(content))
}
