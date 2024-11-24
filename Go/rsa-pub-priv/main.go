// src:
// https://gist.github.com/devinodaniel/8f9b8a4f31573f428f29ec0e884e6673
package main

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"log"
	"os"
	"sync"

	"golang.org/x/crypto/ssh"
)

func main() {
	const (
		privFilename = "id_tsa_test"
		pubFilename  = "id_resa_test.pub"
		bitsize      = 4069
	)

	privkey, err := generatePrivateKey(bitsize)
	if err != nil {
		log.Fatal(err)
	}

	pubkey, err := generatePublicKey(&privkey.PublicKey)
	if err != nil {
		log.Fatal(err)
	}

	privbytes := encodePrivateKeyToPem(privkey)
	var wg sync.WaitGroup
	wg.Add(2)
	go writeFile(&wg, privFilename, privbytes)
	go writeFile(&wg, pubFilename, pubkey)
	wg.Wait()

}

func generatePrivateKey(bitsize int) (*rsa.PrivateKey, error) {
	priv, err := rsa.GenerateKey(rand.Reader, bitsize)
	if err != nil {
		return nil, err
	}

	if err = priv.Validate(); err != nil {
		return nil, err
	}

	return priv, nil
}

func generatePublicKey(privkey *rsa.PublicKey) ([]byte, error) {
	pubrsa, err := ssh.NewPublicKey(privkey)
	if err != nil {
		return nil, nil
	}

	pubytes := ssh.MarshalAuthorizedKey(pubrsa)
	return pubytes, nil
}

func encodePrivateKeyToPem(priv *rsa.PrivateKey) []byte {
	// Get ASN.1 DER format
	privDER := x509.MarshalPKCS1PrivateKey(priv)

	// PEM.Block
	privblock := pem.Block{
		Type:    "RSA PRIVATE KEY",
		Headers: nil, Bytes: privDER,
	}
	privPEM := pem.EncodeToMemory(&privblock)
	return privPEM
}

func writeFile(w *sync.WaitGroup, fname string, data []byte) {
	defer w.Done()
	if err := os.WriteFile(fname, data, 0755); err != nil {
		log.Println(err)
	}
}
