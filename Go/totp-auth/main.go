package main

import (
	"bufio"
	"bytes"
	"encoding/base32"
	"fmt"
	"image/png"
	"log/slog"
	"net"
	"os"
	"time"

	"github.com/pquerna/otp"
	"github.com/pquerna/otp/totp"
	"github.com/yeqown/go-qrcode/v2"
	"github.com/yeqown/go-qrcode/writer/standard"
)

func generateQr(target, msg string) error {
	qrc, err := qrcode.New(msg)
	if err != nil {
		err = fmt.Errorf("could not generate QRCode: %v", err)
		return err
	}

	w, err := standard.New(target)
	if err != nil {
		err = fmt.Errorf("standard.New failed: %v", err)
		return err
	}

	// save file
	if err = qrc.Save(w); err != nil {
		err = fmt.Errorf("could not save image: %v", err)
		return err
	}
	return nil

}

func skipLoopbackAndOffline(iface net.Interface) bool {
	return (iface.Flags&net.FlagUp == 0) && (iface.Flags&net.FlagLoopback != 0)
}

func retrieveIp(addr net.Addr) (ipstring string, error error) {
	var ip net.IP
	switch v := addr.(type) {
	case *net.IPNet:
		ip = v.IP
	case *net.IPAddr:
		ip = v.IP
	}
	if ip == nil || ip.IsLoopback() {
		return
	}
	ip = ip.To4()
	if ip == nil {
		return
	}
	ipstring = ip.String()
	return

}

func externalIP() (string, error) {
	ifaces, err := net.Interfaces()
	if err != nil {
		return "", err
	}
	for _, iface := range ifaces {
		addrs, err := iface.Addrs()
		if err != nil {
			return "", err
		}
		if skipLoopbackAndOffline(iface) {
			continue
		}
		for _, addr := range addrs {
			ipstring, err := retrieveIp(addr)
			if ipstring == "" || err != nil {
				continue
			}
			return ipstring, err
		}
	}
	return "", fmt.Errorf("are you connected to the network?")
}

func display(key *otp.Key, data []byte) {
	fmt.Printf("Issuer:       %s\n", key.Issuer())
	fmt.Printf("Account Name: %s\n", key.AccountName())
	fmt.Printf("Secret:       %s\n", key.Secret())
	fmt.Printf("Origin URL:   %s\n", key.URL())
	fmt.Println("Writing PNG to qr-code.png....")
	os.WriteFile("qr-code.png", data, 0644)
	fmt.Println("")
	fmt.Println("Please add your TOTP to your OTP Application now!")
	fmt.Println("")
}

func promptForPasscode() string {
	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Enter Passcode: ")
	text, _ := reader.ReadString('\n')
	return text
}

// Demo function, not used in main
// Generates Passcode using a UTF-8 (not base32) secret and custom parameters
func GeneratePassCode(utf8string string) string {
	secret := base32.StdEncoding.EncodeToString([]byte(utf8string))
	passcode, err := totp.GenerateCodeCustom(secret, time.Now(), totp.ValidateOpts{
		Period:    30,
		Skew:      1,
		Digits:    otp.DigitsSix,
		Algorithm: otp.AlgorithmSHA512,
	})
	if err != nil {
		panic(err)
	}
	return passcode
}

func main() {
	slog.SetDefault(slog.New(slog.NewJSONHandler(os.Stdout, nil)))
	ip, err := externalIP()
	if err != nil {
		slog.Error(err.Error())
		return
	}
	slog.Info("local ip address", "ip", ip)
	key, err := totp.Generate(totp.GenerateOpts{
		Issuer:      "Example.com",
		AccountName: "alice@example.com",
	})
	if err != nil {
		slog.Error(err.Error())
		return
	}
	// Convert TOTP key into a PNG
	var buf bytes.Buffer
	img, err := key.Image(200, 200)
	if err != nil {
		slog.Error(err.Error())
		return
	}
	png.Encode(&buf, img)

	// display the QR code to the user.
	display(key, buf.Bytes())

	// Now Validate that the user's successfully added the passcode.
	fmt.Println("Validating TOTP...")
	passcode := promptForPasscode()
	valid := totp.Validate(passcode, key.Secret())
	if valid {
		println("Valid passcode!")
		os.Exit(0)
	} else {
		println("Invalid passcode!")
		os.Exit(1)
	}
}
