package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"time"

	"github.com/secsy/goftp"
	"github.com/yob/graval"
)

type offlineReport struct{}

type mdriver struct{}

func (dr *mdriver) Authenticate(user, pass string) bool {
	return true
}

func (dr *mdriver) Bytes(path string) (bytes int64) {
	file, err := os.Stat(strings.Trim(path, "\\"))
	if err != nil {
		fmt.Println("Bytes err:", err)
		return 0
	}
	return file.Size()
}

func (dr *mdriver) ModifiedTime(path string) (time.Time, error) {
	return time.Now(), nil
}

func (dr *mdriver) ChangeDir(path string) bool {
	return path == "/"
}

func (dr *mdriver) DirContents(path string) (files []os.FileInfo) {
	return
}

func (dr *mdriver) DeleteDir(path string) bool {
	return false
}

func (dr *mdriver) DeleteFile(path string) bool {
	return false
}

func (dr *mdriver) Rename(from, to string) bool {
	return false
}

func (dr *mdriver) MakeDir(path string) bool {
	return true
}

func (dr *mdriver) GetFile(path string) (reader io.ReadCloser, err error) {
	return
}

type mdriverMaker struct{}

func (f *mdriverMaker) NewDriver() (graval.FTPDriver, error) {
	return &mdriver{}, nil
}

func (dr *mdriver) PutFile(dest string, data io.Reader) bool {
	return true
}

func fakeFtpServer(ftpready chan bool) {
	factory := new(mdriverMaker)
	opts := &graval.FTPServerOpts{
		Factory:     factory,
		ServerName:  "Fake ftp server",
		Port:        3001,
		PasvMinPort: 60200,
		PasvMaxPort: 60300,
	}
	ftpServer := graval.NewFTPServer(opts)
	ftpready <- true
	log.Println(ftpServer.ListenAndServe())
}

func main() {
	ftpready := make(chan bool, 1)
	go fakeFtpServer(ftpready)
	<-ftpready

}

func fakeSendToMtrass(report *offlineReport, filename string) (err error) {
	client, e := goftp.Dial("localhost:3001")
	if e != nil {
		//err = ex.Error(e, -255).Rem("Failed to create ftp client")
		err = e

		//err = ex.Error(e, -255).Rem("Failed to dial sftp")
		return
	}

	defer client.Close()

	/*
		sftpClient, e := sftp.NewClient(client)
		if e != nil {
			err = ex.Error(e, -255).Rem("Failed to create sftp client")
			return
		}
	*/

	/*
		fDestination, e := sftpClient.Create("/DATA/CDR/CC/PAGGR/" + filename)
		if e != nil {
			err = ex.Error(e, -255).Rem("Failed creating destination file")
			return
		}
	*/

	fSource, e := os.Open(filename)
	if e != nil {
		err = e
		return
	}

	//_, e = io.Copy(fDestination, fSource)
	fDestination := filename
	e = client.Store(fDestination, fSource)
	if e != nil {
		err = e
		return
	}

	return
}
