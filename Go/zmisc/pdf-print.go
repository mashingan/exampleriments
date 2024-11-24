package main

import (
	/*
		"bufio"
		"log"
		"os"
		"strings"
	*/

	"github.com/signintech/gopdf"
)

func main() {

	pdf := gopdf.GoPdf{}
	pdf.Start(gopdf.Config{PageSize: *gopdf.PageSizeA4})
	pdf.AddPage()
	//buff := bufio.NewWriter(os.Stdout)
	/*
		err := pdf.AddTTFFont("wts11", "../ttf/wts11.ttf")
		if err != nil {
			log.Print(err.Error())
			return
		}

		err = pdf.SetFont("wts11", "", 14)
		if err != nil {
			log.Print(err.Error())
			return
		}
	*/
	pdf.Cell(nil, "您好")
	pdf.WritePdf("hello.pdf")
	//pdf.Write(buff)
	//log.Println(buff)
}
