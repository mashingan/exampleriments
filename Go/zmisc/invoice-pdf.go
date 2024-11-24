package main

import (
	"bytes"
	"github.com/boombuler/barcode/ean"
	gp "github.com/jung-kurt/gofpdf"
	"image/jpeg"
	"io"
)

const eancode = "55123457"

func main() {
	pdf := gp.NewCustom(&gp.InitType{
		UnitStr:        "cm",
		Size:           gp.SizeType{Wd: 2.0, Ht: 5.0},
		OrientationStr: "Portrait",
	})
	bar, err := ean.Encode(eancode)
	if err != nil {
		panic(err)
	}
	// need this because gofpdf requires io.reader but jpeg.Encode
	// requires io.writer
	r, w := io.Pipe()
	go func() {
		jpeg.Encode(w, bar, nil)
		w.Close()
	}()
	buf := new(bytes.Buffer)
	buf.ReadFrom(r)
	pdf.AddPage()
	pdf.SetFont("Helvetica", "", 3)
	pdf.SetX(0.25)
	pdf.CellFormat(1.0, 0.75, eancode, "", 0, "LM", false, 0, "")
	opt := gp.ImageOptions{ImageType: "jpg"}
	_ = pdf.RegisterImageOptionsReader("barcode", opt, buf)
	if err != nil {
		panic(err)
	}
	pdf.ImageOptions("barcode", 0.25, 0.75, 1.0, 0.5, false, opt, 0, "")
	err = pdf.OutputFileAndClose("basic-go.pdf")
	if err != nil {
		panic(err)
	}
}
