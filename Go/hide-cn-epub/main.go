package main

import (
	"archive/zip"
	"encoding/xml"
	"flag"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/rylans/getlang"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		log.Fatalf("Usage: hce <path/where/the/file.epub>")
	}
	fname := args[0]
	repub, err := zip.OpenReader(fname)
	if err != nil {
		log.Fatal(err)
	}
	defer repub.Close()
	dir, fpath := filepath.Split(fname)
	ext := filepath.Ext(fname)
	thef := strings.Replace(fpath, ext, "", -1)
	copye, err := os.Create(filepath.Join(dir, thef+"_cleaned"+ext))
	if err != nil {
		log.Fatal(err)
	}
	defer copye.Close()
	wepub := zip.NewWriter(copye)
	defer func() {
		if err := wepub.Close(); err != nil {
			log.Fatal(err)
		}
	}()
	var (
		wg sync.WaitGroup
	)
	for _, f := range repub.File {
		fw, err := wepub.Create(f.Name)
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}
		fr, err := f.Open()
		if err != nil {
			log.Fatal(err)
		}
		ext := filepath.Ext(f.Name)
		if strings.ToLower(ext) == ".xhtml" {
			wg.Add(1)
			// go processXhtml(&wg, fw, fr)
			processXhtml(&wg, fw, fr)
			continue
		}
		if _, err := io.Copy(fw, fr); err != nil {
			log.Println(err)
		}
	}
	wg.Wait()
}

func processXhtml(wg *sync.WaitGroup, w io.Writer, r io.ReadCloser) {
	defer func() {
		wg.Done()
		r.Close()
	}()
	decoder := xml.NewDecoder(r)
	decoder.Strict = false
	encoder := xml.NewEncoder(w)
	defer encoder.Close()
traversing:
	for {
		token, err := decoder.Token()
		if err != nil {
			if err == io.EOF {
				break traversing
			}
			log.Println(err)
			continue
		}
		switch v := token.(type) {
		case xml.StartElement:
			v.Name.Space = ""
			if v.Name.Local != "p" {
				encoder.EncodeToken(v)
				continue
			}
			t2, err := decoder.Token()
			if err != nil {
				if err == io.EOF {
					break traversing
				}
				log.Println(err)
				continue
			}
			innertext, ok := t2.(xml.CharData)
			if !ok {
				continue
			}
			stext := string(innertext)
			info := getlang.FromString(stext)
			if info.LanguageCode() == "zh" {
				continue
			}
			v.Attr = nil
			encoder.EncodeToken(v)
			encoder.EncodeToken(innertext)
		case xml.EndElement:
			v.Name.Space = ""
			encoder.EncodeToken(v)
		default:
			encoder.EncodeToken(v)
		}
	}
}
