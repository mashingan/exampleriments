package main

import (
	"archive/zip"
	"bytes"
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

type entry struct {
	name string
	rc   io.ReadCloser
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		runGui()
		return
	}
	fname := args[0]
	if err := clean(fname); err != nil {
		log.Fatal(err)
	}
}

func clean(fname string) error {
	repub, err := zip.OpenReader(fname)
	if err != nil {
		log.Println(err)
		return err
	}
	defer repub.Close()
	dir, fpath := filepath.Split(fname)
	ext := filepath.Ext(fname)
	thef := strings.Replace(fpath, ext, "", -1)
	copye, err := os.Create(filepath.Join(dir, thef+"_cleaned"+ext))
	if err != nil {
		log.Println(err)
		return err
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
	ch := make(chan entry, 10)
	done := make(chan struct{}, 1)
	go func() {
		for entry := range ch {
			fw, err := wepub.Create(entry.name)
			if err != nil {
				log.Println(err)
				continue
			}
			if _, err := io.Copy(fw, entry.rc); err != nil {
				log.Println(err)
			}
		}
		done <- struct{}{}
	}()
	for _, f := range repub.File {
		fr, err := f.Open()
		if err != nil {
			log.Println(err)
			return err
		}
		ext := filepath.Ext(f.Name)
		if strings.HasSuffix(strings.ToLower(ext), "html") {
			wg.Add(1)
			go processXhtml(&wg, f.Name, ch, fr)
			continue
		}
		ch <- entry{f.Name, fr}
	}
	wg.Wait()
	close(ch)
	<-done
	return nil
}

func processXhtml(wg *sync.WaitGroup, fname string, ch chan<- entry, r io.ReadCloser) {
	defer func() {
		wg.Done()
		r.Close()
	}()
	decoder := xml.NewDecoder(r)
	decoder.Strict = false
	w := new(bytes.Buffer)
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
				encoder.EncodeToken(t2)
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
			encoder.EncodeToken(v)
		default:
			encoder.EncodeToken(v)
		}
	}
	ch <- entry{fname, io.NopCloser(w)}
}
