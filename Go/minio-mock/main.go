package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/gofiber/fiber/v2"
	"github.com/minio/minio-go/v7"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	mc, err := minio.New("localhost", &minio.Options{
		Transport: &miniomock{},
		Region:    "eheh",
	})
	os.MkdirAll("files", 0777)
	if err != nil {
		log.Fatal(err)
	}
	app := fiber.New()
	app.Post("/", func(c *fiber.Ctx) error {
		filename := c.FormValue("filename")
		fileh, err := c.FormFile("document")
		if err != nil {
			return err
		}
		file, err := fileh.Open()
		if err != nil {
			return err
		}
		upinfo, err := mc.PutObject(c.UserContext(), "files", filename, file, fileh.Size, minio.PutObjectOptions{})
		if err != nil {
			return err
		}
		log.Println(upinfo)
		return c.Status(fiber.StatusOK).JSON(upinfo)
	})
	app.Get("/", func(c *fiber.Ctx) error {
		fname := c.Query("filename")
		if fname == "" {
			return fmt.Errorf("fname cannot be empty")
		}
		obj, err := mc.GetObject(c.Context(), "files", fname,
			minio.GetObjectOptions{})
		if err != nil {
			log.Println(err)
			return err
		}
		stat, err := obj.Stat()
		if err != nil {
			log.Println(err)
			return err
		}
		mt := mime.TypeByExtension(filepath.Ext(fname))
		c.Set("content-type", mt)
		c.Set("content-disposition", fmt.Sprintf(`attachment;filename="%s"`, fname))
		log.Printf("fname: %s, mimetype: %s, size: %d\n", fname, mt, stat.Size)
		return c.Status(fiber.StatusOK).SendStream(obj, int(stat.Size))
	})
	log.Println(app.Listen(":3000"))
}

type (
	Response[Data any] struct {
		Message string `json:"message"`
		Data    Data   `json:"data,omitempty"`
	}
	miniomock struct{}
)

func prepareError(res *http.Response, errmsg string) {
	res.Body = io.NopCloser(bytes.NewBufferString(errmsg))
	res.ContentLength = int64(len(errmsg))
	res.Status = http.StatusText(http.StatusInternalServerError)
	res.StatusCode = http.StatusInternalServerError
}

func (mm *miniomock) RoundTrip(req *http.Request) (*http.Response, error) {
	const ok = "OK"
	res := &http.Response{
		Proto:         "HTTP/1.1",
		ProtoMajor:    1,
		ProtoMinor:    1,
		Request:       req,
		Body:          io.NopCloser(bytes.NewBufferString(ok)),
		ContentLength: 2,
		Status:        http.StatusText(http.StatusOK),
		StatusCode:    http.StatusOK,
	}
	log.Printf("req.path: %s\n", req.URL.Path)
	switch req.Method {
	case http.MethodPut:
		if err := saveFile(req); err != nil {
			log.Println(err)
			prepareError(res, err.Error())
			return res, nil
		}
		return res, nil
	case http.MethodGet, http.MethodHead:
		f, err := os.Open("." + req.URL.Path)
		if err != nil {
			log.Println(err)
			prepareError(res, err.Error())
			return res, nil
		}
		if req.Method == http.MethodGet {
			res.Body = f
		}
		res.Header = make(http.Header)
		stat, err := f.Stat()
		if err != nil {
			log.Println(err)
			prepareError(res, err.Error())
			return res, nil
		}
		_, fname := filepath.Split(req.URL.Path)
		res.Header.Add("content-disposition",
			fmt.Sprintf(`attachment; filename="%s"`, fname))
		modformat := "Mon, 2 Jan 2006 15:04:05 GMT"
		res.Header.Add("last-modified",
			stat.ModTime().In(time.UTC).Format(modformat))
		res.ContentLength = stat.Size()
		log.Printf("fname: %s, mimetype: %s, size: %d\n", fname, "", stat.Size())
		return res, nil
	default:
	}
	return res, nil
}

func saveFile(req *http.Request) error {
	f, err := os.Create("." + req.URL.Path)
	if err != nil {
		log.Println(err)
		return err
	}
	defer req.Body.Close()
	defer f.Close()
	n := int64(0)
	for n < req.ContentLength {
		nw, err1 := io.Copy(f, req.Body)
		if err1 != nil && err1 != io.EOF {
			log.Println(err1)
			return err1
		}
		if nw == 0 || err1 == io.EOF {
			break
		}
		n += nw
		log.Printf("current write length: %d\n", n)
	}
	return nil
}

func respmsg(msg string) Response[struct{}] {
	return Response[struct{}]{
		Message: msg,
		Data:    struct{}{},
	}
}
