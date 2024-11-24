package main

import (
	"database/sql"
	"fmt"
	"io"
	"net/http"
	"syscall"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/gofiber/fiber/v2"
)

func TestMigrate(t *testing.T) {
	db, mock, _ := sqlmock.New()
	mock.ExpectExec(".*$").WillReturnError(fmt.Errorf("error create table"))
	err := migrate(db)
	if err == nil {
		t.Error("expected error create table, got nil")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 1))
	err = migrate(db)
	if err != nil {
		t.Error("expected success, got error:", err)
	}
}

func TestHandleRequest(t *testing.T) {
	app := handleRequest(nil)
	if app == nil {
		t.Error("app should not nil")
	}
}

func TestUsersByPage(t *testing.T) {
	// case bad limit
	app := handleRequest(nil)
	req, _ := http.NewRequest("GET", "/user?limit=a", nil)
	res, _ := app.Test(req, 100)
	body, _ := io.ReadAll(res.Body)
	if res.StatusCode != http.StatusBadRequest {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusBadRequest, res.StatusCode)
	}
	t.Log("body:", string(body))

	// case bad page
	req, _ = http.NewRequest("GET", "/user?page=a", nil)
	res, _ = app.Test(req, 100)
	body, _ = io.ReadAll(res.Body)
	if res.StatusCode != http.StatusBadRequest {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusBadRequest, res.StatusCode)
	}
	t.Log("body:", string(body))

	// case failed query
	db, mock, _ := sqlmock.New()
	app = handleRequest(db)
	mock.ExpectQuery(".*$").WillReturnError(fmt.Errorf("failed query"))
	req, _ = http.NewRequest("GET", "/user", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	// case success with second error failed
	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectQuery(".*$").WillReturnRows(
		sqlmock.NewRows([]string{"id", "name", "email"}).
			AddRow(1, "haha", "hehe").
			AddRow(2, "haha", "hehe").
			RowError(3, fmt.Errorf("row error")).
			AddRow(4, "haha", "hehe"),
	)
	req, _ = http.NewRequest("GET", "/user", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
}

func TestUpdateUser(t *testing.T) {
	app := handleRequest(nil)
	req, _ := http.NewRequest("GET", "/user/edit/test/with-new-email/test@example.com", nil)
	res, _ := app.Test(req, 100)
	bodyres, _ := io.ReadAll(res.Body)
	if res.StatusCode != http.StatusMethodNotAllowed {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusMethodNotAllowed, res.StatusCode)
	}
	t.Log("bodyres:", string(bodyres))

	db, mock, _ := sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnError(fmt.Errorf("failed update"))
	req, _ = http.NewRequest("PUT", "/user/edit/test/with-new-email/test@example.com", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 0))
	req, _ = http.NewRequest("PUT", "/user/edit/test/with-new-email/test@example.com", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 1))
	req, _ = http.NewRequest("PUT", "/user/edit/test/with-new-email/test@example.com", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
}

func TestDeleteuser(t *testing.T) {
	db, mock, _ := sqlmock.New()
	app := handleRequest(db)
	mock.ExpectExec(".*$").WillReturnError(fmt.Errorf("error delete"))
	req, _ := http.NewRequest("DELETE", "/user/remove/test", nil)
	res, _ := app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 0))
	req, _ = http.NewRequest("DELETE", "/user/remove/test", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 1))
	req, _ = http.NewRequest("DELETE", "/user/remove/test", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
}

func TestNewUser(t *testing.T) {
	db, mock, _ := sqlmock.New()
	app := handleRequest(db)
	mock.ExpectExec(".*$").WillReturnError(fmt.Errorf("error insert"))
	req, _ := http.NewRequest("POST", "/user/new/test/test@example.com", nil)
	res, _ := app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 0))
	req, _ = http.NewRequest("POST", "/user/new/test/test@example.com", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 1))
	req, _ = http.NewRequest("POST", "/user/new/test/test@example.com", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
}

func TestAllUsers(t *testing.T) {
	db, mock, _ := sqlmock.New()
	app := handleRequest(db)
	mock.ExpectQuery(".*$").WillReturnError(fmt.Errorf("error insert"))
	req, _ := http.NewRequest("GET", "/users", nil)
	res, _ := app.Test(req, 100)
	if res.StatusCode != http.StatusInternalServerError {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusInternalServerError, res.StatusCode)
	}

	db, mock, _ = sqlmock.New()
	app = handleRequest(db)
	mock.ExpectQuery(".*$").WillReturnRows(
		sqlmock.NewRows([]string{"id", "name", "email"}).
			AddRow(1, "one", "one-email").
			AddRow(2, "two", "two-email").
			RowError(3, fmt.Errorf("error row 3")).
			AddRow(4, "two", "two-email"),
	)
	req, _ = http.NewRequest("GET", "/users", nil)
	res, _ = app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
}

func TestHandlerFunc(t *testing.T) {
	app := fiber.New()
	app.Get("/", handlerFunc("hello"))
	req, _ := http.NewRequest("GET", "/", nil)
	res, _ := app.Test(req, 100)
	if res.StatusCode != http.StatusOK {
		t.Errorf("expected status code '%d' got '%d'",
			http.StatusOK, res.StatusCode)
	}
	resbody, _ := io.ReadAll(res.Body)
	expect := "hello"
	if expect != string(resbody) {
		t.Errorf("expecting '%s' but got '%s'", expect, string(resbody))
	}
}

func TestMain(*testing.T) {
	oriopen := sqlOpen
	db, mock, _ := sqlmock.New()
	sqlOpen = func(string, string) (*sql.DB, error) {
		return db, nil
	}
	mock.ExpectExec(".*$").WillReturnResult(sqlmock.NewResult(1, 1))
	go main()
	time.Sleep(100 * time.Millisecond)
	quit <- syscall.SIGTERM

	sqlOpen = oriopen
}
