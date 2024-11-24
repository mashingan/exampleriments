package main

import (
	"database/sql"
	"log/slog"

	ora "github.com/sijms/go-ora/v2"
)

func main() {
	const port = 1521
	// connStr := ora.BuildUrl("localhost", port, "xepdb1", "ot", "orcl1234", nil)
	connStr := ora.BuildUrl("localhost", port, "xepdb1", "rdruffy", "yffurdr", nil)
	slog.Info("", "connStr", connStr)
	db, err := sql.Open("oracle", connStr)
	if err != nil {
		slog.Error(err.Error())
	}
	defer db.Close()
	err = db.Ping()
	if err != nil {
		slog.Error(err.Error())
	} else {
		slog.Info(connStr + ": connected")
	}
	const q = `select count(*) from contacts`
	var count int64
	if err = db.QueryRow(q).Scan(&count); err != nil {
		slog.Error(err.Error())
	}
	slog.Info("", "count", count)
}
