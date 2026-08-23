package main

import (
	"database/sql"
	"fmt"
	"log/slog"
	"maps"
	"strings"
	"time"

	"github.com/mashingan/smapping"
	_ "github.com/mattn/go-sqlite3"
	ora "github.com/sijms/go-ora/v2"
)

func main() {
	const port = 1521
	const tbname = "dev_migration"
	const dback = "backup.db"
	connStr := ora.BuildUrl("localhost", port, "freepdb1", "dev1", "dev1", nil)
	slog.Info("", "connStr", connStr)
	db, err := sql.Open("oracle", connStr)
	if err != nil {
		slog.Error(err.Error())
		return
	}
	defer db.Close()
	dbs, err := sql.Open("sqlite3", dback)
	if err != nil {
		slog.Error(err.Error())
		return
	}
	defer dbs.Close()
	err = db.Ping()
	if err != nil {
		slog.Error(err.Error())
		return
	} else {
		slog.Info(connStr + ": connected")
	}
	q := fmt.Sprintf(`select count(*) from %s`, tbname)
	var count int64
	if err = db.QueryRow(q).Scan(&count); err != nil {
		slog.Error(err.Error())
	}
	slog.Info("", "count", count)

	qselect := fmt.Sprintf("select * from %s", tbname)
	rows, err := db.Query(qselect)
	if err != nil {
		slog.Error(err.Error())
		return
	}
	defer rows.Close()
	cts, err := rows.ColumnTypes()
	if err != nil {
		slog.Error(err.Error())
		return
	}

	ks := make([]string, 0, 20)
	tys := make([]string, 0, 20)

	mdata := make(map[string]any)
	for _, ct := range cts {
		typ := ct.DatabaseTypeName()
		cn := ct.Name()
		ks = append(ks, cn)
		var dval any
		cnull, nullok := ct.Nullable()
		typlower := strings.ToLower(typ)
		valtype := "TEXT"
		if strings.Contains(typlower, "char") || strings.Contains(typlower, "raw") {
			dval = ""
			if cnull && nullok {
				dval = sql.NullString{}
			}
		} else if typ == "NUMBER" {
			valtype = "INTEGER"
			dval = 0
			if cnull && nullok {
				dval = sql.Null[int]{}
			}
		} else if strings.Contains(typlower, "timestamp") {
			valtype = "TEXT"
			dval = time.Time{}
			if cnull && nullok {
				dval = sql.Null[time.Time]{}
			}
		}
		modifier := ""
		if cn == "ID" {
			modifier = "PRIMARY KEY"
		}
		tys = append(tys, fmt.Sprintf("%s %s %s", cn, valtype, modifier))
		mdata[cn] = dval
	}
	slog.Info("", "keys", ks)
	slog.Info("", "types", tys)

	fbody := strings.Join(tys, ",")
	qcsq := fmt.Sprintf(`CREATE TABLE IF NOT EXISTS %s ( %s )`, tbname, fbody)
	slog.Info("", "query create table", qcsq)
	res, err := dbs.Exec(qcsq)
	if err != nil {
		slog.Error(err.Error())
		return
	}
	_ = res

	const qinsertbase = `INSERT INTO %s(%s) VALUES(%s)`
	for rows.Next() {
		mr := make(map[string]any)
		maps.Copy(mr, mdata)
		if err = smapping.SQLScan(rows, mr, "", ks...); err != nil {
			slog.Error(err.Error())
			continue
		}
		vs := make([]any, 0, len(mr))
		ks := make([]string, 0, len(mr))
		for k, v := range mr {
			slog.Info("rowval", k, fmt.Sprintf("%v", v))
			ks = append(ks, k)
			vs = append(vs, v)
		}
		slog.Info("========end row=======")
		qinsert := fmt.Sprintf(qinsertbase,
			tbname,
			strings.Join(ks, ","),
			strings.Repeat("?,", len(vs)-1)+"?")
		slog.Info("", "query insert", qinsert)
		slog.Info("", "vvs", vs)
		_, err = dbs.Exec(qinsert, vs...)
		if err != nil {
			slog.Error(err.Error())
		}
	}
}
