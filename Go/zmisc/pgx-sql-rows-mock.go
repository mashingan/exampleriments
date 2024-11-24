package main

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"fmt"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgproto3"
	"github.com/jackc/v4/pgx"
)

type rowsmock struct {
	rows     *sql.Rows
	scanFail bool
}

func (r *rowsmock) Close()                              { r.rows.Close() }
func (r *rowsmock) Err() error                          { return r.rows.Err() }
func (r *rowsmock) CommandTag() (cmd pgconn.CommandTag) { return }
func (r *rowsmock) FieldDescriptions() (
	list []pgproto3.FieldDescription) {
	return
}
func (r *rowsmock) Next() bool { return r.rows.Next() }
func (r *rowsmock) Scan(dest ...interface{}) error {
	if r.scanFail {
		return fmt.Errorf("planned fail")
	}
	return r.rows.Scan(dest...)
}
func (r *rowsmock) Values() ([]interface{}, error) { return nil, nil }
func (r *rowsmock) RawValues() [][]byte            { return [][]byte{nil} }

type dbmock struct {
	db       *sql.DB
	scanFail bool
}

func (d *dbmock) Query(ctx context.Context, query string,
	values ...interface{}) (pgx.Rows, error) {
	rows, err := d.db.QueryContext(ctx, query, values...)
	if err != nil {
		return nil, err
	}
	rm := &rowsmock{rows: rows, scanFail: d.scanFail}
	return rm, nil
}

func (d *dbmock) Exec(ctx context.Context, query string,
	values ...interface{}) (driver.Result, error) {
	return d.db.ExecContext(ctx, query, values...)
}
