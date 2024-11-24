package main

import (
	"fmt"
	"testing"

	"github.com/DATA-DOG/go-sqlmock"
)

func TestMain(t *testing.T) {
	main()

	s := "builder"
	whichrand = &s
	main()

	s = "pool"
	whichrand = &s
	main()

	cleandbvar := true
	cleandb = &cleandbvar
	main()
}

func TestMigrate(t *testing.T) {
	db, mock, _ := sqlmock.New()

	mock.ExpectExec("create table.*").WillReturnError(fmt.Errorf("error create table"))
	if err := migrate(db); err == nil {
		t.Fatal("expect 'error create table' but got nil")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectExec("create table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("create view.*").WillReturnError(fmt.Errorf("error create view"))
	if err := migrate(db); err == nil {
		t.Fatal("expect 'error create view' but got nil")
	}
	db, mock, _ = sqlmock.New()
	mock.ExpectExec("create table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("create view.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("pragma.*").WillReturnError(fmt.Errorf("failed setup pragma"))
	if err := migrate(db); err != nil {
		t.Fatal("expect no error, but got:", err)
	}
}

func TestReinitTable(t *testing.T) {
	db, mock, _ := sqlmock.New()
	mock.ExpectExec("drop table.*").WillReturnError(fmt.Errorf("error drop table"))
	if err := reinitTable(db); err == nil {
		t.Fatal("expect 'error drop table' but got nil")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectExec("drop table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("drop view.*").WillReturnError(fmt.Errorf("error drop view"))
	if err := reinitTable(db); err == nil {
		t.Fatal("expect 'error drop view' but got nil")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectExec("drop table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("drop view.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec(".*").WillReturnError(fmt.Errorf("error when migrate"))
	if err := reinitTable(db); err == nil {
		t.Fatal("expect 'error when migrate' but got nil")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectExec("drop table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("drop view.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("create table.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("create view.*").WillReturnResult(sqlmock.NewResult(1, 1))
	mock.ExpectExec("pragma.*").WillReturnError(fmt.Errorf("failed setup pragma"))
	if err := reinitTable(db); err != nil {
		t.Fatal("expect no error, but got:", err)
	}

}

func BenchmarkRandomMsg(b *testing.B) {
	var res *[]byte
	for i := 0; i < b.N; i++ {
		res = randomMsg()
	}
	_ = res
}

func BenchmarkRandomMsgSimple(b *testing.B) {
	var res []byte
	for i := 0; i < b.N; i++ {
		res = randomMsgSimple()
	}
	_ = res
}

func BenchmarkRandomMsgBuilder(b *testing.B) {
	var res string
	for i := 0; i < b.N; i++ {
		res = randomMsgBuilder()
	}
	_ = res
}

func BenchmarkMainSimple(b *testing.B) {
	for i := 0; i < b.N; i++ {
		main()
	}
}

func BenchmarkMainPool(b *testing.B) {
	s := "pool"
	whichrand = &s
	for i := 0; i < b.N; i++ {
		main()
	}
}

func BenchmarkMainBuilder(b *testing.B) {
	s := "builder"
	whichrand = &s
	for i := 0; i < b.N; i++ {
		main()
	}
}
