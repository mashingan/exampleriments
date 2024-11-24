package main

import (
	"fmt"
	"testing"

	"github.com/DATA-DOG/go-sqlmock"
)

func TestPrepareStmt(t *testing.T) {
	db, mock, _ := sqlmock.New()
	mock.ExpectPrepare(fetchLabelById).
		WillReturnError(fmt.Errorf("this is error"))
	if err := prepareStmt(db); err == nil {
		t.Error("prepare stmt should be error")
	}

	db, mock, _ = sqlmock.New()
	mock.ExpectPrepare(fetchLabelById)
	if err := prepareStmt(db); err != nil {
		t.Error("prepare stmt should not error, but got:", err)
	}
}

func TestFetchId(t *testing.T) {
	db, mock, _ := sqlmock.New()
	mock.ExpectPrepare(fetchLabelById).ExpectQuery().
		WillReturnError(fmt.Errorf("error fetching"))
	//prepareStmt(db)
	stmt, _ = db.Prepare(fetchLabelById)
	if _, err := fetchId(1); err == nil {
		t.Error("expected error fetching")
	}

	labelrows := []string{"label"}
	db, mock, _ = sqlmock.New()
	expectLabel := "hehe"
	mock.ExpectPrepare(fetchLabelById).ExpectQuery().
		WillReturnRows(sqlmock.NewRows(labelrows).
			AddRow(expectLabel))
	//prepareStmt(db)
	stmt, _ = db.Prepare(fetchLabelById)
	label, err := fetchId(1)
	if err != nil {
		t.Error("expected stmt query success, got error:", err)
	}
	if label != expectLabel {
		t.Errorf("expected label '%s' but got '%s'", expectLabel, label)
	}
}
