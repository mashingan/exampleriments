# gresqlin

Invoke command `./run.ps1` to run.

Before we start, we need to create necessary user/role, db, schema and table with docker.  

1. Start docker desktop on Windows.
2. Invoke the script `run.ps1` using powershell terminal.
3. Enter psql13 terminal, using docker dekstop or cmd.
4. Connect to database within terminal with command: `psql -U postgres`.
5. Add new user `create role pquser with login createdb password 'psqlpass';`.
6. Add new database `create database pqdb with owner = pquser encoding = 'UTF8';`
7. Quit by pressing Ctrl+D.
8. Re-connect with command: `psql -U pquser -d pqdb`.
9. Add new schema: `create schema local`;`.
10. Add new table: `create table local.gresqlin(msg text, boring_float real, created_at timetamp with time zone default now());`
