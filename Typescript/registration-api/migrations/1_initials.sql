CREATE TABLE users(
    id PRIMARY KEY, UNIQUE, AUTOINCREMENT,
    email VARCHAR2(30) UNIQUE,
    "password" VARCHAR2(50)
);