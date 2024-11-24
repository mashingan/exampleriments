DROP TABLE IF EXISTS authors;
CREATE TABLE authors (
    id integer PRIMARY KEY AUTOINCREMENT,
    author_name text,
    created_at TEXT default (datetime('now'))
);

DROP TABLE IF EXISTS tutorials;
CREATE TABLE tutorials (
    id integer PRIMARY KEY AUTOINCREMENT,
    title TEXT,
    created_at TEXT default (datetime('now')),
    author INTEGER, 
    FOREIGN KEY(author) REFERENCES authors(id)
);

DROP TABLE IF EXISTS comments;
CREATE TABLE comments (
    id integer PRIMARY KEY AUTOINCREMENT,
    body_comment text,
    commenter name,
    tutorial integer,
    created_at TEXT default (datetime('now')),
    FOREIGN KEY(tutorial) REFERENCES tutorials(id)
);

INSERT INTO authors(id, author_name) VALUES
(1, 'rahshingan');

INSERT INTO tutorials (title, author) VALUES
('First Tutorial', 1),
('Second Tutorial', 1),
('third tutorial', 1);

INSERT INTO comments(body_comment, commenter, tutorial) VALUES
('test comment for tutorial', 'rahshingan', 1),
('test comment for tutorial', 'rahshingan', 2),
('test comment for tutorial', 'rahshingan', 3);