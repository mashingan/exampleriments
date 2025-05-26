package interp

type TokenType uint

type Token struct {
	Type    TokenType
	Literal string
}

const (
	Illegal TokenType = iota
	Eof
	Ident
	Int
	Assign
	Plus
	Comma
	Semicolon
	Lparen
	Rparen
	Lbrace
	Rbrace
	Function
	Let
)
