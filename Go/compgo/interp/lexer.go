package interp

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           byte
}

func NewLexer(input string) *Lexer {
	return &Lexer{input, 0, 0, 0}
}

var mapTokenLexer = map[string]TokenType{
	"=": Assign,
	"+": Plus,
	"(": Lparen,
	")": Rparen,
	"{": Lbrace,
	"}": Rbrace,
	",": Comma,
	";": Semicolon,
}

func (l *Lexer) NextToken() Token {
	if l.readPosition == len(l.input) {
		return Token{Eof, ""}
	}
	tch := string(l.input[l.readPosition])
	t, ok := mapTokenLexer[string(l.input[l.readPosition])]
	if !ok {
		return Token{Illegal, ""}
	}
	l.readPosition++
	return Token{t, tch}
}
