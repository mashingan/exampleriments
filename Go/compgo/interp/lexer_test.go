package interp

import "testing"

func TestNextToken(t *testing.T) {
	input := `=+(){},;`
	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{Assign, "="},
		{Plus, "+"},
		{Lparen, "("},
		{Rparen, ")"},
		{Lbrace, "{"},
		{Rbrace, "}"},
		{Comma, ","},
		{Semicolon, ";"},
		{Eof, ""},
	}
	l := NewLexer(input)
	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - token type wrong. expected=%q, got %q",
				i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - token literal wrong. expected=%q, got %q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}

}
