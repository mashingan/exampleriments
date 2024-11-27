const std = @import("std");

const Token = enum {
    number,
    plus,
    minus,
    mul,
    div,
    eof,
    invalid,
};

const Lexer = struct {
    input: []const u8,
    pos: usize,

    pub fn init(input: []const u8) Lexer {
        return Lexer{
            .input = input,
            .pos = 0,
        };
    }

    pub fn next_token(self: *Lexer) Token {
        while ((self.pos < self.input.len) and (self.input[self.pos] == ' ' or self.input[self.pos] == '\t' or self.input[self.pos] == '\n')) {
            self.pos += 1; // Skip whitespace
        }

        if (self.pos >= self.input.len) {
            return Token.eof;
        }

        const c = self.input[self.pos];
        switch (c) {
            '+' => return Token.plus,
            '-' => return Token.minus,
            '*' => return Token.mul,
            '/' => return Token.div,
            '0'...'9' => return Token.number,
            else => return Token.invalid,
        }
    }

    pub fn advance(self: *Lexer) void {
        self.pos += 1;
    }
};

const Parser = struct {
    lexer: *Lexer,
    current_token: Token,

    pub fn init(self: *Parser, lexer: *Lexer) void {
        self.lexer = lexer;
        self.current_token = lexer.next_token();
    }

    pub fn eat(self: *Parser, token: Token) void {
        if (self.current_token == token) {
            self.current_token = self.lexer.next_token();
        } else {
            std.debug.print("Syntax error: Expected token {}, found {}.\n", .{ token, self.current_token });
            std.posix.abort();
        }
    }

    pub fn factor(self: *Parser) i32 {
        const token = self.current_token;
        if (token == Token.number) {
            const value = self.lexer.input[self.lexer.pos] - '0'; // Convert char to number (assuming single digit for simplicity)
            self.lexer.advance();
            self.eat(Token.number);
            return value;
        } else {
            std.debug.print("Syntax error: Expected a number, found {}.\n", .{token});
            std.posix.abort();
        }
    }

    pub fn term(self: *Parser) i32 {
        var result = self.factor();
        while (self.current_token == Token.mul or self.current_token == Token.div) {
            const token = self.current_token;
            if (token == Token.mul) {
                self.lexer.advance();
                self.eat(Token.mul);
                result *= self.factor();
            } else if (token == Token.div) {
                self.lexer.advance();
                self.eat(Token.div);
                result = @divTrunc(result, self.factor());
            }
        }
        return result;
    }

    pub fn expr(self: *Parser) i32 {
        var result = self.term();
        while (self.current_token == Token.plus or self.current_token == Token.minus) {
            const token = self.current_token;
            if (token == Token.plus) {
                self.lexer.advance();
                self.eat(Token.plus);
                result += self.term();
            } else if (token == Token.minus) {
                self.lexer.advance();
                self.eat(Token.minus);
                result -= self.term();
            }
        }
        return result;
    }
};

pub fn main() !void {
    const input = "3 + 5 * 2 - 8 / 4";
    var lexer = Lexer.init(input);
    var parser = Parser{
        .lexer = &lexer,
        .current_token = lexer.next_token(),
    };

    const result = parser.expr();
    std.debug.print("Result: {}\n", .{result});
}
