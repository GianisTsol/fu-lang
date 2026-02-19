

class Token:
    """Enhanced token with position information"""
    def __init__(self, value, line, col, pos):
        self.value = value
        self.line = line
        self.col = col
        self.pos = pos  # character position in file
    
    def __repr__(self):
        return f"Token({self.value!r}, L{self.line}:C{self.col})"
    
    def __eq__(self, other):
        if isinstance(other, str):
            return self.value == other
        return self.value == other.value



class Tokenizer:
    specials = [";", ",", ".", ":", "(", ")", "[", "]", "{", "}", "*", "%", "=", "+", "-", ">", "<"]

    def __init__(self, stream):
        self.stream = stream
        self.buf = ""
        self.line = 1
        self.col = 1
        self.pos = 0

    def next(self):
        while True:
            c = self.stream.read()
            
            if not c:
                # Return any remaining buffer
                if len(self.buf) > 0:
                    tok = Token(self.buf, self.line, self.col - len(self.buf), self.pos - len(self.buf))
                    self.buf = ""
                    return tok
                return None

            self.pos += 1

            # Track newlines
            if c == "\n":
                self.line += 1
                self.col = 1
            else:
                self.col += 1

            # Skip whitespace
            if c in [" ", "\t", "\n", "\r"]:
                if len(self.buf) > 0:
                    tok = Token(self.buf, self.line, self.col - len(self.buf) - 1, self.pos - len(self.buf) - 1)
                    self.buf = ""
                    return tok
                continue

            # Handle special characters
            if c in self.specials:
                if len(self.buf) > 0:
                    # Return buffer first, put special back
                    self.stream.back()
                    self.pos -= 1
                    self.col -= 1
                    tok = Token(self.buf, self.line, self.col - len(self.buf), self.pos - len(self.buf))
                    self.buf = ""
                    return tok
                else:
                    # Return special immediately
                    return Token(c, self.line, self.col - 1, self.pos - 1)

            # Accumulate
            self.buf += c


class FileStream:
    def __init__(self, filename):
        with open(filename, "r") as f:
            self.bytes = f.read()
        self.idx = 0

    def read(self):
        if self.idx >= len(self.bytes):
            return None
        c = self.bytes[self.idx]
        self.idx += 1
        return c

    def back(self):
        if self.idx > 0:
            self.idx -= 1
