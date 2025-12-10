"""Text input reader with navigation capabilities."""


class Reader:
    """Handles reading and navigating through text input."""
    
    def __init__(self):
        self.ptr = 0
        self.text = ""
        self.filename = None

    def load_file(self, filename):
        """Load text from a file."""
        self.filename = filename
        with open(filename, "r") as f:
            self.text = f.read().replace("\n", "")

    def load_text(self, text):
        """Load text directly."""
        self.text = text

    def back(self, i=1):
        """Move pointer back by i positions."""
        self.ptr -= i

    def pos(self):
        """Get current pointer position."""
        return self.ptr

    def peek(self):
        """Peek at next character without advancing."""
        if self.ptr + 1 < len(self.text):
            return self.text[self.ptr + 1]
        return None
    
    def read(self, i=1):
        """Read i characters and advance pointer."""
        self.ptr += i
        if self.ptr > len(self.text):
            return False
        return self.text[self.ptr - i:self.ptr]

    def read_last(self, length=1, offset=0):
        """Read previous characters."""
        return self.text[self.ptr - length - offset: self.ptr - offset]
