"""Token parser and lexer for the custom language."""

from config import BOUNDINGS, ParseError, get_special_chars
from reader import Reader


def get_bounded_block_end(fileio, bounding_chars):
    """Find the end of a bounded block (e.g., matching parentheses)."""
    c = fileio.read()
    if c != bounding_chars[0]:
        raise ParseError(f"Expected '{bounding_chars[0]}' at position {fileio.pos()}")
    
    depth = 1
    chars_read = 0
    
    while depth > 0:
        c = fileio.read()
        chars_read += 1
        
        if not c:
            raise ParseError("Unexpected EOF: unclosed bounded block")
        
        if c == bounding_chars[1]:
            depth -= 1
        elif c == bounding_chars[0]:
            depth += 1
    
    fileio.back(chars_read)
    return chars_read


def remove_spaces(text):
    """Remove leading spaces from text."""
    return text.lstrip()


def parse(fileio):
    """Parse input text into token tree."""
    final = []
    buf = ""
    temp_type = "t"
    specials = get_special_chars()
    
    c = fileio.read()
    while c:
        if c in specials:
            if buf.strip():
                final.append((temp_type, buf))
            
            temp_type = "t"
            buf = ""

            # Handle string literals
            if c == '"':
                string_buf = ""
                c = fileio.read()
                while c and c != '"':
                    if c == '\\':
                        next_c = fileio.read()
                        if next_c == 'n':
                            string_buf += '\n'
                        elif next_c == 't':
                            string_buf += '\t'
                        elif next_c == '\\':
                            string_buf += '\\'
                        elif next_c == '"':
                            string_buf += '"'
                        else:
                            string_buf += next_c
                        c = fileio.read()
                    else:
                        string_buf += c
                        c = fileio.read()
                
                if c != '"':
                    raise ParseError("Unexpected EOF: unclosed string literal")
                
                final.append(("string", string_buf))
                c = fileio.read()
                continue

            # Handle special single characters
            if c == "*":
                temp_type = "*"
            elif c == "$":
                temp_type = "$"
            elif c == ";":
                final.append(("b", ";"))
                c = fileio.read()
                continue
            elif c == ".":
                final.append(("t", "."))
                c = fileio.read()
                continue
            elif c == ":":
                final.append(("t", ":"))
                c = fileio.read()
                continue
            elif c == "#":
                s = fileio.read(2)
                if s and len(s) >= 1 and s[0] == "*":
                    final.append(("#", "*"))
                    c = s[1] if len(s) > 1 else fileio.read()
                    continue
                try:
                    template_id = int(s)
                    final.append(("#", template_id))
                except ValueError:
                    raise ParseError(f"Invalid template id '#{s}'")
                c = fileio.read()
                continue
            
            # Handle bounded blocks
            is_bounding = False
            for bounding in BOUNDINGS:
                if c == bounding[0]:
                    fileio.back()
                    block_end = get_bounded_block_end(fileio, bounding)
                    block = fileio.read(block_end - 1)
                    block = remove_spaces(block)

                    reader = Reader()
                    reader.load_text(block)
                    parsed = parse(reader)

                    final.append((f"{bounding[0]}{bounding[1]}", parsed))
                    is_bounding = True
                    break
            
            if is_bounding:
                c = fileio.read()
                continue
        else:
            buf += c

        c = fileio.read()

    if buf.strip():
        final.append((temp_type, buf))

    return final


def split_sentences(tokens):
    """Split tokens into sentences based on separators."""
    sentences = []
    start = 0
    
    for k, (token_type, _) in enumerate(tokens):
        if token_type == "b":
            if start < k:
                sentences.append(tokens[start:k])
            start = k + 1
    
    if start < len(tokens):
        sentences.append(tokens[start:])
    
    return [s for s in sentences if s]
