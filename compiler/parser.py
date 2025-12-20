"""Token parser and lexer for the custom language."""

from config import BOUNDINGS, ParseError, get_special_chars
from reader import Reader


# Token type constants
TOKEN_TEXT = "t"
TOKEN_SEPARATOR = "b"
TOKEN_STAR = "*"
TOKEN_DOLLAR = "$"
TOKEN_RAW = "~"
TOKEN_STRING = "string"
TOKEN_TEMPLATE = "#"

# Special character handlers
SPECIAL_HANDLERS = {
    "*": TOKEN_STAR,
    "$": TOKEN_DOLLAR,
    "~": TOKEN_RAW,
}


def get_bounded_block_end(fileio, bounding_chars):
    """Find the end of a bounded block (e.g., matching parentheses).
    
    Args:
        fileio: Reader object for file I/O
        bounding_chars: Tuple of (opening, closing) characters
        
    Returns:
        int: Number of characters to the end of the block
        
    Raises:
        ParseError: If block is unclosed or malformed
    """
    char = fileio.read()
    if char != bounding_chars[0]:
        raise ParseError(f"Expected '{bounding_chars[0]}' at position {fileio.pos()}")
    
    depth = 1
    chars_read = 0
    
    while depth > 0:
        char = fileio.read()
        chars_read += 1
        
        if not char:
            raise ParseError("Unexpected EOF: unclosed bounded block")
        
        if char == bounding_chars[1]:
            depth -= 1
        elif char == bounding_chars[0]:
            depth += 1
    
    fileio.back(chars_read)
    return chars_read


def remove_spaces(text):
    """Remove leading spaces from text."""
    return text.lstrip()


def parse_string_literal(fileio):
    """Parse a string literal with escape sequences.
    
    Args:
        fileio: Reader object positioned after opening quote
        
    Returns:
        str: The parsed string content
        
    Raises:
        ParseError: If string is unclosed
    """
    string_buffer = ""
    char = fileio.read()
    
    while char and char != '"':
        if char == '\\':
            next_char = fileio.read()
            escape_sequences = {
                'n': '\n',
                't': '\t',
                '\\': '\\',
                '"': '"'
            }
            string_buffer += escape_sequences.get(next_char, next_char)
            char = fileio.read()
        else:
            string_buffer += char
            char = fileio.read()
    
    if char != '"':
        raise ParseError("Unexpected EOF: unclosed string literal")
    
    return string_buffer


def parse_template_reference(fileio):
    """Parse a template reference (#N or #*).
    
    Args:
        fileio: Reader object positioned after '#'
        
    Returns:
        tuple: (TOKEN_TEMPLATE, template_id) where id is int or "*"
        
    Raises:
        ParseError: If template reference is invalid
    """
    reference = fileio.read(2)
    
    if reference and len(reference) >= 1 and reference[0] == "*":
        return (TOKEN_TEMPLATE, "*")
    
    try:
        template_id = int(reference)
        return (TOKEN_TEMPLATE, template_id)
    except ValueError:
        raise ParseError(f"Invalid template id '#{reference}'")


def parse_bounded_block(fileio, bounding_chars):
    """Parse a bounded block (parentheses, brackets, braces).
    
    Args:
        fileio: Reader object positioned before opening bounding char
        bounding_chars: Tuple of (opening, closing) characters
        
    Returns:
        tuple: (bounding_type, parsed_tokens) where bounding_type is like "()"
    """
    block_end = get_bounded_block_end(fileio, bounding_chars)
    block_content = fileio.read(block_end - 1)
    block_content = remove_spaces(block_content)
    
    reader = Reader()
    reader.load_text(block_content)
    parsed_tokens = parse(reader)
    
    bounding_type = f"{bounding_chars[0]}{bounding_chars[1]}"
    return (bounding_type, parsed_tokens)


def handle_special_character(char, fileio):
    """Handle special single characters that become tokens.
    
    Args:
        char: The special character
        fileio: Reader object for reading additional chars if needed
        
    Returns:
        tuple: (token_type, value) or None if char doesn't create a complete token
    """
    if char == ";":
        return (TOKEN_SEPARATOR, ";")
    if char == ",":
        return (TOKEN_SEPARATOR, ".")
    elif char == ".":
        return (TOKEN_TEXT, ".")
    elif char == ":":
        return (TOKEN_TEXT, ":")
    elif char == "&":
        return (TOKEN_TEXT, "&")

    return None


def parse(fileio):
    """Parse input text into token tree.
    
    Args:
        fileio: Reader object containing the text to parse
        
    Returns:
        list: List of tokens as (type, value) tuples
    """
    tokens = []
    buffer = ""
    current_token_type = TOKEN_TEXT
    special_chars = get_special_chars()
    
    char = fileio.read()
    while char:
        if char in special_chars:
            # Flush any buffered text (wildcards can be empty, so flush even if buffer is empty)
            if buffer.strip() or current_token_type in (TOKEN_STAR, TOKEN_DOLLAR, TOKEN_RAW):
                tokens.append((current_token_type, buffer))
            
            current_token_type = TOKEN_TEXT
            buffer = ""

            # Handle string literals
            if char == '"':
                string_content = parse_string_literal(fileio)
                tokens.append((TOKEN_STRING, string_content))
                char = fileio.read()
                continue

            # Handle template references
            if char == "#":
                template_token = parse_template_reference(fileio)
                tokens.append(template_token)
                char = fileio.read()
                continue
            
            # Handle simple special characters
            simple_token = handle_special_character(char, fileio)
            if simple_token:
                tokens.append(simple_token)
                char = fileio.read()
                continue
            
            # Handle token type modifiers
            if char in SPECIAL_HANDLERS:
                current_token_type = SPECIAL_HANDLERS[char]
                char = fileio.read()
                continue
            
            # Handle bounded blocks
            bounded_token = None
            for bounding in BOUNDINGS:
                if char == bounding[0]:
                    fileio.back()
                    bounded_token = parse_bounded_block(fileio, bounding)
                    break
            
            if bounded_token:
                tokens.append(bounded_token)
                char = fileio.read()
                continue
        else:
            buffer += char

        char = fileio.read()

    # Flush any remaining buffered text
    if buffer.strip():
        tokens.append((current_token_type, buffer))

    return tokens


def split_sentences(tokens):
    """Split tokens into sentences based on separators.
    
    Args:
        tokens: List of parsed tokens
        
    Returns:
        list: List of token lists, one per sentence
    """
    sentences = []
    start_index = 0
    
    for index, (token_type, _) in enumerate(tokens):
        if token_type == TOKEN_SEPARATOR:
            if start_index < index:
                sentences.append(tokens[start_index:index])
            start_index = index + 1
    
    if start_index < len(tokens):
        sentences.append(tokens[start_index:])
    
    return [sentence for sentence in sentences if sentence]