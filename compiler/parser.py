from ast_definitions import *
from tokenizer import Token, Tokenizer, FileStream

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.errors = []
        self.idx = -1
        self.stack = []  # (idx, errors)
        self.barriers = []
        self.token = None
        self.furthest_error = None  # Track deepest parse failure
    
    def push(self):
        err_num = len(self.errors) 
        self.stack.append((self.idx, err_num))

    def pop(self):
        self.idx, err_num = self.stack.pop()
        self.errors = self.errors[0:err_num]

    def barrier(self):
        self.barriers.append(self.idx)

    def available(self, ignore_barriers=False):
        if not ignore_barriers and self.idx + 1 in self.barriers:
            return False
        return self.idx < len(self.tokens) - 1

    def log(self, error):
        """Log error with position tracking"""
        tok = self.token if self.token else (self.tokens[-1] if self.tokens else None)
        
        if tok:
            error_info = {
                'message': error,
                'token': tok,
                'line': tok.line,
                'col': tok.col,
                'pos': tok.pos
            }
        else:
            error_info = {
                'message': error,
                'token': None,
                'line': 0,
                'col': 0,
                'pos': len(self.tokens)
            }
        
        self.errors.append(error_info)
        
        # Track furthest error for better diagnostics
        if not self.furthest_error or error_info['pos'] > self.furthest_error['pos']:
            self.furthest_error = error_info

    def reset(self):
        self.errors = []
        self.furthest_error = None

    def advance(self):
        self.idx += 1
        if self.idx >= len(self.tokens):
            return False
        self.token = self.tokens[self.idx]
        return True

    def back(self, n=1):
        self.idx -= n
        if self.idx >= 0:
            self.token = self.tokens[self.idx]

    def collect(self):
        if self.advance():
            return self.token
        return None

    def expect(self, token_value):
        """Expect a specific token value"""
        if self.available(ignore_barriers=True):
            t = self.collect()
            if t and token_value == t.value:
                return t
            self.log(f"Expected '{token_value}' but got '{t.value if t else 'EOF'}'")
            self.back()
            return None
        
        self.log(f"Expected '{token_value}' but got EOF")
        return None

    def skip(self, token_value):
        """Try to skip a token, return it if found"""
        if self.available():
            t = self.collect()
            if t and t.value == token_value:
                return t
            self.back()
        return None


class SyntaxRule:
    """Base class - returns (success, ast_node)"""
    def check(self, parser):
        return False, None


# Token constants
T_FUNC = "func"
T_PAREN_START = "("
T_PAREN_END = ")"
T_BRACE_START = "{"
T_BRACE_END = "}"
T_COMMA = ","


class Block(SyntaxRule):
    def __init__(self, contents_rule, start_t=T_PAREN_START, end_t=T_PAREN_END, node_class=None):
        self.contents_rule = contents_rule
        self.start_t = start_t
        self.end_t = end_t
        self.node_class = node_class

    def check(self, parser):
        start_tok = parser.expect(self.start_t)
        if not start_tok:
            return False, None

        # Save position to find matching bracket
        parser.push()
        depth = 1
        while parser.available():
            c = parser.collect()
            if c and c.value == self.start_t:
                depth += 1
            elif c and c.value == self.end_t:
                depth -= 1
            if depth == 0:
                break
        
        parser.barrier()
        parser.pop()

        # Parse contents
        success, contents_node = self.contents_rule.check(parser)
        if not success:
            return False, None

        end_tok = parser.expect(self.end_t)
        if not end_tok:
            return False, None

        # Create AST node if class provided
        if self.node_class:
            node = self.node_class(contents_node, start_tok, end_tok)
            return True, node
        
        return True, contents_node

    def __repr__(self):
        return f"Block({self.start_t}...{self.end_t})"


class Delimited(SyntaxRule):
    def __init__(self, contents_rule, delimiter=T_COMMA, node_class=None):
        self.contents_rule = contents_rule
        self.delimiter = delimiter
        self.node_class = node_class

    def check(self, parser):
        items = []
        
        while True:
            success, node = self.contents_rule.check(parser)
            if not success:
                if len(items) == 0:
                    return False, None
                break
            
            items.append(node)
            
            if not parser.skip(self.delimiter):
                break
        
        # Wrap in list node if class provided
        if self.node_class:
            return True, self.node_class(items)
        
        return True, items

    def __repr__(self):
        return f"Delimited({self.contents_rule})"


class Chain(SyntaxRule):
    def __init__(self, *rules, node_class=None):
        self.rules = rules
        self.node_class = node_class

    def check(self, parser):
        nodes = []
        
        for rule in self.rules:
            success, node = rule.check(parser)
            if not success:
                return False, None
            nodes.append(node)
        
        # If node_class provided, construct it with all child nodes
        if self.node_class:
            return True, self.node_class(*nodes)
        
        # Otherwise return list of nodes
        return True, nodes

    def __repr__(self):
        return f"Chain({', '.join(str(r) for r in self.rules)})"


class Keyword(SyntaxRule):
    keywords = set()

    def __init__(self, word):
        self.word = word
        Keyword.keywords.add(word)

    def check(self, parser):
        tok = parser.expect(self.word)
        if tok:
            return True, tok  # Return the token itself
        return False, None
    
    def __repr__(self):
        return f"Keyword({self.word})"


class Symbol(SyntaxRule):
    def __init__(self, symbol):
        self.symbol = symbol

    def check(self, parser):
        tok = parser.expect(self.symbol)
        if tok:
            return True, tok
        return False, None

    def __repr__(self):
        return f"Symbol({self.symbol})"


class Variable(SyntaxRule):
    def __init__(self, name=None, node_class=None):
        self.name = name
        self.node_class = node_class

    def check(self, parser):
        if not parser.available():
            return False, None
        
        tok = parser.collect()
        if not tok:
            return False, None
            
        if tok.value in Keyword.keywords or tok.value in Tokenizer.specials:
            parser.back()
            return False, None

        # Create variable node if class provided
        if self.node_class:
            return True, self.node_class(tok)
        
        return True, tok

    def __repr__(self):
        return f"Variable({self.name})"


class Any(SyntaxRule):
    def __init__(self, *options, node_class=None):
        self.options = options
        self.node_class = node_class

    def check(self, parser):
        for option in self.options:
            parser.push()
            success, node = option.check(parser)
            if success:
                if self.node_class:
                    return True, self.node_class(node)
                return True, node
            parser.pop()
        return False, None


class Operand(SyntaxRule):
    def check(self, parser):
        # Try to match operators
        return Any(
            Chain(Symbol("="), Symbol("=")),
            Chain(Symbol(">"), Symbol("=")),
            Chain(Symbol("<"), Symbol("=")),
            Symbol(">"),
            Symbol("<"),
            Symbol("="),
            Symbol("+"),
            Symbol("-"),
            Symbol("/"),
            Symbol("*")
        ).check(parser)


class Statements(SyntaxRule):
    statements = {}

    @staticmethod
    def check(parser):
        statement_nodes = []
        best_errors, progress = None, 0
        
        while parser.available():
            found = False
            
            for name, rule in Statements.statements.items():
                parser.push()
                success, node = rule.check(parser)
                
                if success:
                    found = True
                    statement_nodes.append(node)
                    break
                
                # Track furthest failure
                if parser.idx > progress:
                    progress = parser.idx
                    best_errors = parser.errors.copy()
                
                parser.pop()

            if found:
                # Expect semicolon after statement
                if not parser.expect(";"):
                    return False, None
            else:
                # No statement matched - restore best error
                if best_errors:
                    parser.errors = best_errors
                parser.idx = progress
                return False, None
        
        return True, statement_nodes

    @staticmethod
    def register(name, rule):
        Statements.statements[name] = rule


class Statement(SyntaxRule):
    def __init__(self, name=None):
        self.name = name

    def check(self, parser):
        if not self.name:
            return False, None
        return Statements.statements[self.name].check(parser)

    def __repr__(self):
        return f"Statement({self.name})"


# ==============================================================
# AST GENERATORS
# ==============================================================

def FuncBuilder(keyword, name, params, body):
    print("PARANSNSS", params)
    return ASTFunction(name.value, params, body)

def DeclarationBuilder(name, colon, type_):
    return ASTDeclaration(name.value, type_.value)

def NewBuilder(keyword, declaration):
    return ASTNew(declaration)

def ExpressionBuilder(left, op_tok, right):
    op = op_tok.value
    print(left)
    obj = None
    if op in ["/", "*", "+", "-"]:
        obj = ASTBinaryOp(left, op, right)
    elif op in ["==", ">", "<", "<=", ">="]:
        obj = ASTComparison(left, op, right)
    elif op == "=":
        obj = ASTAssignment(left, right)
    return obj

def SymbolBuilder(tok):
    print("SYMBOLL: ", tok)
    name = tok.value
    try:
        j = int(name)
        return ASTStatic(j) 
    except ValueError:
        pass
    return ASTReference(name)
# ==============================================================
# GRAMMAR REGISTAR
# ==============================================================

Statements.register("func", Chain(
    Keyword("func"),
    Variable("name"),
    Block(Delimited(Statement("declaration"))),
    Block(Statements(), start_t="{", end_t="}"),
    node_class=FuncBuilder
))

Statements.register("declaration", Chain(
    Variable("name"),
    Symbol(":"),
    Variable("type"),
    node_class=DeclarationBuilder
))

Statements.register("new", Chain(
    Keyword("new"),
    Statement("declaration"),
    node_class=NewBuilder
))
Statements.register("expression", Chain(
    Variable("left", node_class=SymbolBuilder),
    Operand(),
    Variable("right", node_class=SymbolBuilder),
    node_class=ExpressionBuilder
))


# ==============================================================
# ERROR REPORTING
# ==============================================================

def print_errors(parser, source_lines):
    """Pretty print errors with context"""
    if not parser.errors:
        return
    
    print("\n" + "="*60)
    print("PARSING ERRORS")
    print("="*60)
    
    # Show furthest error (most likely issue)
    if parser.furthest_error:
        err = parser.furthest_error
        print(f"\nMost likely issue at line {err['line']}, column {err['col']}:")
        print(f"  {err['message']}")
        
        # Show source line with pointer
        if err['line'] <= len(source_lines):
            line = source_lines[err['line'] - 1]
            print(f"\n  {err['line']} | {line}")
            print(f"      {' ' * err['col']}^")
    
    # Show all errors
    print(f"\nAll errors ({len(parser.errors)}):")
    for i, err in enumerate(parser.errors[:10], 1):  # Limit to first 10
        print(f"  {i}. Line {err['line']}, Col {err['col']}: {err['message']}")
    
    if len(parser.errors) > 10:
        print(f"  ... and {len(parser.errors) - 10} more errors")

