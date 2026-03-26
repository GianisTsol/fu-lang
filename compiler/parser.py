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
        b_num = len(self.barriers)
        self.stack.append((self.idx, err_num, b_num, self.furthest_error))

    def pop(self):
        self.idx, err_num, b_num, prev_furthest = self.stack.pop()
        self.errors = self.errors[0:err_num]
        self.barriers = self.barriers[0:b_num]
        self.token = self.tokens[self.idx] if self.idx >= 0 else None
        # Keep whichever furthest_error reached deepest
        if prev_furthest and (not self.furthest_error or prev_furthest['pos'] > self.furthest_error['pos']):
            self.furthest_error = prev_furthest

    def squash(self):
        _, _, _, prev_furthest = self.stack.pop()
        # Keep whichever furthest_error reached deepest
        if prev_furthest and (not self.furthest_error or prev_furthest['pos'] > self.furthest_error['pos']):
            self.furthest_error = prev_furthest

    def barrier(self, idx=-1):
        if idx == -1:
            idx = self.idx
        self.barriers.append(idx)

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
        
        end = parser.idx
        parser.pop()

        # Barrier is now owned by its own frame so it gets rolled back on failure
        parser.push()
        parser.barrier(end)

        # Parse contents
        success, contents_node = self.contents_rule.check(parser)
        if not success:
            parser.pop()
            return False, None

        end_tok = parser.expect(self.end_t)
        if not end_tok:
            parser.pop()
            return False, None

        parser.squash()

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
                # if len(items) == 0:
                #     return False, None
                break
            
            items.append(node)
            
            if not parser.skip(self.delimiter):
                break
        
        # Wrap in list node if class provided
        if self.node_class:
            return True, self.node_class(items)
        
        return True, items

    def __repr__(self):
        return f"Delimited({self.delimiter})"


class Chain(SyntaxRule):
    def __init__(self, *rules, node_class=None):
        self.rules = rules
        self.node_class = node_class

    def check(self, parser):
        nodes = []
        parser.push()
        for rule in self.rules:
            success, node = rule.check(parser)
            if not success:
                parser.pop()
                return False, None
            nodes.append(node)
        
        parser.squash()
        # If node_class provided, construct it with all child nodes
        if self.node_class:
            return True, self.node_class(*nodes)
        
        # Otherwise return list of nodes
        return True, nodes

    def __repr__(self):
        # return f"Chain({', '.join(str(r) for r in self.rules)})"
        return f"Chain({len(self.rules)})"


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

class FuType(SyntaxRule):
    def __init__(self, name=None, args=[], node_class=None):
        self.name = name
        self.args = args
        self.node_class = node_class

    def check(self, parser):
        success, res = Chain(Variable("name"), Optional(Block(Delimited(Ref("arg")), start_t="<", end_t=">"), default=[])).check(parser)
        
        return success, (res[0].value, res[1])

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
                parser.squash()
                if self.node_class:
                    return True, self.node_class(node)
                return True, node
            parser.pop()
        return False, None
    
    def __repr__(self):
        return f"Any({len(self.options)})"

class Optional(SyntaxRule):
    def __init__(self, option, default=None):
        self.option = option
        self.default = default
    
    def check(self, parser):
        parser.push()
        success, node = self.option.check(parser)
        if success:
            parser.squash()
            return True, node
        else:
            parser.pop()
            return True, self.default

    def __repr__(self):
        return f"Optional({repr(self.option)})"

class Operand(SyntaxRule):
    def check(self, parser):
        # Try to match operators
        return Any(
            Chain(Symbol("="), Symbol("=")),
            Chain(Symbol(">"), Symbol("=")),
            Chain(Symbol("<"), Symbol("=")),
            Symbol(">"),
            Symbol("<"),
            Symbol("+"),
            Symbol("-"),
            Symbol("/"),
            Symbol("*")
        ).check(parser)

class Name(SyntaxRule):
    def __init__(self, name, node_class=None):
        self.name = name
        self.node_class = node_class

    def check(self, parser):
        success, node = Chain(
            Variable("name"),
            Optional(Block(Ref("idx"), start_t="[", end_t="]")),
            Optional(Chain(Symbol("."), Ref("child"), node_class=lambda *x: x[1])),
            node_class=NameBuilder).check(parser)
        if success:
            if self.node_class:
                return True, self.node_class(node)
            else:
                return True, node
        return False, None
        
class Statements(SyntaxRule):
    statements = {}

    @staticmethod
    def check(parser):
        statement_nodes = []
        best_errors, best_idx = None, -1
        Statement.offs += 4

        while parser.available():
            found = False
            print("=======\n", "LOOP\n", "========\n")

            for depth, (name, rule) in enumerate(Statements.statements.items()):
                parser.push()

                print(name, parser.token)

                success, node = rule.check(parser)
                if success:
                    found = True
                    statement_nodes.append(node)
                    break

                print(parser.errors)

                # Track the attempt that got furthest by token index
                if parser.idx > best_idx:
                    best_idx = parser.idx
                    best_errors = parser.errors.copy()

                parser.pop()

            if found:
                # Expect semicolon after statement
                if not parser.expect(";"):
                    statement_nodes.pop()
                    parser.pop()
                    Statement.offs -= 4
                    return False, None
                parser.squash()
            else:
                # No statement matched - restore best error context
                if best_errors:
                    parser.errors = best_errors
                Statement.offs -= 4
                return False, None
        
        Statement.offs -= 4
        return True, statement_nodes

    @staticmethod
    def register(name, rule):
        Statements.statements[name] = rule
    


class Statement(SyntaxRule):
    offs = 0

    def __init__(self, name=None):
        self.name = name

    def check(self, parser):
        if not self.name:
            return False, None
        print(f"{' '*Statement.offs}TEST {self.name} {parser.token}")
        Statement.offs += 4
        success, node = Statements.statements[self.name].check(parser)
        Statement.offs -= 4
        print(f"{' '*Statement.offs}{'PASS' if success else 'FAIL'} {self.name} L{parser.token.line}C{parser.token.col} {parser.token}")

        if not success:
            tok = parser.token
            if tok:
                candidate = {
                    'message': f"Expected '{self.name}' statement",
                    'token': tok,
                    'line': tok.line,
                    'col': tok.col,
                    'pos': tok.pos,
                }
                if not parser.furthest_error or candidate['pos'] >= parser.furthest_error['pos']:
                    parser.furthest_error = candidate

        return success, node

    def __repr__(self):
        return f"Statement({self.name})"


# ==============================================================
# AST GENERATORS
# ==============================================================
def AsmBuilder(keyword, inst, params):
    return ASTAsm(inst.value, params)

def FuncBuilder(keyword, name, params, body):
    print("PARANSNSS", params)
    return ASTFunction(name.value, params, body)

def CallBuilder(name, args):
    return ASTFuncCall(name.value, args)

def ReturnBuilder(keyword, expression):
    return ASTReturn(expression)

def MacroBuilder(keyword, name, params, block):
    return ASTMacro(name.value, params, block)

def ClassBuilder(keyword, name, block):
    return ASTClass(name.value, block)

def DeclarationBuilder(name_obj, colon, type_):
    name, size, child = name_obj
    size = 1

    print(type_)
    return ASTDeclaration(name, type_, length=size)

def NewBuilder(keyword, declaration):
    return ASTNew(declaration)

def StaticBuilder(keyword, declaration):
    return None

def ExpressionBuilder(left, op_tok, right):
    if isinstance(op_tok, list):
        op = "".join(t.value for t in op_tok)
    else:
        op = op_tok.value
    obj = None
    if op in ["/", "*", "+", "-"]:
        obj = ASTBinaryOp(left, op, right)
    elif op in ["==", ">", "<", "<=", ">="]:
        obj = ASTComparison(left, op, right)
    elif op == "=":
        obj = ASTAssignment(left, right)
    return obj

def RefBuilder(name_obj):
    print("SYMBOLL: ", name_obj)
    assert "[" not in name_obj[0]
    name, idx, child = name_obj
    try:
        j = int(name)
        return ASTStatic(j) 
    except ValueError:
        pass

    ref = ASTReference(name, idx)

    if child != None:
        return ASTAtrribute(ref, child)
    return ref

def NameBuilder(tok, opt_size, opt_child):
    print("NAMEEE: ", tok, opt_size)
    name = tok.value
    size = 1
    if opt_size != None:
        size = opt_size
    return (name, opt_size, opt_child)

def IfBuilder(keyword, condition, block):
    return ASTIfStatement(condition, block)
    
# ==============================================================
# GRAMMAR REGISTAR
# ==============================================================

Ref = lambda l: Name(l, node_class=RefBuilder)


Statements.register("class", Chain(
    Keyword("class"),
    Variable("name"),
    Block(Statements(), start_t="{", end_t="}"),
    node_class=ClassBuilder
))

Statements.register("func", Chain(
    Keyword("func"),
    Variable("name"),
    Block(Delimited(Statement("declaration"))),
    Block(Statements(), start_t="{", end_t="}"),
    node_class=FuncBuilder
))

Statements.register("macro", Chain(
    Keyword("macro"),
    Variable("name"),
    Block(Delimited(Statement("declaration"))),
    Block(Statements(), start_t="{", end_t="}"),
    node_class=MacroBuilder
))

Statements.register("call", Chain(
    Variable("target"),
    Block(Delimited(Ref("source"))),
    node_class=CallBuilder
))

Statements.register("return", Chain(
    Keyword("return"),
    Statement("expression"),
    node_class=ReturnBuilder
))


Statements.register("assignment", Chain(
    Any(Statement("new"), Statement("static"), Ref("dest")),
    Symbol("="),
    Statement("expression"),
    node_class=ExpressionBuilder 
))


Statements.register("declaration", Chain(
    Name("decname"),
    Symbol(":"),
    FuType("type"),
    node_class=DeclarationBuilder
))


Statements.register("expression", Any(
    Block(Statement("expression")),
    Any(
        Statement("operation"),
        Statement("call"),
        Ref("var"),
)))

Statements.register("operation",
    Chain(
        Any(Ref("left"), Statement("call")),
        Operand(),
        Statement("expression"),
        node_class=ExpressionBuilder
        ),   
)

Statements.register("new", Chain(
    Keyword("new"),
    Statement("declaration"),
    node_class=NewBuilder
))


Statements.register("static", Chain(
    Keyword("static"),
    Statement("declaration"),
    node_class=StaticBuilder
))

Statements.register("if", Chain(
    Keyword("if"),
    Block(Statement("expression")),
    Block(Statements(), start_t="{", end_t="}"),
    node_class=IfBuilder
))

Statements.register("asm", Chain(
    Keyword("asm"),
    Variable("inst"),
    Block(Delimited(Ref("arg"))),
    node_class=AsmBuilder
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
        
        # Show source line with pointer (col is 0-indexed)
        if 0 < err['line'] <= len(source_lines):
            line = source_lines[err['line'] - 1]
            print(f"\n  {err['line']} | {line}")
            print(f"  {' ' * (len(str(err['line'])) + 3 + err['col'])}^")
    
    # Deduplicate errors by (line, col, message) before displaying
    seen = set()
    unique_errors = []
    for err in parser.errors:
        key = (err['line'], err['col'], err['message'])
        if key not in seen:
            seen.add(key)
            unique_errors.append(err)

    limit = 10
    print(f"\nAll errors ({len(unique_errors)}):")
    for i, err in enumerate(unique_errors[:limit], 1):
        print(f"  {i}. Line {err['line']}, Col {err['col']}: {err['message']}")
    
    if len(unique_errors) > limit:
        print(f"  ... and {len(unique_errors) - limit} more errors")