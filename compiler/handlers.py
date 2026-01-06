"""Template handler functions for code generation."""

from config import ARGS_REGISTERS
from ir_system import IRSystem
from reader import Reader
from parser import parse as parse_tokens
from template_matcher import template_match

from config import IMI

class Register:
    def __init__(self, idx):
        self.idx = idx

    def __repr__(self):
        return f"V{self.idx}"

class Address:
    def __init__(self, reg, offset):
        self.reg = reg
        self.offset = offset

    def add(self, other):
        offset = 0
        if isinstance(other, int):
            offset = self.offset + other
        elif isinstance(other, Address):
            if self.reg != other.reg:
                print("Warning: Trying to add addresses with different start regissters.")
                return None
            offset = self.offset + other.offset
        return Address(self.reg, offset)

    def __repr__(self):
        return f"{self.reg}+{self.offset}"

class AnalyzeContext:
    def __init__(self, name):
        self.name = name

        self.parent = None
        self.local = {}

    def push(self, name):
        n = AnalyzeContext(name=name)
        n.parent = self
        return n

    def pop(self):
        if not self.parent:
            print("Error: Cant pop first context.")
            return
        return self.parent

    def consume(self, ctx):
        self.local[ctx.name] = ctx

    def add_object(self, name, obj):
        if name in self.local:
            print(f"Warning: trying to add existing object: {name}")
            return False
        self.local[name] = obj
    
    def get_object(self, name):
        if name in self.local:
            return self.local[name]
        
        if self.parent:
            r = self.parent.get_object(name)
            if r:
                return r
        #print(f"Warning: Geting unknown object: {name}")
        return None

class CodeGenVar:
    def __init__(self, name, vtype):
        self.name = name
        self.vtype = None
        self.address = None
        self.size = 4
    
    def update_address(self, address):
        self.address = address

class CodeGenContext:
    def __init__(self, name="root"):
        self.vars = {}
        self.static = {}

        self.stack_offset = 0

        self.code = []

        self.label_count = 0
        self.reg_count = 0
        self.prev = None

    def push(self, name="null"):
        a = CodeGenContext(name)
        a.name = name
        a.reg_count = self.reg_count
        a.prev = self
        return a
    
    def pop(self):
        if not self.prev:
            print("Error: Cant pop first context.")
            return
        self.prev.code.extend(self.code)
        self.prev.reg_count = self.reg_count
        return self.prev

    def emit(self, code):
        if isinstance(code, list):
            self.code.extend(code)
        self.code.append(code)
        print("emitting: ", code)

    def get_var(self, name):
        if name in self.vars:
            return self.vars[name]
    
    def new_var(self, name, vtype):
        if name in self.vars.keys():
            print(f"Error: Variable already registered: {name}")
            return None
        self.vars[name] = CodeGenVar(name, vtype)

    def get_memory(self, size):
        a = Address("vsp", self.stack_offset)
        self.stack_offset += size
        return a

    def get_size(self, name):
        if name in self.vars:
            return self.vars[name].size
    
    def new_label(self):
        self.label_count += 1
        return self.label_count

    def new_reg(self):
        self.reg_count += 1
        return Register(self.reg_count)

    def update_variable_address(self, name, address: Address):
        if name not in self.vars.keys():
            print(f"Error: Variable not found: {name}")
            return None
        self.vars[name].update_address(address)

class ASTNode:
    def __init__(self):
        self.parent = None

    def analyze(self, ctx: AnalyzeContext):
        pass
    def compile(self, ctx: CodeGenContext):
        pass

class ASTStatic(ASTNode):
    def __init__(self, data):
        self.vtype = None
        if isinstance(data, int):
            self.vtype = "usize"
        
        self.data = data

    def reduce(self, ctx: CodeGenContext):
        if self.vtype == "usize":
            return self.data
    
    def __repr__(self):
        return f"static {self.data}: {self.vtype}"

class ASTReference:
    def __init__(self, name):
        self.name = name
        self.vtype = None
        self.address = None

    def analyze(self, ctx: AnalyzeContext):
        self.vtype = ctx.get_object(self.name)["type"]
    
    def compile(self, ctx: CodeGenContext):
        self.address = ctx.get_var(self.name).address
    
    def reduce(self, ctx: CodeGenContext):
        return self.address

    def __repr__(self):
        return f"ref {self.name}: {self.vtype} ({self.address})"

    
class ASTClass(ASTNode):
    def __init__(self, name, statements):
        self.name = name
        self.statements = statements

    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)
        for statement in self.statements:
            statement.analyze(ctx)
        ctx = ctx.pop()

    def compile(self, context: IRSystem.BlockContext):
        """Handle class declarations."""
        ctx = context.push(self.name)

        for statement in self.statements:
            statement.compile(ctx)
        print(f"Succesfully compiled Class: {self.name}")
        ctx = ctx.pop()
        return True

    def __str__(self):
        inner = ", ".join(str(s) for s in self.statements)
        return f"Class([{inner}])"
    
class ASTReturn:
    def __init__(self, ref: ASTReference):
        self.ref = ref
        self.vtype = None

    def analyze(self, ctx):
        self.ref.analyze(ctx)
        self.vtype = self.ref.vtype

class ASTFunction(ASTNode):
    def __init__(self, name, args, statements):
        self.name = name
        self.args = args #list[ASTDeclaration]
        self.statements = statements
        self.vtype = None
    
    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)

        for arg in self.args:
            arg.analyze(ctx)
        
        return_type = None
        return_types = []

        for statement in self.statements:
            statement.analyze(ctx)
            print(f"Analyzed: {statement}")

            if isinstance(statement, ASTReturn):
                return_types.append(statement.type)
        print("analyzing return types")
        if return_types and len(return_types) >= 1:
            for i in return_types[1:]:
                if i != return_types[0]:
                    return_type = None
                    print(f"Error: Function returns too many types: {return_types}")
                    return
            return_type = return_types[0]
        if not return_type:
            return_type = "void"
        self.vtype = return_type
        print(f"Analyzed func: {self.name}")
        ctx = ctx.pop()
        
    def compile(self, context: IRSystem.BlockContext):
        context = context.push(self.name)
        context.emit(IRSystem.ib.label(self.name))
        for arg in self.args:
            arg.compile(context)
        
        for statement in self.statements:
            statement.compile(context)
        context = context.pop()
    
    def __repr__(self):
        return f"Func {self.name} -> {self.vtype}"

class ASTFuncCall(ASTNode):
    def __init__(self, name, args):
        self.name = name
        self.args = args #list[ASTReference]
        self.func = None
    
    def analyze(self, ctx: AnalyzeContext):
        self.func = ctx.get_object(self.name)
        for idx, arg in enumerate(self.args):
            arg.analyze(ctx)

            assert arg.vtype == self.func.args[idx].vtype


    def compile(self, context: CodeGenContext):
        print(f"Call: {name}, Args: {args}")        

        for idx, arg in enumerate(self.args):
            arg.compile(context)

            if idx < ARGS_REGISTERS:
                context.emit(IRSystem.ib.move(f"v{i + 1}", arg.address))
            elif idx >= ARGS_REGISTERS:
                context.emit(IRSystem.ib.push(arg.address))

class ASTDeclaration(ASTNode):
    def __init__(self, name, vtype):
        self.name = name
        self.vtype = vtype

    def analyze(self, ctx: AnalyzeContext):
        if ctx.get_object(self.name):
            print("Error: Variable already declared")
            return False
        ctx.add_object(name=self.name, obj={"type": self.vtype})

    def compile(self, context: CodeGenContext):
        context.new_var(self.name, self.vtype)

    def __repr__(self):
        return f"{self.name}: {self.vtype}"

class ASTAssignment(ASTNode):
    def __init__(self, target, source):
        self.target = source
        self.source = target
    
    def analyze(self, context):
        self.source.analyze(context)
        self.target.analyze(context)

    def compile(self, context: CodeGenContext):
        self.source.compile(context)
        self.target.compile(context)
        
        #dest, src = self.target.reduce(context), self.source.reduce(context)
        dest, src = self.target, self.source
        context.emit(IRSystem.ib.move(dest, src))

    def __repr__(self):
        return f"{self.source} = {self.target}"

class ASTNew(ASTNode):
    def __init__(self, declaration):
        self.declaration: ASTDeclaration = declaration
        self.address = None
        
    def analyze(self, context):
        self.declaration.analyze(context)
        print(f"Analyzed: {self.declaration}")

    def compile(self, context: CodeGenContext):
        self.declaration.compile(context)

        size = context.get_size(self.declaration.name)
        self.address = context.get_memory(size)
        context.update_variable_address(self.declaration.name, self.address)

    def reduce(self, ctx: CodeGenContext):
        return ctx.get_var(self.declaration.name).address

    def __repr__(self):
        return f"new {self.declaration} ({self.address})"

class ASTMacro(ASTNode):
    def __init__(self, name, block):
        self.name = name
        self.block = block

    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)
        for statement in self.block:
            statement.analyze(ctx)
        ctx.add_object(self.name, self)
        ctx = ctx.pop()

    def compile(self, ctx):
        for statement in self.block:
            statement.compile(ctx)

class ASTBinaryOp(ASTNode):
    def __init__(self, left, op, right):
        self.left = left #ASTReference
        self.right = right #ASTReference
        self.op = op #string + - * / ==

        self.result = None # destination register / address
    
    def analyze(self, ctx):
        self.left.analyze(ctx)
        self.right.analyze(ctx)

        ltype = self.left.vtype
        rtype = self.right.vtype
        print(f"Handling op: {self.left} {self.op} {self.right}")
        if not ltype or not rtype:
            print("Error: op without types? Seriously?..")
            return
        if ltype != rtype:
            print("Warning: op types not matching! ")
        #print(f"Using type {ltype}")

    def compile(self, ctx: CodeGenContext):
        self.right.compile(ctx)
        self.left.compile(ctx)
        self.result = ctx.new_reg()
        ctx.emit(IRSystem.ib.move(self.result, self))

    def reduce(self, ctx):
        return self.result

    def __repr__(self):
        return f"{self.left} {self.op} {self.right}"

class ASTIfStatement(ASTNode):
    def __init__(self, condition, block):
        self.condition = condition #ASTBinaryOp
        self.block = block #list[ASTNode]

    def analyze(self, ctx: AnalyzeContext):
        self.condition.analyze(ctx)
        for statement in self.block:
            statement.analyze(ctx)

    def compile(self, ctx: CodeGenContext):
        self.condition.compile(ctx)
        label = ctx.new_label()
        op = self.condition.op
        if op:
            ib = branch_ib_map[op]
            ctx.emit(ib(f"if_end{label}"))

        for statement in self.block:
            statement.compile(ctx)

        ctx.emit(IRSystem.ib.label(f"if_end{label}"))

    def __repr__(self):
        return f"if ({self.condition})"

def class_handler(name, block, context: IRSystem.BlockContext):
    """Handle class declarations."""
    print(f"Class: {name}")
    statements = block_handler(block, context)
    obj = ASTClass(name, statements)

    return obj

def class_extends_handler(name, parent, block, context):
    print(f"Class: {name}")
    obj = ASTClass(name, block)
    return obj

def func_handler(fname, args, block, context: IRSystem.BlockContext):
    """Handle function declarations."""
    print(f"Function {fname}")
    args_objs = block_handler(args, context=context)
    statements = block_handler(block, context=context)
    obj = ASTFunction(fname, args_objs, statements)
    return obj


def return_handler(value, context: IRSystem.BlockContext):
    """Handle return statements."""
    obj = ASTReturn(value)
    return obj


def call_handler(name, args, context: IRSystem.BlockContext):
    """Handle function calls."""
    print(f"Call: {name}, Args: {args}")
    obj = ASTFuncCall(name, args)
    return obj

def declaration_handler(name, var_type, context: IRSystem.BlockContext):
    """Handle variable declarations."""
    obj = ASTDeclaration(name, var_type)
    return obj

def asm_handler(inst, args, context: IRSystem.BlockContext):
    """Handle harcoded assembly"""
    code = []
    code.extend(block_handler(args, context=context))

    arg2 = context.pipe.pop()

    arg1 = context.pipe.pop()

    try:
        imi =  IMI[inst.upper()].value
    except KeyError:
        print(f"Error: unknown instruction: '{inst}'")
        return []  # or raise custom error
    return [(imi, arg1, arg2)]

OP_ASM_MAP = {}
def op_asm_handler(op_raw, args_raw, asm_raw, context: IRSystem.BlockContext):
    """Handle operators assembly"""
    code = []


    operator = op_raw
    loc = context.name
    print(f"Macro for op {operator} at {loc}, {len(asm_raw)} lines")

    if loc not in OP_ASM_MAP:
        OP_ASM_MAP[loc] = {operator: {}}
    OP_ASM_MAP[loc][operator] = {"asm": asm_raw, "args": args_raw}

    return []

def macro_assembler(macro, args, context):

    args.reverse()
    final = []
    
    sub_context = context.create_child_context("macro")
    block_handler(macro["args"], context=sub_context)
    
    for idx in range(0, len(macro["args"])):
        dec_name = sub_context.pipe.pop()
        dec_t = sub_context.get_type(dec_name)
        arg_t = context.get_type(args[idx])
        if dec_t != arg_t:
            print(f"Warning: Macro type not matching.")
        sub_context.update_variable_register(dec_name, args[idx])


    final.extend(block_handler(macro["asm"], context=sub_context))
    return final

def op_handler(left, op, right, context: IRSystem.BlockContext):
    namespace = context.get_global_prefix()

    obj = ASTBinaryOp(left, op, right)
    return obj
    '''
    if ltype in OP_ASM_MAP:
        if op in OP_ASM_MAP[ltype]:
            op_data = OP_ASM_MAP[ltype][op].copy()
        else:
            print(f"Operation not declared: {op}")
            return []
    else:
        print(f"No ops found for namespace: {ltype}")
        return []

    op_args = [left, right]

    #handle output reg
    #TODO: make this not suck
    if len(op_data["args"]) > len(op_args):
        new_r = context.vreg.new_vreg()
        context.vreg.set_register_type(new_r, ltype)
        op_args.insert(0, new_r)
        context.pipe.push(new_r)

    c = macro_assembler(op_data, op_args, context=context)      
    context.pipe.push(op, "oplist")
    return c
    '''

def multi_op_handler(left, op, right, context: IRSystem.BlockContext):
    namespace = context.get_global_prefix()

    #print(f"Multi op: {left}")
    r = block_handler(right, context=context)[0]

    l = block_handler(left, context=context)[0]

    
    obj = op_handler(l, op, r, context=context)

    return obj

def assignment_handler(left, right, context):
    source = block_handler(right, context=context)[0]
    target = block_handler(left, context=context)[0]
    a = ASTAssignment(source, target)
    return a

def array_declaration_handler(name, size, typ, context):
    code = []
    reg = context.vreg.new_vreg()
    offset = context.get_memory(int(size))
    code.append(IRSystem.ib.move(reg, context.memory_start_reg))
    code.append(IRSystem.ib.add(reg, offset))

    #TODO: malloc and set reg to ptr of arr[0]
    final_size = context.types.typemap[typ]['size'] * size
    context.update_variable_register(name, reg)
    context.set_variable_type(name, typ)
    print(f"Declared '{name}': {typ} with size {size} in {reg}")
    return code

def name_handler(base_name, context: IRSystem.BlockContext):
    r = block_handler(base_name, context=context)
    name = context.pipe.pop()
    try:
        j = int(name)
        return ASTStatic(j) 
    except ValueError:
        pass
    obj = ASTReference(name)
    return obj

def base_name_handler(oname, context: IRSystem.BlockContext):
    context.pipe.push(oname)

def child_handler(parent, child, context: IRSystem.BlockContext):
    reg = context.get_variable_register(parent) #register with pointer to parent
    #TODO: get memory map of class and calculate offsets for expressions or name for func calls (maybe later reduced to offset)
    context.pipe.push(reg)

def name_index_handler(name, index, context: IRSystem.BlockContext):
    print(f"ref index handler {name}")
    code = []

    n = context.vreg.new_vreg()

    f = block_handler(name, context=context)
    c = context.pipe.pop()

    i_reg = context.get_variable_register(index)
    code.append(IRSystem.ib.move(n, c))
    code.append(IRSystem.ib.add(n, i_reg or index))

    context.pipe.push(n)

    return code

def if_handler(cond, block, context: IRSystem.BlockContext):
    condition = block_handler(cond, context=context)[0]
    statements = block_handler(block, context=context)
    obj = ASTIfStatement(condition, statements)
    return obj

branch_ib_map = {
    "==": IRSystem.ib.jne,
    "!=": IRSystem.ib.je,
}
loop_counter = 0

def while_handler(cond, block, context: IRSystem.BlockContext):
    global loop_counter
    code = []
    c = block_handler(cond, context=context)
    code.extend(c)

    code.append(IRSystem.ib.label(f"while_start{loop_counter}"))
    op = context.pipe.pop("oplist")
    if op:
        ib = branch_ib_map[op]
        code.append(ib(f"while_end{loop_counter}").to_tuple)

    b = block_handler(block, context=context)

    code.extend(b)
    code.append(IRSystem.ib.jump(f"while_start{loop_counter}"))
    code.append(IRSystem.ib.label(f"while_end{loop_counter}"))
    loop_counter += 1
    return code

def static_string_handler(strr, context: IRSystem.BlockContext):
    code = []
    
    i = context.add_static(strr)
    context.pipe.push(i)
    return code

def static_handler(declaration, context: IRSystem.BlockContext):
    code = []
    r = block_handler(declaration, context=context)
    code.extend(r)
    name = context.pipe.pop()
    reg = context.add_static(name)
    context.pipe.push(reg)
    context.update_variable_register(name, reg)
    print(f"static {name}: {reg}")



def deref_handler(name, context: IRSystem.BlockContext):
    code = []
    
    code.extend(block_handler(name, context=context))

    vtype = context.get_type(name)
    vreg = context.pipe.pop()

    new_reg = context.vreg.new_vreg()
    c = macro_assembler(OP_ASM_MAP[vtype]["&"])
    code.extend(c)
    return code

def expr_handler(expr, context):
    #print("EXPR:", expr)
    r = block_handler(expr, context=context)
    return r[0]

def new_handler(declaration, context: IRSystem.BlockContext):
    #print(f"new: {declaration}")

    dec = block_handler(declaration, context=context)[0]
    obj = ASTNew(dec)
    return obj




# Token type constants
VARIABLE = "$"
WILDCARD = "*"
KEYWORD = "t"
SYMBOL = "b"
WORD = "t"
PUNCTUATION = "p"
BLOCK_START = "{"
BLOCK_END = "}"
PAREN_START = "("
PAREN_END = ")"
BRACKET_START = "["
BRACKET_END = "]"

#TODO: move these somewhere else
# Format: (pattern, groups, handler)
TEMPLATES = [
    # class $name extends $parent { *block }
    (
        [
            (WORD, "class"),
            (VARIABLE, "name"),
            (WORD, "extends"),
            (VARIABLE, "parent"),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["main", "asm_func"]))])
        ],
        ["main"],
        class_extends_handler
    ),
    
    # class $name { *block }
    (
        [
            (WORD, "class"),
            (VARIABLE, "name"),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["main", "asm_func", "_expr"]))])
        ],
        ["main"],
        class_handler
    ),
    
    # func $name( *args ) { *block }
    (
        [
            (WORD, "func"),
            (VARIABLE, "name"),
            (PAREN_START + PAREN_END, [(WILDCARD, ("args", ["decs"]))]),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["expr", "flow", "new", "asm"]))])
        ],
        ["main"],
        func_handler
    ),
    
    # return $value
    (
        [
            (WORD, "return"),
            (WILDCARD, ("value", ["name", "expr"]))
        ],
        ["flow"],
        return_handler
    ),
    
    # if ( *cond ) { *block }
    (
        [
            (WORD, "if"),
            (PAREN_START + PAREN_END, [(WILDCARD, ("cond", ["expr"]))]),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["expr", "flow", "decs"]))])
        ],
        ["flow"],
        if_handler
    ),
    
    # while ( *cond ) { *block }
    (
        [
            (WORD, "while"),
            (PAREN_START + PAREN_END, [(WILDCARD, ("cond", ["expr"]))]),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["expr", "flow", "decs"]))])
        ],
        ["flow"],
        while_handler
    ),
    
    # _asm_func $op (*args) { *asm }
    (
        [
            (WORD, "_asm_func"),
            (VARIABLE, "op"),
            (PAREN_START + PAREN_END, [(WILDCARD, ("args", ["decs"]))]),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("asm", ["asm"]))]),
        ],
        ["asm_func"],
        op_asm_handler
    ),
    
    # _asm $string
    (
        [
            (WORD, "_asm"),
            (PAREN_START + PAREN_END, [
                (VARIABLE, "inst"),
                (WILDCARD, ("args", ["name"]))
                ]),

        ],
        ["asm"],
        asm_handler
    ),
    
    # $name[$size]: $type
    (
        [
            (VARIABLE, "name"),
            (BRACKET_START + BRACKET_END, [(VARIABLE, "size")]),
            (KEYWORD, ":"),
            (VARIABLE, "type")
        ],
        ["decs"],
        array_declaration_handler
    ),
    

    # $name: $type
    (
        [
            (VARIABLE, "name"),
            (KEYWORD, ":"),
            (VARIABLE, "type")
        ],
        ["decs"],
        declaration_handler
    ),
    # new *dec
    (
        [
            (WORD, "new"),
            (WILDCARD, ("dec", ["decs"]))
        ],
        ["expr", "new"],
        new_handler
    ),
    # static *dec
    (
        [
            (WORD, "static"),
            (WILDCARD, ("dec", ["decs"]))
        ],
        ["expr", "static"],
        static_handler
    ),
    # (*expr)
    (
        [
            (PAREN_START + PAREN_END, (
                (WILDCARD, ("expr", ["_expr", "name"])),)
            )
        ],
        ["expr"],
        expr_handler,
    ),
    # *expr
    (
        [
            (WILDCARD, ("expr", ["_expr", "name"]))
        ],
        ["expr"],
        expr_handler,
    ),
    # *left = *right
    (
        [
            (WILDCARD, ("left", ["name", "new"])),
            (WORD, "="),
            (WILDCARD, ("right", ["expr"]))
        ],
        ["_expr"],
        assignment_handler,
    ),
    # *left $op *right
    (
        [
            (WILDCARD, ("left", ["expr"])),
            (VARIABLE, ("op", ["+", "-", "==", "&"])),
            (WILDCARD, ("right", ["expr"]))
        ],
        ["_expr"],
        multi_op_handler
    ),

    # *name(*args)
    (
        [
            (WILDCARD, ("name", ["base_name"])),
            (PAREN_START + PAREN_END, [(WILDCARD, ("args", ["name"]))]),
        ],
        ["expr"],
        call_handler
    ),
    
    # *name[$index]
    (
        [
            (WILDCARD, ("name", ["base_name"])),
            (BRACKET_START + BRACKET_END, [(VARIABLE, "index")]),
        ],
        ["name"],
        name_index_handler
    ),

    # *name
    (
        [
            (WILDCARD, ("name", ["base_name"])),
        ],
        ["name"],
        name_handler
    ),

    # $parent.$child
    (
        [
            (VARIABLE, "parent"),
            (SYMBOL, "."),
            (VARIABLE, "child"),
        ],
        ["base_name"],
        child_handler
    ),

    # $name
    (
        [
            (VARIABLE, "name")
        ],
        ["base_name"],
        base_name_handler
    ),


    # $string
    (
        [
            ("string", "str")
        ],
        ["expr"],
        static_string_handler
    ),


]


def get_parsed_templates(templates):
    parsed_templates = []
    
    for template_text, groups, handler in templates:
        parsed_templates.append(template_text)
    return parsed_templates

def get_template_handlers(templates):
    parsed_templates = []
    
    for template_text, groups, handler in templates:
        parsed_templates.append(handler)
    return parsed_templates

def get_template_groups(templates):
    parsed_templates = {}
    
    for idx, (template_text, groups, handler) in enumerate(templates):
        parsed_templates[idx] = groups
    return parsed_templates


def unknown_func(*args):
    print("Unknown handler for: ", *args)

@staticmethod
def block_handler(block, context=None):
    template_handlers = get_template_handlers(TEMPLATES)

    """Handle code blocks."""
    if context is None:
        context = IRSystem.BlockContext()
        
    nodes = []
    for idx in range(0, len(block)):
        item = block[idx]
        values, handler = [], unknown_func
        for key, val in item:
            if key == "#":
                handler = template_handlers[val]
            else:
                values.append(val)
            
        try:
            result = handler(*values, context)

            if result:
                nodes.append(result)
        except Exception as e:
            print(f"Error in {handler.__name__}: {e}")
            import traceback
            traceback.print_exc()
    return nodes

def generator(matches):
    ast = block_handler(matches)
    actx = AnalyzeContext("root")
    print("ANALYZING TREE")
    for node in ast:
        node.analyze(actx)
        print(f"Analyzed: {node}")

    print("COMPILING TREE")

    cctx = CodeGenContext("root")
    for node in ast:
        node.compile(cctx)
    return cctx.code

def matching_handler(tokens):
    template_handlers = get_template_handlers(TEMPLATES)
    parsed_templates = get_parsed_templates(TEMPLATES)
    template_groups = get_template_groups(TEMPLATES)

    return template_match(parsed_templates, template_groups, tokens, debug=True)