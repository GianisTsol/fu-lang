"""Template handler functions for code generation."""

from config import ARGS_REGISTERS
from ir_system import IRSystem
from reader import Reader
from parser import parse as parse_tokens
from template_matcher import template_match

from config import IMI

def class_handler(name, block, context: IRSystem.BlockContext):
    """Handle class declarations."""
    print(f"Class: {name}")
    class_context = context.create_child_context(name)
    context.types.typemap[name] = {'size': 4}

    code = block_handler(block, class_context)
    #context.types.typemap[name] = {'size': 4, class_context.variables}


    return code

def func_handler(fname, args, block, context: IRSystem.BlockContext):
    """Handle function declarations."""
    name = context.get_global_prefix() + "." + fname
    
    return_type = None
    return_types = context.pipe.pop_all(pipe="return_type")
    if return_types and len(return_types) >= 1:
        for i in return_types[1:]:
            if i != return_types[0]:
                return_type = None
                print(f"Error: Function returns too many types: {return_types}")
                return
        return_type = return_types[0]
    if not return_type:
        return_type = "void"
    context.set_variable_type(fname, return_type)

    print(f"Function: {name} -> {return_type}, Args: {args}")

    func_context = context.create_child_context(name)

    code = [IRSystem.ib.label(name).to_tuple()]

    arg_code = block_handler(args, func_context)

    # Assign argument registers
    for i in range(min(len(args), ARGS_REGISTERS)):
        arg_name = func_context.pipe.pop()
        func_context.update_variable_register(arg_name, i + 1)
    
    # Handle overflow args
    if len(args) > ARGS_REGISTERS:
        for i in range(ARGS_REGISTERS, len(args)):
            arg_name = func_context.pipe.pop()
            new_reg = func_context.vreg.new_vreg()
            func_context.update_variable_register(arg_name, new_reg)
            code.append(IRSystem.ib.pop(new_reg).to_tuple())

    block_code = block_handler(block, func_context)

    code.extend(block_code)
    
    # Return handling
    ret_reg = func_context.vreg.new_vreg()
    code.append(IRSystem.ib.pop(ret_reg).to_tuple())
    code.append(IRSystem.ib.jump(ret_reg).to_tuple())
    
    return code


def return_handler(value, context: IRSystem.BlockContext):
    """Handle return statements."""
    code = []

    code.extend(block_handler(value, context=context))

    reg = context.pipe.pop()

    reg_type = context.get_type(reg)
    context.pipe.push(reg_type, pipe="return_handler")

    code.append(IRSystem.ib.move('v0', reg).to_tuple())
    return code


def call_handler(name, args, context: IRSystem.BlockContext):
    """Handle function calls."""
    print(f"Call: {name}, Args: {args}")
    code = []
    

    a = block_handler(args, context=context)
    code.extend(a)
    # Pass args in registers
    for i in range(min(len(args), ARGS_REGISTERS)):
        reg = context.pipe.pop()
        if reg:
            code.append(IRSystem.ib.move(f"v{i + 1}", reg).to_tuple())
    
    # Overflow on stack
    if len(args) > ARGS_REGISTERS:
        for i in range(ARGS_REGISTERS, len(args)):
            reg = context.pipe.pop()
            if reg:
                code.append(IRSystem.ib.push(reg).to_tuple())
    
    nn = block_handler(name, context=context)
    code.extend(nn)
    code.append(IRSystem.ib.push("INST_PTR").to_tuple())
    code.append(IRSystem.ib.jump(context.pipe.pop()).to_tuple())

    context.pipe.push("v0")
    return code


def child_call_handler(name, child, args, context):
    """Handle method calls."""
    print(f"Child Call: {name}.{child}")
    code = []
    
    for i in range(min(len(args), ARGS_REGISTERS)):
        arg_name = args[i][0][1]
        reg = context.get_variable_register(arg_name)
        if reg:
            code.append(IRSystem.ib.move(f"v{i + 1}", reg).to_tuple())
    
    if len(args) > ARGS_REGISTERS:
        for i in range(ARGS_REGISTERS, len(args)):
            arg_name = args[i][0][1]
            reg = context.get_variable_register(arg_name)
            if reg:
                code.append(IRSystem.ib.push(reg).to_tuple())

    code.append(IRSystem.ib.push("INST_PTR").to_tuple())
    code.append(IRSystem.ib.jump(f"global.{name}.{child}").to_tuple())
    
    return code


def addto_handler(left, right, context):
    """Handle += operator."""
    print(f"Add: {left} += {right}")
    left_reg = context.get_variable_register(left)
    right_reg = context.get_variable_register(right)
    
    if not left_reg:
        print(f"Warning: '{left}' not found")
        return []
    
    return [IRSystem.ib.add(left_reg, right_reg or right).to_tuple()]

def declaration_handler(name, var_type, context: IRSystem.BlockContext):
    """Handle variable declarations."""
    context.set_variable_type(name, var_type)
    context.pipe.push(name)
    print(f"Declared '{name}': {var_type}")
    return []


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
    
    sub_context = context.create_child_context(f"op_{op_raw}")
    block_handler(args_raw, context=sub_context)
    
    args = [sub_context.pipe.pop() for i in args_raw]

    operator = op_raw
    loc = context.name
    print(f"Macro for op {operator} at {loc}, {len(asm_raw)} lines")

    if loc not in OP_ASM_MAP:
        OP_ASM_MAP[loc] = {operator: {}}
    OP_ASM_MAP[loc][operator] = {"asm": asm_raw, "args": args}

    return []

def op_handler(left, op, right, context: IRSystem.BlockContext):
    namespace = context.get_global_prefix()

    ltype = context.get_type(left)
    rtype = context.get_type(right)
    print(f"Handling op: {left}:{ltype} {op} {right}:{rtype}")
    if not ltype or not rtype:
        print("Error: op without types? Seriously?..")
        return
    if ltype != rtype:
        print("Warning: op types not matching! ")
    print(f"Using type {ltype}")

    if ltype in OP_ASM_MAP:
        if op in OP_ASM_MAP[ltype]:
            op_data = OP_ASM_MAP[ltype][op].copy()
        else:
            print(f"Operation not declared: {op}")
            return []
    else:
        print(f"No ops found for namespace: {ltype}")
        return []


    try:
        right = int(right)
    except:
        pass
    

    final = []

    sub_context = context.create_child_context(f"op_{op}")
    sub_context.update_variable_register(op_data["args"][1], left)
    sub_context.update_variable_register(op_data["args"][0], right)

    if len(op_data["args"]) >= 3:
        new_r = sub_context.vreg.new_vreg()
        sub_context.vreg.set_register_type(new_r, ltype)
        sub_context.update_variable_register(op_data["args"][2], new_r)

        context.pipe.push(new_r)

    final.extend(block_handler(op_data["asm"], context=sub_context))
                    
    return final

def multi_op_handler(left, op, right, context: IRSystem.BlockContext):
    namespace = context.get_global_prefix()


    r_code = block_handler(right, context=context)

    l_code = block_handler(left, context=context)

    l, r = context.pipe.pop(), context.pipe.pop()

    op_code = op_handler(l, op, r, context=context)


    return [*r_code, *l_code, *op_code]

def array_declaration_handler(name, size, typ, context):
    code = []
    reg = context.vreg.new_vreg()
    offset = context.get_memory(int(size))
    code.append(IRSystem.ib.move(reg, context.memory_start_reg).to_tuple())
    code.append(IRSystem.ib.add(reg, offset).to_tuple())

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
        val = int(name)
        context.pipe.push(val)
        print(f"IMMEDIATE (int): {val}")

    except ValueError:
        reg = context.get_variable_register(name)
        rtype = context.get_type(reg)
        context.pipe.push(reg)
        print(f"NAME: {base_name} in reg {reg}:{rtype}")
    return r

def base_name_handler(oname, context: IRSystem.BlockContext):
    context.pipe.push(oname)

def child_handler(parent, child, context: IRSystem.BlockContext):
    reg = context.get_variable_register(parent) #register with pointer to parent
    #TODO: get memory map of class and calculate offsets for expressions or name for func calls (maybe later reduced to offset)
    context.pipe.push(reg)

def name_index_handler(name, index, context: IRSystem.BlockContext):
    print("ref index handler")
    code = []

    n = context.vreg.new_vreg()

    f = block_handler(name, context=context)
    c = context.pipe.pop()

    i_reg = context.get_variable_register(index)
    code.append(IRSystem.ib.move(n, c).to_tuple())
    code.append(IRSystem.ib.add(n, i_reg or index).to_tuple())

    context.pipe.push(n)

    return code

def if_handler(cond, block, context: IRSystem.BlockContext):
    code = []
    c = block_handler(cond, context=context)
    res = context.pipe.pop()

    code.extend(c)

    b = block_handler(block, context=context)

    code.extend(b)

    return code

def while_handler(cond, block, context):
    code = []
    c = block_handler(cond, context=context)
    res = context.pipe.pop()
    
    code.extend(c)

    b = block_handler(block, context=context)

    code.extend(b)

    return code

def static_string_handler(strr, context: IRSystem.BlockContext):
    code = []
    
    i = context.add_static(strr)
    context.pipe.push(f"s{i}")
    return code

def deref_handler(name, context: IRSystem.BlockContext):
    code = []
    
    code.extend(block_handler(name, context=context))
    v = context.vreg.new_vreg()
    c = op_handler(context.pipe.pop(), "&", v, context=context)
    code.extend(c)
    return code

def expr_handler(expr, context):
    #print("EXPR:", expr)
    r = block_handler(expr, context=context)
    return r

def new_handler(declaration, context: IRSystem.BlockContext):
    code = []
    r = block_handler(declaration, context=context)
    code.extend(r)
    name = context.pipe.pop()
    reg = context.vreg.new_vreg()
    context.update_variable_register(name, reg)
    print(f"new {name}: {reg}")




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

# Format: (pattern, groups, handler)
TEMPLATES = [
    # class $name { *block }
    (
        [
            (WORD, "class"),
            (VARIABLE, "name"),
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["main", "asm_func"]))])
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
        ["new"],
        new_handler
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
    # *left $op *right
    (
        [
            (WILDCARD, ("left", ["expr"])),
            (VARIABLE, ("op", ["=", "+", "-", "==", "&"])),
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
        
    instructions = []
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
                instructions.extend(result)
        except Exception as e:
            print(f"Error in {handler.__name__}: {e}")
            import traceback
            traceback.print_exc()
    return instructions

def matching_handler(tokens):
    template_handlers = get_template_handlers(TEMPLATES)
    parsed_templates = get_parsed_templates(TEMPLATES)
    template_groups = get_template_groups(TEMPLATES)

    return template_match(parsed_templates, template_groups, tokens, debug=True)