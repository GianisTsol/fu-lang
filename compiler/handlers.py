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
    code = block_handler(block, class_context)

    context.types[name] = {'size': 4}

    return code

def func_handler(name, args, block, context):
    """Handle function declarations."""
    name = context.get_global_prefix() + "." + name
    print(f"Function: {name}, Args: {args}")
    code = [IRSystem.ib.label(name).to_tuple()]
    func_context = context.create_child_context(name)
    
    # Assign argument registers
    for i in range(min(len(args), ARGS_REGISTERS)):
        arg_name = args[i][1][1]
        func_context.update_variable_register(arg_name, i + 1)
    
    # Handle overflow args
    if len(args) > ARGS_REGISTERS:
        for i in range(ARGS_REGISTERS, len(args)):
            new_reg = func_context.vreg.new_vreg()
            func_context.update_variable_register(args[i][1][1], new_reg)
            code.append(IRSystem.ib.pop(new_reg).to_tuple())

    block_code = block_handler(block, func_context)

    code.extend(block_code)
    
    # Return handling
    ret_reg = func_context.vreg.new_vreg()
    code.append(IRSystem.ib.pop(ret_reg).to_tuple())
    code.append(IRSystem.ib.jump(ret_reg).to_tuple())
    
    return code


def return_handler(value, context):
    """Handle return statements."""
    reg = context.get_variable_register(value)
    if reg:
        return [IRSystem.ib.move('v0', reg).to_tuple()]
    try:
        return [IRSystem.ib.move('v0', int(value)).to_tuple()]
    except ValueError:
        print(f"Warning: Return value '{value}' not found")
        return []


def call_handler(name, args, context: IRSystem.BlockContext):
    """Handle function calls."""
    print(f"Call: {name}, Args: {args}")
    code = []
    
    # Pass args in registers
    for i in range(min(len(args), ARGS_REGISTERS)):
        arg_name = args[i][0][1]
        reg = context.get_variable_register(arg_name)
        if reg:
            code.append(IRSystem.ib.move(f"v{i + 1}", reg).to_tuple())
    
    # Overflow on stack
    if len(args) > ARGS_REGISTERS:
        for i in range(ARGS_REGISTERS, len(args)):
            arg_name = args[i][0][1]
            reg = context.get_variable_register(arg_name)
            if reg:
                code.append(IRSystem.ib.push(reg).to_tuple())
    
    code.append(IRSystem.ib.push("INST_PTR").to_tuple())
    code.append(IRSystem.ib.jump(name).to_tuple())
    context.types.append(name)
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
    reg = context.vreg.new_vreg()
    context.update_variable_register(name, reg)
    context.set_variable_type(name, var_type)
    print(f"Declared '{name}': {var_type} in {reg}")
    return []


def asm_handler(string, context):
    """Handle harcoded assembly"""
    string = string.replace(",", " ")

    args = string.split()
    inst = args[0]
    try:
        imi =  IMI[inst.upper()].value
    except KeyError:
        print(f"Error: unknown instruction: '{inst}'")
        return []  # or raise custom error
    return [(imi, args[1], args[2])]

OP_ASM_MAP = {}
def op_asm_handler(op, asm, context: IRSystem.BlockContext):
    """Handle operators assembly"""

    operator = None
    args = []
    result = None
    ridx = None
    d = op.split()
    for k, v in enumerate(d):
        if k == ridx:
            continue
        if "$" in v:
            args.append(v.replace("$", ""))
        elif "->" in v:
            ridx = k + 1
        elif (v[0], v[-1]) == ("{", "}"):
            operator = v[1:len(v) - 1]
    if ridx:
        result = d[ridx].replace("$", "")

    loc = context.name

    print(f"Declartion for op {operator}: ({args}) -> {result} at {loc}")

    lines = block_handler(asm)

    if loc not in OP_ASM_MAP:
        OP_ASM_MAP[loc] = {operator: {}}
    OP_ASM_MAP[loc][operator] = {"asm": lines, "args": args, "result": result}

    return []

def op_handler(left, op, right, context: IRSystem.BlockContext):
    print(f"Handling op: {op}")
    namespace = context.get_global_prefix()

    ltype = context.get_variable_type(left)
    loc = ltype

    if loc in OP_ASM_MAP:
        if op in OP_ASM_MAP[loc]:
            op_data = OP_ASM_MAP[loc][op].copy()
        else:
            print(f"Operation not declared: {op}")
            return []
    else:
        print(f"No ops found for namespace: {loc}")
        return []

    rtype = context.get_variable_type(right)

    left_reg = context.get_variable_register(left)
    right_reg = context.get_variable_register(right)

    try:
        int(right)
        right_reg = right
    except:
        pass
    
    regs = [left_reg, right_reg]

    result_reg = None
    if op_data["result"]:
        result_reg = context.vreg.new_vreg()
    
    print("AAA")
    context.pipe.append(result_reg)

    final = []

    for line in op_data["asm"]:
        nl = []
        for word in line:
            if type(word) == int:
                nl.append(word)
                continue
            elif result_reg and (op_data["result"] in word):
                nl.append(result_reg)
                continue
            else:
                for i, arg in enumerate(op_data["args"]):
                    if word in arg:
                        nl.append(regs[i])
                        break

        final.append(tuple(nl))
                    
    return final

def multi_op_handler(left, op, right, context: IRSystem.BlockContext):
    print(f"Multi op: {op}")
    namespace = context.get_global_prefix()

    r = block_handler(right, context=context)

    final = op_handler(left, op, context.pipe.pop(), context=context)
    r.extend(final)


    return r

def array_declaration_handler(name, size, typ, context):
    code = []
    reg = context.vreg.new_vreg()
    offset = context.get_memory(int(size))
    code.append(IRSystem.ib.move(reg, context.memory_start_reg).to_tuple())
    code.append(IRSystem.ib.add(reg, offset).to_tuple())

    #TODO: malloc and set reg to ptr of arr[0]
    final_size = context.types[typ]['size'] * size
    context.update_variable_register(name, reg)
    context.set_variable_type(name, typ)
    print(f"Declared '{name}': {typ} with size {size} in {reg}")
    return code

def ref_handler(name, context):
    reg = context.get_variable_register(name)
    context.update_variable_register(f"temp", reg)


def ref_index_handler(name, index, context: IRSystem.BlockContext):
    print("ref index handler")
    code = []
    n = context.vreg.new_vreg()
    c = context.get_variable_register(name)

    i_reg = context.get_variable_register(index)
    code.append(IRSystem.ib.move(n, c).to_tuple())
    code.append(IRSystem.ib.add(n, i_reg or index).to_tuple())
    code.append(IRSystem.ib.load(n, n).to_tuple())

    context.update_variable_register(f"temp", n)

    return code

def if_handler(cond, block, context: IRSystem.BlockContext):
    pass

def while_handler(cond, block, context):
    code = []
    r = block_handler(cond, context=context)
    print(r)


def expr_handler(expr, context):
    print("EXPR:", expr)
    exit()

    try:
        val = int(expr)
        context.pipe.append(val)
    except ValueError:
        pass



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
            (BLOCK_START + BLOCK_END, [(WILDCARD, "block")])
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
            (BLOCK_START + BLOCK_END, [(WILDCARD, ("block", ["expr", "flow", "decs"]))])
        ],
        ["main"],
        func_handler
    ),
    
    # return $value
    (
        [
            (WORD, "return"),
            (VARIABLE, "value")
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
    
    # _op_asm $op { *asm }
    (
        [
            (WORD, "_op_asm"),
            (VARIABLE, "op"),
            (BLOCK_START + BLOCK_END, [(WILDCARD, "asm")])
        ],
        ["asm"],
        op_asm_handler
    ),
    
    # _asm $string
    (
        [
            (WORD, "_asm"),
            (VARIABLE, "string")
        ],
        ["asm"],
        asm_handler
    ),
    
    # new $name[$size]: $type
    (
        [
            (WORD, "new"),
            (VARIABLE, "name"),
            (BRACKET_START + BRACKET_END, [(VARIABLE, "size")]),
            (KEYWORD, ":"),
            (VARIABLE, "type")
        ],
        ["decs"],
        array_declaration_handler
    ),
    
    # new $name: $type
    (
        [
            (WORD, "new"),
            (VARIABLE, "name"),
            (KEYWORD, ":"),
            (VARIABLE, "type")
        ],
        ["decs"],
        declaration_handler
    ),

    # *left $op *right
    (
        [
            (WILDCARD, ("left", ["name"])),
            (VARIABLE, ("op", ["=", "+", "-", "=="])),
            (WILDCARD, ("right", ["name", "expr"]))
        ],
        ["expr"],
        declaration_handler
    ),

    # *name[$index]
    (
        [
            (WILDCARD, ("name", ["base_name"])),
            (BRACKET_START + BRACKET_END, [(VARIABLE, "index")]),
        ],
        ["name"],
        declaration_handler
    ),

    # $parent.$child
    (
        [
            (VARIABLE, "parent"),
            (SYMBOL, "."),
            (VARIABLE, "child"),
        ],
        ["base_name, name"],
        declaration_handler
    ),

    # $name
    (
        [
            (VARIABLE, "name")
        ],
        ["base_name, name"],
        declaration_handler
    ),

]


TEMPLATES_HANDLERS = [

("class $name { *block }", ["main"],class_handler),

("func $name( *args ) { *block }", ["main"], func_handler),
("return $value", ["main"], return_handler),

("if ( *cond ) { *block }", ["main"], if_handler),
("while ( *cond ) { *block }", ["main"], while_handler),


("_op_asm $op { *asm }", ["main"], op_asm_handler),

("_asm $string", ["main"], asm_handler),

("new $name[$size]: $type", ["main"], array_declaration_handler),
("new $name: $type", ["main"], declaration_handler),

#("$left $op *right", multi_op_handler),

("$expr", expr_handler),

("(**expr)", expr_handler)
]

T = [
("$name.$child( *args )", child_call_handler),

("$name( *args )", call_handler),



("$name", ref_handler),
("$name[ $index ]", ref_index_handler),


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