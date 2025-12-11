"""Template handler functions for code generation."""

from config import ARGS_REGISTERS
from ir_system import IRSystem
from reader import Reader
from parser import parse as parse_tokens

from config import IMI


def class_handler(name, block, context: IRSystem.BlockContext):
    """Handle class declarations."""
    print(f"Class: {name}")
    class_context = context.create_child_context(name)
    code = block_handler(block, class_context)

    context.types.append(name)

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
def op_asm_handler(op, args, asm, context: IRSystem.BlockContext):
    """Handle operators assembly"""

    loc = context.name

    print(f"Declartion for op: {op} at {loc}")

    lines = block_handler(asm)

    if loc in OP_ASM_MAP:
        OP_ASM_MAP[loc][op] = lines
    else:
        OP_ASM_MAP[loc] = {op: lines}

    return []

def op_handler(left, op, right, context: IRSystem.BlockContext):
    print(f"Handling op: {op}")
    namespace = context.get_global_prefix()

    ltype = context.get_variable_type(left)
    rtype = context.get_variable_type(right)

    left_reg = context.get_variable_register(left)
    right_reg = context.get_variable_register(right)

    try:
        int(right)
        right_reg = right
    except:
        pass

    loc = ltype
    if loc in OP_ASM_MAP:
        if op in OP_ASM_MAP[loc]:
            lines = OP_ASM_MAP[loc][op]
        else:
            print(f"Operation not declared: {op}")
            return []
    else:
        print(f"No ops found for namespace: {loc}")
        return []
    
    final = []
    result_reg = context.vreg.new_vreg()
    context.update_variable_register("op_temp", result_reg)
    for line in lines:
        nl = []
        for i in line:
            if type(i) is str:
                s = i.replace("left", left_reg).replace("right", right_reg).replace("result", result_reg)
                nl.append(s)
            else:
                nl.append(i)
        final.append(tuple(nl))
    return final

def multi_op_handler(left, op, right, context: IRSystem.BlockContext):
    print(f"Multi op: {op}")
    namespace = context.get_global_prefix()

    r = block_handler(right, context=context)

    ltype = context.get_variable_type(left)
    rtype = context.get_variable_type("op_temp")

    left_reg = context.get_variable_register(left)
    right_reg = context.get_variable_register("op_temp")

    try:
        int(right)
        right_reg = right
    except:
        pass

    loc = ltype
    if loc in OP_ASM_MAP:
        if op in OP_ASM_MAP[loc]:
            lines = OP_ASM_MAP[loc][op]
        else:
            print(f"Operation not declared: {op}")
            return []
    else:
        print(f"No ops found for namespace: {loc}")
        return []
    
    final = []
    result_reg = context.vreg.new_vreg()
    context.update_variable_register("op_temp", result_reg)
    for line in lines:
        nl = []
        for i in line:
            if type(i) is str:
                s = i.replace("left", left_reg).replace("right", right_reg).replace("result", result_reg)
                nl.append(s)
            else:
                nl.append(i)
        final.append(tuple(nl))
    r.extend(final)
    print("OP ASM: ", r)
    return r


TEMPLATES_HANDLERS = [
("class $name { *block }", class_handler),
("call $name.$child( *args )", child_call_handler),

("func $name( *args ) { *block }", func_handler),
("return $value", return_handler),

("call $name( *args )", call_handler),

("$name: $type", declaration_handler),
("new $name: $type", declaration_handler),

("$left $op ( *right )", multi_op_handler),

("$left $op $right", op_handler),

("_op_asm $op ( *args ) { *asm }", op_asm_handler),

("_asm $string", asm_handler),

]


def get_parsed_templates():
    """Register a new template with its handler function."""
    parsed_templates = []
    
    for template_text, handler in TEMPLATES_HANDLERS:
        reader = Reader()
        reader.load_text(template_text)
        parsed = parse_tokens(reader)
        parsed_templates.append(parsed)
    return parsed_templates

def get_template_handlers():
    """Register a new template with its handler function."""
    parsed_templates = []
    
    for template_text, handler in TEMPLATES_HANDLERS:
        parsed_templates.append(handler)
    return parsed_templates

template_handlers = get_template_handlers()
parsed_templates = get_parsed_templates()


def unknown_func(*args):
    print("Unknown handler for: ", *args)

@staticmethod
def block_handler(block, context=None):
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
                print(result)
                instructions.extend(result)
        except Exception as e:
            print(f"Error in {handler.__name__}: {e}")
            import traceback
            traceback.print_exc()
    return instructions

