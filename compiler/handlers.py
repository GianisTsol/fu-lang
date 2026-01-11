"""Template handler functions for code generation."""

from config import ARGS_REGISTERS
from ir_system import IRSystem
from config import IMI

from ast_definitions import *

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
        return None  # or raise custom error
    #return [(imi, arg1, arg2)]

OP_ASM_MAP = {}
def op_asm_handler(op_raw, args_raw, asm_raw, context: IRSystem.BlockContext):
    """Handle operators assembly"""
    code = []


    operator = op_raw
    loc = context.name
    print(f"Macro for op {operator} at {loc}, {len(asm_raw)} lines")
    '''
    if loc not in OP_ASM_MAP:
        OP_ASM_MAP[loc] = {operator: {}}
    OP_ASM_MAP[loc][operator] = {"asm": asm_raw, "args": args_raw}
    '''
    args = block_handler(args_raw, context)
    b = block_handler(asm_raw, context)
    return ASTMacro(op_raw, args, b)

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

    obj = None
    if op in ["/", "*", "+", "-"]:
        obj = ASTBinaryOp(left, op, right)
    elif op in ["==", ">", "<", "<=", ">="]:
        obj = ASTComparison(left, op, right)
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
    print(f"Array '{name}': {typ} with size {size}")

    size = int(size)
    return ASTDeclaration(name, typ, size)

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
    n = block_handler(name, context=context)[0]

    name = n.name
    index = int(index)

    return ASTReference(name, index=index)

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

def generator(ast):
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
