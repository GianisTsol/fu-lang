from ir_system import IRSystem, Instruction, Type, Address, TypedOperand, Lifetimes, OperandGroup
from config import IMI, ARGS_REGISTERS

void = Type(0, False)
usize = Type(8, False)
isize = Type(8, True)
u8    = Type(1, False)


class FuType(Type):
    def __init__(self, name, obj):
        self.name = name
        self.obj = obj

    def is_primitive(self):
        if isinstance(self.obj, Type):
            return True
        return False
    
    def size(self):
        if self.is_primitive():
            return self.obj.size
        else:
            print(self.obj)
            return self.obj.size()
    
    def reduce(self):
        if self.is_primitive():
            return self.obj
        else:
            return self.obj.reduce()
    def __repr__(self):
        return f"vFu:<{self.name}:{self.obj}>"


VTYPES = {
    "void": FuType("void", void),
    "usize": FuType("usize", usize),
    "u8": FuType("u8", u8),
    "isize": FuType("isize", isize),
    "type": FuType("type", void),
    "ptr": FuType("ptr", usize),

}

class SymbolTable:
    def __init__(self, name):
        self.name = name

        self.stack = []
        self.table = {}
    
    def push(self, name):
        self.stack.append(name)
        self.set_nested(self.table, [*self.stack, name], {})
    
    def pop(self):
        if self.stack:
            self.stack.pop()
        

    def set_nested(self, d, keys, value):
        for k in keys[:-1]:
            d = d.setdefault(k, {})
        d[keys[-1]] = value

    def get_nested(self, d, keys):
        for k in keys:
            d = d[k]
        return d

    def add(self, name, data):
        self.set_nested(self.table, [*self.stack, name], data)
    
    def get(self, name, offset=0):
        if offset > len(self.stack):
            return None
        t = self.get_nested(self.table, self.stack[:(len(self.stack)-offset)])

        if name in t:
            return t[name]
        
        return self.get(name, offset=offset+1)

class AnalyzeContext:
    def __init__(self, name):
        self.name = name

        self.parent = None

        self.loc = []
        self.symbols = SymbolTable(name)

    def push(self, name):
        self.symbols.push(name)
        return self

    def pop(self):
        self.symbols.pop()
        return self

    def add_object(self, name, obj):
        #print(f"New precompile symbol: {name}")
        self.symbols.add(name, obj)
    
    def get_object(self, name):
        return self.symbols.get(name)

class CodeGenVar:
    def __init__(self, name, vtype):
        self.name = name
        self.vtype: Type = vtype
        self.location = None
    
    def __repr__(self):
        return f"{self.location}:{self.size} ({self.name})"
    
class CodeGenContext:
    stack_ptr_reg = 7
    def __init__(self, name="root", parent=None):
        self.vars = {}
        self.static = {}

        self.stack_offset = 0

        self.code = []

        self.label_count = 0
        self.reg_count = 0

        self.prev = parent
        if self.prev:
            self.reg_count = self.prev.reg_count
            self.label_count = self.prev.label_count

    def push(self, name="null"):
        a = CodeGenContext(name, self)
        return a
    
    def pop(self):
        if not self.prev:
            print("Error: Cant pop first context.")
            return
        self.prev.code.extend(self.code)
        self.prev.label_count = self.label_count
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
    
    def new_var(self, name, vtype, length):
        if name in self.vars.keys():
            print(f"Error: Variable already registered: {name}")
            return None
        print("NEW VAR", name, "W TYPE: ", vtype)
        self.vars[name] = CodeGenVar(name, vtype)
        self.vars[name].size = vtype.size() * length

    def get_memory(self, vtype):
        a = TypedOperand(vtype.reduce())
        self.stack_ptr_reg += 1
        self.stack_offset += vtype.size()
        #self.emit(IRSystem.ib.add(Register(self.stack_ptr_reg), Register(self.stack_ptr_reg), Immediate(size)))
        return a

    def get_size(self, name):
        if name in self.vars:
            return self.vars[name].size
    
    def new_label(self):
        self.label_count += 1
        return self.label_count

    def new_reg(self, vtype):
        self.reg_count += 1
        if self.stack_ptr_reg == self.reg_count:
            self.reg_count += 1
        return TypedOperand(vtype.reduce())

    def update_variable_location(self, name, location):
        a = self.get_var(name)
        if not a:
            print(f"Error: Variable not found: {name}")
            return None
        a.location = location

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
            self.vtype = FuType("static", Type(self._bytes_required(data), signed=True))
        
        self.data = data
        self.store = None

    def compile(self, ctx):
        self.store = ctx.get_memory(self.vtype)
        self.store.lifetime = Lifetimes.STATIC #todo: define lifetypes better
        self.store.data = self.data

    def _bytes_required(self, v, signed=True):
        if signed:
            bits = (v.bit_length() + 1) if v >= 0 else ((-v - 1).bit_length() + 1)
        else:
            if v < 0:
                raise ValueError("Unsigned representation cannot be negative")
            bits = v.bit_length()
        return max(1, (bits + 7) // 8)
        
    def reduce(self):
        return self.store
    
    def __repr__(self):
        return f"static {self.data}: {self.vtype}"

class ASTReference:
    def __init__(self, name, index=None, space = None):
        self.name = name
        self.vtype: Type = None
        self.location = None
        self.index = index


    def analyze(self, ctx: AnalyzeContext):
        decl = ctx.get_object(self.name)
        if self.index:
            self.index.analyze(ctx)
        if decl == None:
            print('>'.join(ctx.symbols.stack))
            print(f"Error: Unknown object ({self.name})[{self.index}] {decl}")
            exit(1)
        self.vtype = decl["obj"].vtype


    def compile(self, ctx: CodeGenContext):
        var: CodeGenVar = ctx.get_var(self.name)
        if self.index is not None:
            vtable = ctx.get_var(self.vtype.name)
            if not vtable:
                print(f"Error: vtable not found for type {self.vtype.name}")
                print(ctx)
                exit(1)

            if "__index__" not in vtable:
                print('>'.join(ctx.symbols.stack))
                print(f"Error: {self.vtype.name} ({self.name}) does not support indexing.")
                exit(1)
            macro = vtable["__index__"]["obj"]
            macro.assemble([self.index.reduce()], ctx)
        self.location = var.location

    def reduce(self):
        return self.location
        #return TypedOperand(self.location, self.vtype.reduce())

    def __repr__(self):
        return f"ref {self.name}[{self.index}]: {self.vtype} ({self.location})"

class AstTarget(ASTNode):
    def __init__(self, name, block: list[ASTNode]):
        self.name = name
    def analyze(self, ctx):
        self.parent.analyze(ctx)
        ctx.push(self.parent.name)
        self.child.analyze(ctx)
        ctx.pop()
    def compile(self, ctx):
        ctx.push(self.parent.name)
        self.child.compile(ctx)
        
        ctx.pop()

class ASTAtrribute(ASTNode):
    def __init__(self, parent: ASTReference, child: ASTReference):
        self.parent = parent
        self.child = child
        self.vtype = child.vtype
    def analyze(self, ctx):
        self.parent.analyze(ctx)
        ctx.push(self.parent.name)
        self.child.analyze(ctx)
        ctx.pop()
    def compile(self, ctx):
        ctx.push(self.parent.name)
        self.child.compile(ctx)
        
        ctx.pop()

class ASTClass(ASTNode):
    def __init__(self, name, statements):
        self.name = name
        self.statements = statements

        self.size_ = 1


    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)
        for statement in self.statements:
            statement.analyze(ctx)

            if isinstance(statement, ASTDeclaration):
                self.size += statement.size
        ctx = ctx.pop()
        vtable = ctx.get_object(self.name)
        if not vtable:
            print(f"Error: Table for type {self.vtype.name} not found.")
            exit(1)
        if self.name in VTYPES:
            self.vtype = VTYPES[self.name]
        else:
            VTYPES[self.name] = FuType(self.name, self)

            if "__dec__" not in vtable:
                print(f"Error: no definition (__dec__ macro) for class {self.name}")
                exit(1)
            macro = vtable["__dec__"]["obj"]
            self.vtype = macro.vtype

    def size(self):
        return self.size_

    def reduce(self):
        return self.vtype.reduce()
    def compile(self, context: CodeGenContext):
        """Handle class declarations."""
        ctx = context.push(self.name)

        for statement in self.statements:
            statement.compile(ctx)
        print(f"Succesfully compiled Class: {self.name}")
        ctx = ctx.pop()
        return True

    def __str__(self):
        inner = ", ".join(str(s) for s in self.statements)
        return f"Class {self.name} ([{inner}])"


class ASTReturn:
    def __init__(self, ref: ASTReference):
        self.ref = ref
        self.vtype = None

    def analyze(self, ctx):
        self.ref.analyze(ctx)
        self.vtype = self.ref.vtype
    
    def compile(self, ctx):
        self.ref.compile(ctx)
        assert type(self.vtype) == FuType
        ctx.emit(IRSystem.ib.move(TypedOperand(self.vtype.reduce(), lifetime=Lifetimes.RETURN), self.ref.reduce()))
    
class ASTFunction(ASTNode):
    def __init__(self, name, args, statements):
        self.name = name
        self.args: list[ASTDeclaration] = args
        self.statements = statements
        self.vtype = None

        self.return_operand = None
    
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
                return_types.append(statement.vtype)
        print("analyzing return types")
        if return_types and len(return_types) >= 1:
            for i in return_types[1:]:
                if i != return_types[0]:
                    return_type = None
                    print(f"Error: Function returns too many types: {return_types}")
                    return
            return_type = return_types[0]
        if not return_type:
            return_type = void
        self.vtype = return_type
        ctx = ctx.pop()
        ctx.add_object(self.name, {"obj": self})

        
    def compile(self, context: CodeGenContext):
        context = context.push(self.name)
        context.emit(IRSystem.ib.label(self.name))

        arg_group = OperandGroup()
        for idx, arg in enumerate(self.args):
            arg.compile(context)

            loc = TypedOperand(arg.vtype.reduce(), lifetime=Lifetimes.ARG, group=arg_group)
            context.update_variable_location(arg.name, loc)
            arg.location = loc
            
        for statement in self.statements:
            statement.compile(context)
        
        context.emit(IRSystem.ib.ret())
        context = context.pop()
    
    def __repr__(self):
        return f"Func {self.name} -> {self.vtype}"

def _builtin_sizeof(node):
    pass
BUILTINS = {"sizeof": _builtin_sizeof}
class ASTFuncCall(ASTNode):
    def __init__(self, name, args):
        self.name = name
        self.args: list[ASTReference] = args
        self.func = None

        self.vtype = None
    
    def analyze(self, ctx: AnalyzeContext):
        if self.name in BUILTINS:
            self.vtype = VTYPES["usize"]
            return
        print(f"Call {self.name} {self.args}")
        self.func = ctx.get_object(self.name)["obj"]
        self.vtype = self.func.vtype
        for idx, arg in enumerate(self.args):
            arg.analyze(ctx)
            print(arg.vtype, self.func.args[idx].vtype)
            assert arg.vtype == self.func.args[idx].vtype

    def compile(self, context: CodeGenContext):
        if self.name in BUILTINS:
            return
        print(f"Call: {self.name}, Args: {self.args}")        
        if not self.func:
            print("Error: cant call {self.name} because it doesnt exist.")
            exit(1)
        assert len(self.args) == len(self.func.args)
        for idx, (fdef, fref) in enumerate(zip(self.func.args, self.args)):
            fref.compile(context)

            context.emit(IRSystem.ib.move(fdef.reduce(), fref.reduce()))

        context.emit(IRSystem.ib.call(self.name))


    def reduce(self):
        if self.name in BUILTINS:
            return TypedOperand(VTYPES["usize"].reduce(), lifetime=Lifetimes.STATIC)
        return TypedOperand(self.func.vtype.reduce(), lifetime=Lifetimes.RETURN)

class ASTDeclaration(ASTNode):
    def __init__(self, name, vtype, length=1):
        self.name = name

        self.vtype = None
        self.vtype_ = vtype

        self.location = None

    def analyze(self, ctx: AnalyzeContext):
        if ctx.get_object(self.name):
            print("Error: Variable already declared")
            exit(1)
            return False
        vtype = self.vtype_
        if vtype[0] in VTYPES:
            self.vtype = VTYPES[vtype[0]]
        else:
            print(f"Error: Unknown Type {vtype[0]}")
            print(VTYPES.keys())
            exit(1)
        if self.vtype is None:
            print(f"Error: complex type {vtype[0]} reduces to None.")
            exit(1)
        self.size = self.vtype.size()

        ctx.add_object(name=self.name, obj={"obj": self, "size": {"obj": ASTStatic(self.size)}})

    def compile(self, context: CodeGenContext):
        context.new_var(self.name, self.vtype, 1)

    def reduce(self):
        if not self.location:
            print("Error: {self.name} not initialized.")
            exit(1)
        return self.location
    def __repr__(self):
        return f"{self.name}: {self.vtype}"

class ASTAssignment(ASTNode):
    def __init__(self, target, source):
        self.target = target
        self.source = source
    
    def analyze(self, context):
        self.source.analyze(context)
        self.target.analyze(context)

    def compile(self, context: CodeGenContext):
        self.source.compile(context)
        self.target.compile(context)
        
        dest, src = self.target.reduce(), self.source.reduce()
        context.emit(IRSystem.ib.move(dest, src))

    def __repr__(self):
        return f"{self.target} = {self.source}"

class ASTNew(ASTNode):
    def __init__(self, declaration):
        self.declaration: ASTDeclaration = declaration
        self.address = None
        
    def analyze(self, context):
        self.declaration.analyze(context)
        print(f"Analyzed: {self.declaration}")

    def compile(self, context: CodeGenContext):
        self.declaration.compile(context)

        self.address = TypedOperand(self.declaration.vtype.reduce(), lifetime=Lifetimes.LOCAL)
        context.update_variable_location(self.declaration.name, self.address)

    def reduce(self, ctx: CodeGenContext):
        return self.address


    def __repr__(self):
        return f"new {self.declaration} ({self.address})"

class ASTMacro(ASTNode):
    def __init__(self, name, args, block):
        self.name = name
        self.args: list[ASTDeclaration] = args
        self.block: list[ASTNode] = block

    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)
        for arg in self.args:
            arg.analyze(ctx)

        return_type = None
        return_types = []

        for statement in self.block:
            statement.analyze(ctx)
            print(f"Analyzed: {statement}")

            if isinstance(statement, ASTReturn):
                return_types.append(statement.vtype)
        print("analyzing return types")
        if return_types and len(return_types) >= 1:
            for i in return_types[1:]:
                if i != return_types[0]:
                    return_type = None
                    print(f"Error: Macro returns too many types: {return_types}")
                    return
            return_type = return_types[0]
        if not return_type:
            return_type = void
        self.vtype = return_type

        ctx = ctx.pop()
        ctx.add_object(self.name, {"obj": self})


    def compile(self, ctx):
        pass
    
    def assemble(self, args, ctx: CodeGenContext):
        ctx = ctx.push(self.name)
        for idx, arg in enumerate(self.args):
            arg.compile(ctx)
            source = args[idx]
            print(arg.name, source)
            ctx.update_variable_location(arg.name, source)
        
        for statement in self.block:
            statement.compile(ctx)

        ctx = ctx.pop()
    def __repr__(self):
        return f"Macro {self.name}"


class ASTComparison(ASTNode):
    def __init__(self, left, op, right):
        self.left = left #ASTReference
        self.right = right #ASTReference
        self.op = op #string < > = <= >=
    
    def analyze(self, ctx):
        self.left.analyze(ctx)
        self.right.analyze(ctx)

        ltype = self.left.vtype
        rtype = self.right.vtype
        print(f"Handling comparison: {self.left} {self.op} {self.right}")
        if not ltype or not rtype:
            print("Error: comparison without types? Seriously?..")
            return
        if ltype != rtype:
            print("Warning: comparison types not matching! ")
        #print(f"Using type {ltype}")

    def compile(self, ctx: CodeGenContext):
        self.right.compile(ctx)
        self.left.compile(ctx)

        ctx.emit(IRSystem.ib.cmp(self.left.reduce(ctx), self.right.reduce(ctx)))

    def reduce(self, ctx):
        print("Trying to reduce a comparison? How does this happen?")

    def __repr__(self):
        return f"{self.left} {self.op} {self.right}"

op_macro_map = {
    "+": "__add__",
    "-": "__sub__",
    "*": "__mul__",
    "/": "__div__",

}
class ASTBinaryOp(ASTNode):
    def __init__(self, left, op, right):
        self.left = left #ASTReference
        self.right = right #ASTReference
        self.op = op #string + - * /

        self.result = None # destination register / address

        self.macro = None #macro to handle the op

        self.vtype = None

    def analyze(self, ctx):
        self.left.analyze(ctx)
        self.right.analyze(ctx)
        self.vtype = self.left.vtype
        ltype = self.left.vtype
        rtype = self.right.vtype
        print(f"Handling op: {self.left} {self.op} {self.right}")
        if not ltype or not rtype:
            print("Error: op without types? Seriously?..")
            return
        if ltype != rtype:
            print(f"Error: op types not matching! {ltype}{self.op}{rtype}")
            exit(1)
            #TODO: implicit type casting?
        #print(f"Using type {ltype}")
        macro_name = op_macro_map[self.op]

        macro = ctx.symbols.get(self.left.vtype.name)
        if not macro:
            print(f"Error: Macro for operation {self.op} ({macro_name}) not implemented for {self.left.vtype.name}")
            exit(1)
        self.macro = macro[macro_name]["obj"]

    def compile(self, ctx: CodeGenContext):
        self.right.compile(ctx)
        self.left.compile(ctx)

        if isinstance(self.left, ASTStatic) and isinstance(self.right, ASTStatic):
            res = 0
            if self.op == "+":
                res = self.left.data + self.right.data
            elif self.op == "-":
                res = self.left.data - self.right.data
            elif self.op == "*":
                res = self.left.data * self.right.data
            elif self.op == "/":
                res = self.left.data // self.right.data
            st = ASTStatic(res)
            st.compile(ctx)

            self.result = st.store
        else:
            self.result = ctx.new_reg(self.left.vtype)
        
            if self.macro:
                self.macro.assemble([self.reduce(), self.left.reduce(), self.right.reduce()], ctx)
            else:
                print(f"Macro for op {self.op} not found.")

    def reduce(self):
        return self.result

    def __repr__(self):
        return f"{self.left} {self.op} {self.right}"

branch_ib_map = {
    "==": IRSystem.ib.jne,
    "!=": IRSystem.ib.je,
    # ">": IRSystem.ib.jgt,
    # "<": IRSystem.ib.jlt,
}

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

class ASTAsm(ASTNode):
    def __init__(self, inst, args):
        self.inst = None
        try:
            self.inst =  IMI[inst.upper()].value
        except KeyError:
            print(f"Error: unknown instruction: '{inst}'")
        self.args = args
    
    def analyze(self, ctx: AnalyzeContext):
        for arg in self.args:
            arg.analyze(ctx)

    def compile(self, ctx: CodeGenContext):
        operands = []
        for arg in self.args:
            arg.compile(ctx)
            operands.append(arg.reduce())

        ctx.emit(Instruction(self.inst, operands))
