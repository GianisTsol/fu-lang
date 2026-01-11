from ir_system import IRSystem

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
        offset = self.offset
        if isinstance(other, int):
            offset += other
        elif isinstance(other, Address):
            if self.reg != other.reg:
                print("Warning: Trying to add addresses with different start regissters.")
                return None
            offset += other.offset
        return Address(self.reg, offset)

    def __repr__(self):
        return f"{self.reg}+{self.offset}"

class Type:
    def __init__(self, name, size, signed):
        self.name = name
        self.size: int = size
        self.signed = signed

        self.macros = {}

    def __repr__(self):
        return f"<{self.name}>"

void = Type("void", 0, False)
usize = Type("usize", 8, False)
isize = Type("isize", 8, True)
u8    = Type("u8",    1, False)
ptr   = Type("ptr",   8, False)

VTYPES = {
    "void:": void,
    "usize": usize,
    "u8": u8,
}

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
        self.vtype: Type = vtype
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
    
    def new_var(self, name, vtype, length):
        if name in self.vars.keys():
            print(f"Error: Variable already registered: {name}")
            return None
        print("NEW VAR", name, "W TYPE: ", vtype)
        self.vars[name] = CodeGenVar(name, vtype)
        self.vars[name].size = vtype.size * length

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
    def __init__(self, name, index=0):
        self.name = name
        self.vtype: Type = None
        self.address: Address = None
        self.index: int = index

    def analyze(self, ctx: AnalyzeContext):
        obj = ctx.get_object(self.name)
        if not obj:
            print(f"Error: Unknown object {self.name}")
        self.vtype = obj["type"]
    
    def compile(self, ctx: CodeGenContext):
        var: CodeGenVar = ctx.get_var(self.name)
        self.address = var.address.add(self.index * var.vtype.size)

    def reduce(self, ctx: CodeGenContext):
        return self.address

    def __repr__(self):
        return f"ref {self.name}[{self.index}]: {self.vtype} ({self.address})"

    
class ASTClass(ASTNode):
    def __init__(self, name, statements):
        self.name = name
        self.statements = statements


    def analyze(self, ctx: AnalyzeContext):
        ctx = ctx.push(self.name)
        for statement in self.statements:
            statement.analyze(ctx)
        ctx = ctx.pop()

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
        ctx = ctx.pop()
        
    def compile(self, context: CodeGenContext):
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
    def __init__(self, name, vtype, length=1):
        self.name = name

        self.type = None

        if vtype in VTYPES:
            self.vtype = VTYPES[vtype]
        else:
            print(f"Error: Unknown Type {vtype}")

        self.length = length
        self.size = self.vtype.size * self.length

    def analyze(self, ctx: AnalyzeContext):
        if ctx.get_object(self.name):
            print("Error: Variable already declared")
            return False
        ctx.add_object(name=self.name, obj={"type": self.vtype})

    def compile(self, context: CodeGenContext):
        context.new_var(self.name, self.vtype, self.length)

    def __repr__(self):
        return f"{self.name}: {self.vtype}[{self.length}]"

class ASTAssignment(ASTNode):
    def __init__(self, target, source):
        self.target = source
        self.source = target
    
    def analyze(self, context):
        print(self)
        self.source.analyze(context)
        self.target.analyze(context)

    def compile(self, context: CodeGenContext):
        self.source.compile(context)
        self.target.compile(context)
        
        dest, src = self.target.reduce(context), self.source.reduce(context)
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

        size = self.declaration.size
        self.address = context.get_memory(size)
        context.update_variable_address(self.declaration.name, self.address)

    def reduce(self, ctx: CodeGenContext):
        return ctx.get_var(self.declaration.name).address

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

        for statement in self.block:
            statement.analyze(ctx)
        ctx.add_object(self.name, self)
        ctx = ctx.pop()

    def compile(self, ctx):
        pass

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


class ASTBinaryOp(ASTNode):
    def __init__(self, left, op, right):
        self.left = left #ASTReference
        self.right = right #ASTReference
        self.op = op #string + - * /

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
        # TODO: implement operation assembly

        ctx.emit(IRSystem.ib.move(self.result, self))

    def reduce(self, ctx):
        return self.result

    def __repr__(self):
        return f"{self.left} {self.op} {self.right}"

branch_ib_map = {
    "==": IRSystem.ib.jne,
    "!=": IRSystem.ib.je,
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