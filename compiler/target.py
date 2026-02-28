from config import IMI
from ir_system import TypedOperand, Lifetimes

class Amd64:
    USIZE = 8 # bytes
    class GeneralRegister:
        def __init__(self, reg64, reg32, reg16, reg8, reg8H=None):
            self.reg64 = reg64
            self.reg32 = reg32
            self.reg16 = reg16
            self.reg8 = reg8
            self.reg8H = reg8H 

        def reduce(self, b):
            size = b * 8
            table = {
                8: self.reg8,
                16: self.reg16,
                32: self.reg32,
                64: self.reg64,
            }
            if size not in table or table[size] is None:
                raise ValueError(f"Register does not support {size}-bit access")
            return table[size]

    class Immediate:
        def __init__(self, value):
            try:
                self.value = int(value)
            except ValueError:
                print("Error: immediate thats not an int?")
                exit(1)
            
        def bytes_required(self, value: int, signed: bool = True) -> int:
            if signed:
                # Signed integers use two's complement
                if value >= 0:
                    bits = value.bit_length() + 1  # sign bit
                else:
                    bits = (-value - 1).bit_length() + 1
            else:
                if value < 0:
                    raise ValueError("Unsigned representation cannot be negative")
                bits = value.bit_length()

            return max(1, (bits + 7) // 8)

        def to_hex_bytes(self, value: int, size: int, signed: bool = True) -> str:
            if size <= 0:
                raise ValueError("size must be positive")
            if not signed and value < 0:
                raise ValueError("Unsigned representation cannot be negative")

            return value.to_bytes(size, byteorder="big", signed=signed).hex()
        
        def reduce(self, size):
            rq = self.bytes_required(self.value)
            if size < rq:
                print("Immediate to big. Maybe place it in memory?")
                exit(1)
            return self.to_hex_bytes(self.value, size)

        def __repr__(self):
            return f"{self.value}"

    class Address:
        def __init__(self, offset):
            self.offset = offset
        
        def reduce(self, size):
            ss = Amd64.size_specifiers[size*8]
            return f"{ss} [RSP+{self.offset}]"

    gprs = {
        "RAX": GeneralRegister("rax", "eax", "ax", "al", "ah"),
        "RBX": GeneralRegister("rbx", "ebx", "bx", "bl", "bh"),
        "RCX": GeneralRegister("rcx", "ecx", "cx", "cl", "ch"),
        "RDX": GeneralRegister("rdx", "edx", "dx", "dl", "dh"),

        "RSI": GeneralRegister("rsi", "esi", "si", "sil"),
        "RDI": GeneralRegister("rdi", "edi", "di", "dil"),
        "RBP": GeneralRegister("rbp", "ebp", "bp", "bpl"),
        "RSP": GeneralRegister("rsp", "esp", "sp", "spl"),

        "R8":  GeneralRegister("r8",  "r8d",  "r8w",  "r8b"),
        "R9":  GeneralRegister("r9",  "r9d",  "r9w",  "r9b"),
        "R10": GeneralRegister("r10", "r10d", "r10w", "r10b"),
        "R11": GeneralRegister("r11", "r11d", "r11w", "r11b"),
        "R12": GeneralRegister("r12", "r12d", "r12w", "r12b"),
        "R13": GeneralRegister("r13", "r13d", "r13w", "r13b"),
        "R14": GeneralRegister("r14", "r14d", "r14w", "r14b"),
        "R15": GeneralRegister("r15", "r15d", "r15w", "r15b"),
    }

    size_specifiers = {
        8: "byte",
        16: "word",
        32: "dword",
        64: "qword",
    }

    def __init__(self):
        self.name = "x86_64"

        self.register_names = list(self.gprs.keys())
        self.register_names.reverse()

        self.argument_registers = ["RDI", "RSI", "RDX", "RCX", "R8", "R9"]
        self.return_register = "RAX"
        self.stack_pointer = "RSP"

        self.free_registers = sorted(self.register_names, key=lambda x: len(self.argument_registers) - self.argument_registers.index(x) if x in self.argument_registers else 0)
        self.used_registers = []

        self.size_allocated = 0
        self.operand_location_map = {}

        self.code = []
        
    def emit(self, text):
        self.code.append(text)

    def force_free_register(self, name):
        if name not in self.register_names:
            exit(1)
        if name in self.free_registers:
            return
        other = self.get_register()

        self.emit(f"mov {other}, {name}")

        idx = None
        for k, v in self.operand_location_map.items():
            if v == name:
                idx = k
        self.operand_location_map[idx] = self.gprs[other]
        self.used_registers.remove(name)
        self.free_registers.append(name)

    def get_register(self, name=None):
        if name:
            if name not in self.register_names:
                exit(1)
            if name in self.free_registers:
                reg = name
            else:
                raise("register in use. try a force free first?")
        else:
            score, best = 0, None
            for reg in self.free_registers:
                c = 0
                if reg.lower() not in self.argument_registers:
                    c += 1
                if reg.upper() is not self.return_register.upper():
                    c += 2
                if c > score:
                    best = reg
                    score = c
            reg = best
        print(self.free_registers)
        self.free_registers.remove(reg)
        self.used_registers.append(reg)
        return reg

    def free_register(self, name):
        name = name.upper()
        if name not in self.register_names:
            print(f"Unkown register. '{name}'")
            print(self.register_names)
            raise UnboundLocalError
            exit(1)
        if name not in self.used_registers:
            print(f"Cant free the free... '{name}'")
            return
        self.used_registers.remove(name)
        self.free_registers.append(name)

    def allocate_memory(self, size):
        size = (size + 15) & ~15
        self.emit(f"sub rsp, {size}")
        self.size_allocated += size

    def allocate_operand(self, op: TypedOperand):
        result = Amd64.Immediate(12340)

        if op.lifetime == Lifetimes.TEMP:
            n = self.get_register()
            result = self.gprs[n]
        elif op.lifetime == Lifetimes.RETURN:
            result = self.gprs[self.return_register]
        elif op.lifetime == Lifetimes.ARG:
            for reg in self.argument_registers:
                if reg in self.free_registers:
                    result = self.gprs[self.get_register(name=reg)]
                    break
        elif op.lifetime == Lifetimes.LOCAL:
            self.allocate_memory(op.vtype.size)
            result = Amd64.Address(self.size_allocated + op.vtype.size)

        elif op.lifetime == Lifetimes.STATIC:
            result = Amd64.Immediate(op.data)
        self.operand_location_map[op.idx] = result

    def free_operand(self, idx):
        print(idx)
        loc = self.operand_location_map[idx]
        self.operand_location_map.pop(idx)
        if isinstance(loc, Amd64.GeneralRegister):
            self.free_register(loc.reg64)

    def get_operand_location(self, op):
        if op.idx in self.operand_location_map:
            return self.operand_location_map[op.idx]
        else:
            self.allocate_operand(op)
            return self.operand_location_map[op.idx]

    def compile(self, ir_code, metadata):
        print("="*60)
        print(self.name)
        print("="*60)
        print(f"Free registers: {', '.join(self.free_registers)}")
        for idx, inst in enumerate(ir_code):
            meta = metadata[idx]
            self.compile_instruction(inst)

            for k in meta["last"]:
                self.free_operand(k)

        for r in self.code:
            print(r)
    
    def compile_operand(self, op, size=USIZE):
        if isinstance(op, TypedOperand):
            return self.get_operand_location(op).reduce(op.vtype.size)
        elif isinstance(op, str):
            return op
        else:
            print(f"Unknown operand: {op}({type(op)})")
            exit(1)

    def compile_instruction(self, inst):
        params = []
        for op in inst.operands:
            params.append(self.compile_operand(op))
        print(inst.operands, params)
        if inst.opcode == IMI.ADD:
            if params[0] != params[1]:
                self.emit(f"mov {params[0]}, {params[1]}")
            self.emit(f"add {params[0]}, {params[2]}")
        elif inst.opcode == IMI.SUB:
            self.emit(f"mov {params[0]}, {params[1]}")
            self.emit(f"sub {params[0]}, {params[2]}")
        elif inst.opcode == IMI.LABEL:
            self.size_allocated = 0
            self.emit(f"{params[0]}:")
            self.emit("push rbp")
            self.emit("mov rbp, rsp")
        elif inst.opcode == IMI.RET:
            self.emit("mov rsp, rbp")
            self.emit("pop rbp")
            self.emit("ret")
        elif inst.opcode == IMI.CALL:
            self.force_free_register(self.return_register)
            self.emit(f"call {params[0]}")
            self.free_register(self.return_register)

        elif inst.opcode == IMI.MOVE:
            c = 'mov'
            if inst.operands[0].vtype.size > inst.operands[1].vtype.size:
                if inst.operands[0].vtype.signed == inst.operands[1].vtype.signed:
                    if inst.operands[0].vtype.signed == True:
                        c = 'movsx'
                    elif inst.operands[0].vtype.signed == False:
                        c = 'movzx'
                    if isinstance(inst.operands[0], Amd64.Address):
                        tmp = self.gprs[self.get_register()].reduce(inst.operands[0].vtype.size)
                        self.emit(f"movvv {tmp}, {params[0]}")
            self.emit(f"{c} {', '.join(params)}")



