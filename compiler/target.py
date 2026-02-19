from config import IMI
from ir_system import TypedOperand, Immediate, Address

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

    def compile(self, ir_code):
        print("="*60)
        print(self.name)
        print("="*60)
        result = []
        for inst in ir_code:
            result.extend(self.compile_instruction(inst))
        
        for r in result:
            print(r)
    
    def compile_operand(self, op, size=USIZE):
        if isinstance(op, TypedOperand):
            if op.lifetime == 0:
                print(op.idx)
                reg = self.gprs[self.register_names[op.idx]].reduce(size)
                return reg
            elif op.lifetime == 1:
                return 0
            elif op.lifetime == 3:
                return str(op.data)
        elif isinstance(op, str):
            return op
        else:
            print(f"Unknown operand: {op}({type(op)})")
            exit(1)

    def compile_instruction(self, inst):
        result = []
        params = []
        for op in inst.operands:
            params.append(self.compile_operand(op))
            print(op, params)
        if inst.opcode == IMI.ADD:
            if params[0] != params[1]:
                result.append(f"mov {params[0]}, {params[1]}")
            result.append(f"add {params[0]}, {params[2]}")
        elif inst.opcode == IMI.SUB:
            result.append(f"mov {params[0]}, {params[1]}")
            result.append(f"sub {params[0]}, {params[2]}")
        elif inst.opcode == IMI.LABEL:
            result.append(f"{params[0]}:")
        elif inst.opcode == IMI.RET:
            result.append("ret")
        elif inst.opcode == IMI.CALL:
            result.append(f"call {params[0]}")

        elif inst.opcode == IMI.MOVE:
            c = 'mov'
            if inst.operands[0].vtype.size > inst.operands[1].vtype.size:
                if inst.operands[0].vtype.signed == inst.operands[1].vtype.signed:
                    if inst.operands[0].vtype.signed == True:
                        c = 'movsx'
                    elif inst.operands[0].vtype.signed == False:
                        c = 'movzx'
            result.append(f"{c} {', '.join(params)}")
        return result



