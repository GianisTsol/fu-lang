from config import IMI
from ir_system import TypedOperand, Address, Lifetimes

class Amd64:
    USIZE = 8 # bytes
    class GeneralRegister:
        def __init__(self, reg64, reg32, reg16, reg8, reg8H=None):
            self.reg64 = reg64
            self.reg32 = reg32
            self.reg16 = reg16
            self.reg8 = reg8
            self.reg8H = reg8H

        def reduce(self, size):
            table = {1: self.reg8, 2: self.reg16, 4: self.reg32, 8: self.reg64}
            name = table.get(size)
            if name is None:
                raise ValueError(f"Register does not support {size}-byte access")
            return name

        def __repr__(self):
            return self.reg64

    class Immediate:
        def __init__(self, value):
            try:
                self.value = int(value)
            except (ValueError, TypeError):
                raise ValueError(f"Immediate value must be an integer, got: {value!r}")

        def _bytes_required(self, signed=True):
            v = self.value
            if signed:
                bits = (v.bit_length() + 1) if v >= 0 else ((-v - 1).bit_length() + 1)
            else:
                if v < 0:
                    raise ValueError("Unsigned representation cannot be negative")
                bits = v.bit_length()
            return max(1, (bits + 7) // 8)

        def reduce(self, size, signed=True):
            required = self._bytes_required(signed)
            if size < required:
                raise OverflowError(
                    f"Immediate {self.value} requires {required} bytes but only {size} allocated"
                )
            return self.value.to_bytes(size, byteorder="big", signed=signed).hex()

        def __repr__(self):
            return str(self.value)

    class Address:
        def __init__(self, offset):
            self.offset = offset

        def reduce(self, size):
            specifier = Amd64.size_specifiers[size * 8]
            return f"{specifier} [rbp-{self.offset}]"

        def __repr__(self):
            return f"[rbp-{self.offset}]"

    # -------------------------------------------------------------------------
    # Register / size tables
    # -------------------------------------------------------------------------

    gprs = {
        "RAX": GeneralRegister("rax", "eax",  "ax",   "al",  "ah"),
        "RBX": GeneralRegister("rbx", "ebx",  "bx",   "bl",  "bh"),
        "RCX": GeneralRegister("rcx", "ecx",  "cx",   "cl",  "ch"),
        "RDX": GeneralRegister("rdx", "edx",  "dx",   "dl",  "dh"),
        "RSI": GeneralRegister("rsi", "esi",  "si",   "sil"),
        "RDI": GeneralRegister("rdi", "edi",  "di",   "dil"),
        "RBP": GeneralRegister("rbp", "ebp",  "bp",   "bpl"),
        "RSP": GeneralRegister("rsp", "esp",  "sp",   "spl"),
        "R8":  GeneralRegister("r8",  "r8d",  "r8w",  "r8b"),
        "R9":  GeneralRegister("r9",  "r9d",  "r9w",  "r9b"),
        "R10": GeneralRegister("r10", "r10d", "r10w", "r10b"),
        "R11": GeneralRegister("r11", "r11d", "r11w", "r11b"),
        "R12": GeneralRegister("r12", "r12d", "r12w", "r12b"),
        "R13": GeneralRegister("r13", "r13d", "r13w", "r13b"),
        "R14": GeneralRegister("r14", "r14d", "r14w", "r14b"),
        "R15": GeneralRegister("r15", "r15d", "r15w", "r15b"),
    }

    size_specifiers = {8: "byte", 16: "word", 32: "dword", 64: "qword"}

    def __init__(self):
        self.name = "x86_64"

        # Argument registers in SysV ABI order
        self.argument_registers = ["RDI", "RSI", "RDX", "RCX", "R8", "R9"]
        self.return_register    = "RAX"
        self.stack_pointer      = "RSP"

        all_regs = list(self.gprs.keys())

        # Prefer non-arg, non-return registers when allocating temporaries
        def _alloc_score(reg):
            in_args   = reg in self.argument_registers
            is_return = reg == self.return_register
            return (not in_args, not is_return)

        self.free_registers = sorted(all_regs, key=_alloc_score, reverse=True)
        self.used_registers: list[str] = []

        self.operand_location_map: dict = {}
        self.code: list[str] = []

    # -------------------------------------------------------------------------
    # Emission
    # -------------------------------------------------------------------------

    def emit(self, text: str):
        self.code.append(text)

    # -------------------------------------------------------------------------
    # Register management
    # -------------------------------------------------------------------------

    def get_register(self, name: str | None = None) -> str:
        """Allocate and return a free register (by canonical upper-case name)."""
        if name:
            name = name.upper()
            if name not in self.free_registers:
                raise RuntimeError(f"Register {name} is not free; force-free it first")
            reg = name
        else:
            if not self.free_registers:
                raise RuntimeError("No free registers available (register spilling not yet implemented)")
            reg = self.free_registers[0]  # already sorted by preference

        self.free_registers.remove(reg)
        self.used_registers.append(reg)
        return reg

    def free_register(self, name: str):
        name = name.upper()
        if name not in self.register_names:
            raise ValueError(f"Unknown register '{name}'")
        if name not in self.used_registers:
            return  # already free — no-op
        self.used_registers.remove(name)
        self.free_registers.append(name)

    def force_free_register(self, name: str):
        """Move the value in *name* to another register so *name* becomes free."""
        name = name.upper()
        if name in self.free_registers:
            return

        dest_name = self.get_register()
        dest_gpr  = self.gprs[dest_name]
        self.emit(f"mov {dest_gpr.reg64}, {self.gprs[name].reg64}")

        # Update location map so the operand now lives in the new register
        for idx, loc in self.operand_location_map.items():
            if isinstance(loc, Amd64.GeneralRegister) and loc.reg64 == self.gprs[name].reg64:
                self.operand_location_map[idx] = dest_gpr
                break

        self.used_registers.remove(name)
        self.free_registers.append(name)

    @property
    def register_names(self):
        return list(self.gprs.keys())

    # -------------------------------------------------------------------------
    # Stack frame layout  (two-pass)
    # -------------------------------------------------------------------------

    def _align(self, offset: int, alignment: int) -> int:
        return (offset + alignment - 1) & ~(alignment - 1)

    def layout_locals(self, ir_code) -> int:
        """
        First pass: walk all operands and assign stable rbp-relative offsets
        to every LOCAL operand.  Returns the total frame size (16-byte aligned).
        No code is emitted here.
        """
        offset = 0
        for inst in ir_code:
            for op in inst.operands:
                if not isinstance(op, TypedOperand):
                    continue
                if op.lifetime != Lifetimes.LOCAL:
                    continue
                if op.idx in self.operand_location_map:
                    continue  # already laid out (same operand referenced twice)
                offset += op.vtype.size
                offset  = self._align(offset, min(op.vtype.size, 8))
                self.operand_location_map[op.idx] = Amd64.Address(offset)

        return self._align(offset, 16)

    def emit_prologue(self, label: str, frame_size: int):
        self.emit(f"{label}:")
        self.emit("push rbp")
        self.emit("mov rbp, rsp")
        if frame_size > 0:
            self.emit(f"sub rsp, {frame_size}")

    def emit_epilogue(self):
        self.emit("mov rsp, rbp")
        self.emit("pop rbp")
        self.emit("ret")

    # -------------------------------------------------------------------------
    # Operand allocation / release
    # -------------------------------------------------------------------------

    def allocate_operand(self, op: TypedOperand):
        if op.lifetime == Lifetimes.TEMP:
            reg  = self.get_register()
            loc  = self.gprs[reg]

        elif op.lifetime == Lifetimes.RETURN:
            self.force_free_register(self.return_register)
            loc = self.gprs[self.return_register]

        elif op.lifetime == Lifetimes.ARG:
            loc = None
            for reg in self.argument_registers:
                if reg in self.free_registers:
                    loc = self.gprs[self.get_register(name=reg)]
                    break
            if loc is None:
                raise RuntimeError("All argument registers are in use")

        elif op.lifetime == Lifetimes.LOCAL:
            # Already assigned by layout_locals; nothing to do at this point.
            return

        elif op.lifetime == Lifetimes.STATIC:
            loc = Amd64.Immediate(op.data)

        else:
            raise ValueError(f"Unknown lifetime: {op.lifetime}")

        self.operand_location_map[op.idx] = loc

    def free_operand(self, idx: int):
        loc = self.operand_location_map.pop(idx, None)
        if isinstance(loc, Amd64.GeneralRegister):
            self.free_register(loc.reg64)

    def get_operand_location(self, op: TypedOperand):
        if op.idx not in self.operand_location_map:
            self.allocate_operand(op)
        return self.operand_location_map[op.idx]

    # -------------------------------------------------------------------------
    # Compilation
    # -------------------------------------------------------------------------

    def compile(self, ir_code, metadata):
        print("=" * 60)
        print(self.name)
        print("=" * 60)

        # Two-pass: lay out locals first so the prologue can reserve the right
        # amount of space in one shot.
        frame_size = self.layout_locals(ir_code)

        for idx, inst in enumerate(ir_code):
            meta = metadata[idx]
            self.compile_instruction(inst, frame_size=frame_size)
            for k in meta["last"]:
                self.free_operand(k)

        for line in self.code:
            print(line)

    def compile_operand(self, op) -> str:
        if isinstance(op, TypedOperand):
            return self.get_operand_location(op).reduce(op.vtype.size)
        if isinstance(op, str):
            return op
        raise TypeError(f"Unknown operand type: {type(op)!r}")

    def compile_instruction(self, inst, *, frame_size: int = 0):
        params = [self.compile_operand(op) for op in inst.operands]

        match inst.opcode:
            case IMI.LABEL:
                self.emit_prologue(params[0], frame_size)

            case IMI.RET:
                self.emit_epilogue()

            case IMI.MOVE:
                dst_op, src_op = inst.operands[0], inst.operands[1]
                dst_size, src_size = dst_op.vtype.size, src_op.vtype.size

                dst, src = params[0], params[1]
                if dst_size > src_size:
                    mnemonic = "movsx" if dst_op.vtype.signed else "movzx"
                else:
                    mnemonic = "mov"

                if dst_op.lifetime == Lifetimes.LOCAL:
                    r = self.get_register()
                    reg = self.gprs[r].reduce(dst_size)
                    self.emit(f"{mnemonic} {reg}, {src}")
                    src = reg
                    self.free_register(r)
                self.emit(f"mov {dst}, {src} ;")

            case IMI.ADD:
                if params[0] != params[1]:
                    self.emit(f"mov {params[0]}, {params[1]}")
                self.emit(f"add {params[0]}, {params[2]}")

            case IMI.SUB:
                if params[0] != params[1]:
                    self.emit(f"mov {params[0]}, {params[1]}")
                self.emit(f"sub {params[0]}, {params[2]}")

            case IMI.CALL:
                self.force_free_register(self.return_register)
                self.emit(f"call {params[0]}")
                # Return register is now live in RAX; caller decides when to free it.

            case _:
                raise NotImplementedError(f"Unhandled opcode: {inst.opcode}")