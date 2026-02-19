"""Main IR generation system with register management."""

from config import ARGS_REGISTERS
from ir_instructions import InstructionBuilder, Instruction

class Type:
    def __init__(self, size, signed):
        self.size: int = size
        self.signed = signed
        
        self.name = repr(self)

    def __repr__(self):
        return f"<{'i' if self.signed else 'u'}{self.size}>"

class TypedOperand:
    def __init__(self, idx, vtype, lifetime=0, data=b""):
        self.idx = idx

        assert type(idx) == int
        self.vtype = vtype
        self.lifetime = 0
        self.is_ptr = False

        self.data = data
    
    def reduce(self):
        raise NotImplementedError()

    def __repr__(self):
        return f"[L{self.lifetime}]:V{self.idx}:{self.vtype}"

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

    def __repr__(self):
        return f"{self.value}"


class Address:
    def __init__(self, reg, offset, vtype=None):
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
        return f"[{self.reg}+{self.offset}]"

class IRSystem:
    """IR generation system with context management."""
    
    ib = InstructionBuilder()
