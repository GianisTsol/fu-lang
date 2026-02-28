"""Main IR generation system with register management."""

from config import ARGS_REGISTERS
from ir_instructions import InstructionBuilder, Instruction

from enum import IntEnum


class Type:
    def __init__(self, size, signed):
        self.size: int = size
        self.signed = signed
        
        self.name = repr(self)

    def __repr__(self):
        return f"<{'i' if self.signed else 'u'}{self.size}>"

from enum import Enum

class Lifetimes(IntEnum):
    STATIC = 5
    GLOBAL = 4
    LOCAL = 3
    ARG = 2
    RETURN = 1
    TEMP = 0


class TypedOperand:
    idx = 0
    def __init__(self, vtype, lifetime=Lifetimes.TEMP, group=0, data=b""):
        TypedOperand.idx += 1
        self.idx = TypedOperand.idx
        
        self.group = group
        assert type(vtype) == Type
        self.vtype = vtype
        self.lifetime = lifetime
        self.is_ptr = False

        self.data = data
    
    def reduce(self):
        raise NotImplementedError()

    def __repr__(self):
        return f"[L{self.lifetime.name}]:V{self.idx}:{self.vtype}"

class OperandGroup:
    idx = 1
    def __init__(self):
        OperandGroup.idx += 1
        self.idx = OperandGroup.idx


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
