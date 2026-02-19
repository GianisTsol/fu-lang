"""IR instruction definitions and builder."""

from dataclasses import dataclass
from typing import Any, Tuple, Union
from config import IMI


@dataclass
class Instruction:
    """Represents a single IR instruction with validation."""
    opcode: IMI
    operands: Tuple[Any, ...]
    def __post_init__(self):
        """Validate instruction format."""
        if not isinstance(self.opcode, IMI):
            try:
                self.opcode = IMI(self.opcode)
            except ValueError:
                raise ValueError(f"Invalid opcode: {self.opcode}")
        self._validate()
    
    def _validate(self):
        """Validate operand count and types."""
        validations = {
            IMI.LOAD: (2,), IMI.STORE: (2,), IMI.MOVE: (2,),
            IMI.PUSH: (1,), IMI.POP: (1,), IMI.JUMP: (1,),
            IMI.LABEL: (1,), IMI.ADD: (3,), IMI.SUB: (3,),
            IMI.MUL: (2,), IMI.DIV: (2,), IMI.CMP: (2,),
            IMI.JE: (1,), IMI.JNE: (1,), IMI.JGT: (1,),
            IMI.JLT: (1,), IMI.CALL: (1,), IMI.RET: (0,),
            IMI.NOP: (0,),
        }
        
        if self.opcode in validations:
            expected_count = validations[self.opcode][0]
            if len(self.operands) != expected_count:
                raise ValueError(
                    f"{self.opcode.name} expects {expected_count} operands, "
                    f"got {len(self.operands)}"
                )
    
    def to_tuple(self):
        """Convert to legacy tuple format."""
        return (self.opcode, *self.operands)
    
    def __str__(self):
        """Human-readable string representation."""
        if self.opcode == IMI.LABEL:
            return f"{self.operands[0]}:"
        op_str = ", ".join(str(op) for op in self.operands)
        return f"{self.opcode.name} {op_str}"
    
    @staticmethod
    def from_tuple(tup):
        """Create Instruction from legacy tuple format."""
        if len(tup) < 1:
            raise ValueError("Instruction tuple must have at least opcode")
        return Instruction(tup[0], tup[1:])


class InstructionBuilder:
    """Helper class to build instructions with validation."""
    
    @staticmethod
    def move(dest, src):
        return Instruction(IMI.MOVE, (dest, src))
    
    @staticmethod
    def load(reg, ptr):
        return Instruction(IMI.LOAD, (reg, ptr))
    
    @staticmethod
    def store(reg, ptr):
        return Instruction(IMI.STORE, (reg, ptr))
    
    @staticmethod
    def push(reg_or_val):
        return Instruction(IMI.PUSH, (reg_or_val,))
    
    @staticmethod
    def pop(reg):
        return Instruction(IMI.POP, (reg,))
    
    @staticmethod
    def jump(target):
        return Instruction(IMI.JUMP, (target,))
    
    @staticmethod
    def label(name):
        return Instruction(IMI.LABEL, (name,))
    
    @staticmethod
    def add(dest, a, b):
        return Instruction(IMI.ADD, (dest, a, b))
    
    @staticmethod
    def sub(dest, a, b):
        return Instruction(IMI.SUB, (dest, a, b))
    
    @staticmethod
    def mul(dest, src):
        return Instruction(IMI.MUL, (dest, src))
    
    @staticmethod
    def div(dest, src):
        return Instruction(IMI.DIV, (dest, src))
    
    @staticmethod
    def cmp(left, right):
        return Instruction(IMI.CMP, (left, right))
    
    @staticmethod
    def je(label):
        return Instruction(IMI.JE, (label,))
    
    @staticmethod
    def jne(label):
        return Instruction(IMI.JNE, (label,))
    
    @staticmethod
    def call(target):
        return Instruction(IMI.CALL, (target,))
    
    @staticmethod
    def ret():
        return Instruction(IMI.RET, ())
    
    @staticmethod
    def nop():
        return Instruction(IMI.NOP, ())
