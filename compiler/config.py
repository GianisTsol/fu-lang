"""Configuration constants and enums for the compiler."""

from enum import IntEnum

# Parsing Configuration
BOUNDINGS = (("(", ")"), ("{", "}"), ("[", "]"))
SEPARATORS = (";", ",", ".", ":")
ARGS_REGISTERS = 4


class IMI(IntEnum):
    """Intermediate Machine Instruction opcodes."""
    LOAD = 0
    STORE = 1
    MOVE = 2
    PUSH = 3
    POP = 4
    JUMP = 5
    LABEL = 6
    ADD = 7
    SUB = 8
    MUL = 9
    DIV = 10
    CMP = 11
    JE = 12   # Jump if equal
    JNE = 13  # Jump if not equal
    JGT = 14  # Jump if greater than
    JLT = 15  # Jump if less than
    CALL = 16

    RET = 17
    NOP = 18

    ALLOC = 19
    FREE = 20


class Spec:
    def __init__(self, number_of_operands, operands_affected=[]):
        self.number_of_operands = number_of_operands
        self.operands_affected = operands_affected
        
InstructionSpecs = {
    IMI.LOAD: Spec(2, [0]),
    IMI.STORE: Spec(2, [0]),
    IMI.MOVE: Spec(2, [0]),
    IMI.JUMP: Spec(1, []),
    IMI.LABEL: Spec(1, []),
    IMI.ADD: Spec(3, [0]),
    IMI.SUB: Spec(3, [0]),
    IMI.MUL: Spec(3, [0]),
    IMI.DIV: Spec(3, [0]),
    IMI.CMP: Spec(2, []),
    IMI.CALL: Spec(1, []),
    IMI.RET: Spec(1, []),
    IMI.PUSH: Spec(1, []),
    IMI.POP: Spec(1, [0]),

}



class ParseError(Exception):
    """Custom exception for parsing errors."""
    pass


def get_special_chars():
    """Get list of special characters used in parsing."""
    return ([b[0] for b in BOUNDINGS] + 
            [b[1] for b in BOUNDINGS] + 
            [" ", ";", ",", '"', "*", "$", "#", ".", ":", "&"])
