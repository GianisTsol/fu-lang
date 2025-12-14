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


class ParseError(Exception):
    """Custom exception for parsing errors."""
    pass


def get_special_chars():
    """Get list of special characters used in parsing."""
    return ([b[0] for b in BOUNDINGS] + 
            [b[1] for b in BOUNDINGS] + 
            [" ", ";", ",", '"', "*", "$", "#", ".", ":"])
