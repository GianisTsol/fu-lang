"""IR code analysis and validation."""

from typing import List, Dict, Set, Union
from ir_instructions import Instruction
from config import IMI, InstructionSpecs
from ir_system import TypedOperand, Lifetimes


def find_labels(instructions: List[Instruction]) -> Dict[str, int]:
    """Find all labels and their positions."""
    labels = {}
    for i, inst in enumerate(instructions):
        if inst.opcode == IMI.LABEL:
            labels[inst.operands[0]] = i
    return labels

def find_register_usage(instructions: List[Instruction]) -> Set[str]:
    """Find all registers used in the code."""
    registers = set()
    for inst in instructions:
        for op in inst.operands:
            if isinstance(op, TypedOperand):
                registers.add(op)
                print(type(op.vtype), op, inst)
                assert type(op.vtype) != int
    return registers

def validate_jumps(instructions: List[Instruction]) -> List[str]:
    """Validate that all jump targets exist."""
    errors = []
    labels = find_labels(instructions)
    
    jump_opcodes = (IMI.JUMP, IMI.JE, IMI.JNE, IMI.JGT, IMI.JLT, IMI.CALL)
    for inst in instructions:
        if inst.opcode in jump_opcodes:
            target = inst.operands[0]
            if isinstance(target, str) and not target.startswith('v'):
                if target not in labels and target != "INST_PTR":
                    errors.append(f"Jump to undefined label: {target}")
    
    return errors

class OperandMeta:
    reads = []
    writes = []
    size = 0
    lifetime = 0

def generate_operand_metadata(instructions: List[Instruction]):
    """Generate metadata of operand usage."""
    metadata = {}
    for i, instruction in enumerate(instructions):
        spec = InstructionSpecs[instruction.opcode]

        for op_idx, op in enumerate(instruction.operands):
            if type(op) is not TypedOperand:
                print(f"Operand is {type(op)}, i hope you know what ur doing")
                continue
            if op.idx not in metadata:
                metadata[op.idx] = OperandMeta()
                metadata[op.idx].lifetime = op.lifetime
                metadata[op.idx].size = op.vtype.size
            if op_idx in spec.operands_affected:
                metadata[op.idx].writes.append(i)
            else:
                metadata[op.idx].reads.append(i)
    return metadata

def generate_instruction_metadata(instructions: List[Instruction]):
    metadata = {}
    for i in range(0, len(instructions)):
        metadata[i] = {"first": [], "last": [], "operands": {x: [] for x in Lifetimes}}
    m = generate_operand_metadata(instructions)
    for op_idx, op_meta in m.items():
        first = min(min(op_meta.reads), min(op_meta.writes))
        last = max(max(op_meta.reads), max(op_meta.writes))

        metadata[first]["first"].append(op_idx)
        metadata[last]["last"].append(op_idx)
        
        for j in range(first, last):
            metadata[j]["operands"][op_meta.lifetime].append(op_idx)

    return metadata

def generate_label_metadata(self, instructions):
    metadata = {}
    curr = None
    start = 0
    imd = generate_instruction_metadata(instructions)
    
    for idx, inst in enumerate(instructions):
        if inst.opcode == IMI.LABEL:
            if curr:
                metadata[curr] = (start, idx)
            start = idx
            curr = inst.operands[0]
    return metadata
        

def print_ir_code(instructions: List[Union[Instruction, tuple]]):
    """Pretty print IR instructions with analysis."""
    
    # Convert to Instruction objects
    inst_list = []
    for inst in instructions:
        if isinstance(inst, tuple):
            inst_list.append(Instruction.from_tuple(inst))
        else:
            inst_list.append(inst)
    
    print("\n" + "="*70)
    print("GENERATED IR CODE:")
    print("="*70)
    
    # Analyze
    labels = find_labels(inst_list)
    registers = find_register_usage(inst_list)
    errors = validate_jumps(inst_list)
    
    # Print instructions
    for i, inst in enumerate(inst_list):
        if inst.opcode == IMI.LABEL:
            print(f"{inst.operands[0]}:")
        else:
            print(f"  {i:3d}: {inst}")
    
    print("="*70)
    print(f"\nSTATISTICS:")
    print(f"  Total instructions: {len(inst_list)}")
    print(f"  Labels: {len(labels)}")
    print(f"  Total Operands: {len(registers)} ({sum(k.vtype.size for k in registers)} Bytes)")
        
    if errors:
        print(f"\nVALIDATION ERRORS:")
        for error in errors:
            print(f"  ⚠ {error}")
    else:
        print(f"\n✓ All jumps validated")
    print()

