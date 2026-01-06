"""IR code analysis and validation."""

from typing import List, Dict, Set, Union
from ir_instructions import Instruction
from config import IMI


class IRAnalyzer:
    """Analyzer for IR code properties."""
    
    @staticmethod
    def find_labels(instructions: List[Instruction]) -> Dict[str, int]:
        """Find all labels and their positions."""
        labels = {}
        for i, inst in enumerate(instructions):
            if inst.opcode == IMI.LABEL:
                labels[inst.operands[0]] = i
        return labels
    
    @staticmethod
    def find_register_usage(instructions: List[Instruction]) -> Set[str]:
        """Find all registers used in the code."""
        registers = set()
        for inst in instructions:
            for op in inst.operands:
                if isinstance(op, str) and op.startswith('v'):
                    registers.add(op)
        return registers
    
    @staticmethod
    def validate_jumps(instructions: List[Instruction]) -> List[str]:
        """Validate that all jump targets exist."""
        errors = []
        labels = IRAnalyzer.find_labels(instructions)
        
        jump_opcodes = (IMI.JUMP, IMI.JE, IMI.JNE, IMI.JGT, IMI.JLT, IMI.CALL)
        for inst in instructions:
            if inst.opcode in jump_opcodes:
                target = inst.operands[0]
                if isinstance(target, str) and not target.startswith('v'):
                    if target not in labels and target != "INST_PTR":
                        errors.append(f"Jump to undefined label: {target}")
        
        return errors


def print_ir_code(instructions: List[Union[Instruction, tuple]]):
    """Pretty print IR instructions with analysis."""
    from ir_instructions import Instruction
    from ir_analyzer import IRAnalyzer
    from config import IMI
    
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
    labels = IRAnalyzer.find_labels(inst_list)
    registers = IRAnalyzer.find_register_usage(inst_list)
    errors = IRAnalyzer.validate_jumps(inst_list)
    
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
    print(f"  Registers: {len(registers)}")
    
    if registers:
        print(f"  Register list: {', '.join(sorted(registers))}")
    
    if errors:
        print(f"\nVALIDATION ERRORS:")
        for error in errors:
            print(f"  ⚠ {error}")
    else:
        print(f"\n✓ All jumps validated")
    print()

