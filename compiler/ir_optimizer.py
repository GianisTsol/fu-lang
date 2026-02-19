"""IR code optimization passes."""

from typing import List
from ir_instructions import Instruction
from config import IMI


class IROptimizer:
    """Optimizer for IR instruction sequences."""
    
    @staticmethod
    def remove_dead_code(instructions: List[Instruction]) -> List[Instruction]:
        """Remove unreachable code after unconditional jumps."""
        optimized = []
        skip_until_label = False
        
        for inst in instructions:
            if skip_until_label:
                if inst.opcode == IMI.LABEL:
                    skip_until_label = False
                    optimized.append(inst)
                continue
            
            optimized.append(inst)
            
            if inst.opcode in (IMI.JUMP, IMI.RET):
                skip_until_label = True
        
        return optimized
    
    @staticmethod
    def remove_redundant_moves(instructions: List[Instruction]) -> List[Instruction]:
        """Remove MOVE instructions where source and dest are the same."""
        return [
            inst for inst in instructions
            if not (inst.opcode == IMI.MOVE and inst.operands[0] == inst.operands[1])
        ]

    @staticmethod
    def optimize(instructions: List[Instruction]) -> List[Instruction]:
        """Apply all optimizations."""
        result = instructions
        result = IROptimizer.remove_redundant_moves(result)
        result = IROptimizer.remove_dead_code(result)
        return result
