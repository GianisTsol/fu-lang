"""IR code optimization passes."""

from typing import List
from ir_instructions import Instruction
from config import IMI
from ir_system import TypedOperand


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
        instructions = [
            inst for inst in instructions
            if not (inst.opcode == IMI.MOVE and inst.operands[0] == inst.operands[1])
        ]
        result = []
        for i in range(0, len(instructions)):
            curr_inst = instructions[i]
            if i == len(instructions) - 1:
                continue
            next_inst = instructions[i+1]

            if curr_inst.opcode == next_inst.opcode == IMI.MOVE:
                if curr_inst.operands[0] == next_inst.operands[1]:
                    next_inst.operands[1] == curr_inst.operands[1]
                    continue
            result.append(curr_inst)
        
        return result

    @staticmethod
    def reduce_operands(instructions, metadata):
        result = []

        free = []
        remap = {}
        for idx, inst in enumerate(instructions):
            first, last = metadata[idx]["first"], metadata[idx]["last"]
            free.extend(last)
            for j in first:
                if len(free) == 0:
                    break
                remap[j] = free.pop()
            for op in inst.operands:
                if type(op) is not TypedOperand:
                    continue
                if op.idx in remap:
                    op.idx = remap[op.idx]
            result.append(inst)
        return result

    @staticmethod
    def optimize(instructions: List[Instruction], metadata) -> List[Instruction]:
        """Apply all optimizations."""
        result = instructions
        result = IROptimizer.remove_dead_code(result)
        return result
