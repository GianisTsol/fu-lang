"""Main entry point for the compiler."""

from reader import Reader
from parser import parse
from template_matcher import template_match, parsed_templates
from ir_system import IRSystem
from ir_instructions import Instruction
from ir_optimizer import IROptimizer
from ir_analyzer import print_ir_code
from handlers import block_handler

def compile_file(filename, debug=False, optimize=True):
    """Compile a source file to IR code."""
    try:
        # Parse
        r = Reader()
        r.load_file(filename)
        parsed_tokens = parse(r)
        
        if debug:
            print("="*70)
            print("PARSED TOKENS:")
            print("="*70)
            for i, token in enumerate(parsed_tokens):
                print(f"{i}: {token}")
        
        # Match templates
        if debug:
            print("\n" + "="*70)
            print("MATCHING TEMPLATES...")
            print(f"parsed templates: {parsed_templates}")
            print("="*70)
        
        matched = template_match(parsed_templates, parsed_tokens, debug=debug)
        
        if debug:
            print("\n" + "="*70)
            print("MATCHED RESULT:")
            print("="*70)
            for i, match in enumerate(matched):
                print(f"{i}: {match}")
        
        # Generate IR
        ir_code = block_handler(matched)
        
        # Optimize if requested
        if optimize:
            instructions = [Instruction.from_tuple(inst) for inst in ir_code]
            optimized = IROptimizer.optimize(instructions)
            print_ir_code(optimized)
            return optimized
        else:
            print_ir_code(ir_code)
            return ir_code
        
    except Exception as e:
        print(f"\nERROR: {e}")
        import traceback
        traceback.print_exc()
        return None


def compile_text(text, debug=False, optimize=True):
    """Compile source text to IR code."""
    try:
        r = Reader()
        r.load_text(text)
        parsed_tokens = parse(r)
        
        matched = template_match(parsed_templates, parsed_tokens, debug=debug)
        ir_code = IRSystem.block_handler(matched)
        
        if optimize:
            instructions = [Instruction.from_tuple(inst) for inst in ir_code]
            return IROptimizer.optimize(instructions)
        
        return ir_code
        
    except Exception as e:
        print(f"\nERROR: {e}")
        return None


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        debug = "--debug" in sys.argv
        no_opt = "--no-opt" in sys.argv
        
        compile_file(filename, debug=debug, optimize=not no_opt)
    else:
        print("Usage: python main.py <filename> [--debug] [--no-opt]")
        print("\nExample:")
        print("  python main.py test.fu")
        print("  python main.py test.fu --debug")
        print("  python main.py test.fu --debug --no-opt")