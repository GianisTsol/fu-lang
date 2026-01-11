"""Main entry point for the compiler."""

from parser import Parser, Statements, print_errors
from tokenizer import Tokenizer, FileStream
from handlers import generator


from ir_instructions import Instruction
from ir_optimizer import IROptimizer
from ir_analyzer import print_ir_code

def compile_file(filename, debug=False, optimize=True):
    """Compile a source file to IR code."""
    # Read source file
    with open(filename, "r") as f:
        source = f.read()
    source_lines = source.split('\n')
    
    # Tokenize
    f = FileStream(filename)
    tok = Tokenizer(f)
    toks = []
    while t := tok.next():
        toks.append(t)
    
    print("Tokens:")
    for t in toks[:20]:  # Show first 20
        print(f"  {t}")
    if len(toks) > 20:
        print(f"  ... and {len(toks) - 20} more")
    
    # Parse
    p = Parser(toks)
    success, ast = Statements.check(p)

    if success:
        print("\n" + "="*60)
        print("✓ Successfully parsed!")
        print("="*60)
        print("\nAST:")
        for node in ast:
            print(f"  {node}")
    else:
        print_errors(p, source_lines)
        exit()
    # Generate IR
    ir_code = generator(ast)
    
    # Optimize if requested
    if optimize:
        instructions = [Instruction.from_tuple(inst) if type(inst) == tuple else inst for inst in ir_code]
        optimized = IROptimizer.optimize(instructions)
        print_ir_code(optimized)
        return optimized
    else:
        print_ir_code(ir_code)
        return ir_code
    
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