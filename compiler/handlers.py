from ast_definitions import *

def generator(ast):
    actx = AnalyzeContext("root")
    print("ANALYZING TREE")
    for node in ast:
        node.analyze(actx)
        print(f"Analyzed: {node}")

    print("COMPILING TREE")

    cctx = CodeGenContext("root")
    for node in ast:
        node.compile(cctx)
    return cctx.code
