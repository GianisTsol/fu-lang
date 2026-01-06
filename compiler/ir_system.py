"""Main IR generation system with register management."""

from config import ARGS_REGISTERS
from ir_instructions import InstructionBuilder

class IRSystem:
    """IR generation system with context management."""
    
    ib = InstructionBuilder()
    
    # =========================
    # Types
    # =========================
    class Type:
        def __init__(self, name, size, signed):
            self.name = name
            self.size = size
            self.signed = signed

        def __repr__(self):
            return self.name

    void = Type("void", 0, False)
    usize = Type("usize", 8, False)
    isize = Type("isize", 8, True)
    u8    = Type("u8",    1, False)
    ptr   = Type("ptr",   8, False)

    # =========================
    # IR Values
    # =========================
        
    class TypeManager:
        def __init__(self):
            self.typemap = {"void": {}}

        def add_type(self, name):
            if name in self.typemap:
                print(f"Error: Type already registered: {name}")
                return
            self.typemap[name] = {}
        
        def get_types(self):
            return self.typemap.keys()

    class Value:
        def __init__(self, typ, name):
            self.type = typ
            self.name = name
        def __repr__(self):
            return f"{self.name}:{self.type}"



    class VirtualRegisterManager:
        """Manages virtual register allocation."""
        
        def __init__(self):
            self.register_count = ARGS_REGISTERS + 1
            self.freed = []

            self.register_types = {}

        def new_vreg(self):
            """Allocate a new virtual register."""
            if self.freed:
                return self.freed.pop()
            self.register_count += 1
            return f"v{self.register_count}"

        def free_reg(self, reg):
            """Mark register as freed."""
            self.freed.append(reg)

        def get_register_type(self, reg):
            if not reg:
                print("Warning: Why are you trying to get the type of None?")
                return
            try:
                int(reg)
                print(f"Warning: Why are you giving the VRM int again? {reg}")
            except ValueError:
                pass

            if not (type(reg) == str and len(reg) > 1 and reg.startswith("v")):
                print(f"Warning: doesnt look much like a register does it? {reg}")
                return
            if reg in self.register_types:
                return self.register_types[reg]
            print(f"Warning: Register type not found: {reg}")

        def set_register_type(self, register, vtype):
            self.register_types[register] = vtype

    class MetaPipe:
        def __init__(self):
            self.pipes = {}
        
        def push(self, data, pipe="default"):
            #print(f"{data} pushed to pipe.")
            if pipe not in self.pipes:
                self.pipes[pipe] = []

            self.pipes[pipe].append(data)

        
        def pop(self, pipe="default"):
            if self.peek(pipe=pipe):
                return self.pipes[pipe].pop()
        
        def peek(self, pipe="default"):
            if pipe not in self.pipes:
                print(f"Warning: This is not a pipe: {pipe}")
                return None
            if len(self.pipes[pipe]) == 0:
                print("Nothing in pipe! check your logic!")
                return None    
            return self.pipes[pipe][-1]

        def pop_all(self, pipe="default"):
            if pipe not in self.pipes:
                return
            data = self.pipes[pipe]
            self.pipes[pipe] = []
            return data

    class BlockContext:
        """Context for tracking variables and registers."""
        
        def __init__(self, parent=None, name="global"):
            self.name = name
            self.vreg = IRSystem.VirtualRegisterManager()

            self.variables = {}
    
            self.types = IRSystem.TypeManager()
            
            #self.stack_ptr = IRSystem.Address("vSTACK_PTR", 0)
            self.stack_offset = 0

            self.static_memory = []

            self.pipe = IRSystem.MetaPipe()
            self.parent = parent

            self.code = []

        def emit(self, code):
            if isinstance(code, list):
                self.code.extend(code)
            self.code.append(code)

        def get_memory(self, size):
            m = self.memory_used
            self.memory_used += size
            return m

        def get_type(self, value):
            if not value:
                print("Warning: Trying to get type of none")
                return
            n = self.get_variable_type(value)
            if n : return n
            try:
                int(value)
                return "usize"
            except ValueError:
                pass
            
            try:
                num = int(value[1:])
            except ValueError:
                print(f"Warning: how can this even have a type? {type(value)}({value})")
                return

            if value.startswith("s"):
                add = self.get_static(value)
                return self.get_type(add)
            elif value.startswith("v"):
                reg_type = self.vreg.get_register_type(value)
                if reg_type: 
                    return reg_type
                else:
                    return
            print(f"No type found for {type(value)}({value})")


        def get_variable_register(self, var_name):
            """Get register for variable."""
            if var_name in self.variables:
                if "register" in self.variables[var_name]:
                    return self.variables[var_name]["register"]
            if self.parent:
                return self.parent.get_variable_register(var_name)
            return None

        def add_static(self, static):
            self.static_memory.append(static)
            return f"s{len(self.static_memory) - 1}"

        def get_static(self, static):
            idx = int(static[1:])
            if idx < len(self.static_memory):
                return self.static_memory[idx]

        def update_variable_register(self, var_name, reg):
            """Update variable register."""
            try:
                reg_num = int(reg)
                #TODO: somehow categorize imm and reg for assembly?
            except (ValueError, TypeError):
                pass
            if var_name not in self.variables:
                self.variables[var_name] = {}
            else:
                vtype = self.get_type(var_name)
                if not vtype:
                    print(f"Warning: Updating variable register before setting type '{var_name}'")
                self.vreg.set_register_type(reg, vtype)
            self.variables[var_name]["register"] = reg

        
        def get_variable_type(self, var_name):
            """Get type for variable."""
            if var_name in self.variables:
                if "type" in self.variables[var_name]:
                    return self.variables[var_name]["type"]
            if self.parent:
                return self.parent.get_variable_register(var_name)
            return None

        def set_variable_type(self, var_name, vtype):
            """Update variable register."""
            if var_name not in self.variables:
                self.variables[var_name] = {}
            if vtype not in self.types.get_types():
                print(f"Error: Invalid type: {vtype}")
                return
            self.variables[var_name]["type"] = vtype
            if "register" in self.variables[var_name]:
                reg = self.variables[var_name]["register"]
                self.vreg.set_register_type(vtype)


        def create_child_context(self, name):
            """Create child context."""
            child = IRSystem.BlockContext(parent=self, name=name)
            child.pipe = self.pipe
            child.types = self.types
            child.vreg = self.vreg
            return child
        
        def get_global_prefix(self):
            """Get fully qualified name."""
            t, s = self, []
            while t.parent is not None:
                s.append(t.name)
                t = t.parent
            s.append(t.name)
            s.reverse()
            return ".".join(s)
