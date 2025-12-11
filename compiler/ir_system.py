"""Main IR generation system with register management."""

from config import ARGS_REGISTERS
from ir_instructions import InstructionBuilder

class IRSystem:
    """IR generation system with context management."""
    
    ib = InstructionBuilder()
    
    class VirtualRegisterManager:
        """Manages virtual register allocation."""
        
        def __init__(self):
            self.register_count = ARGS_REGISTERS + 1
            self.freed = []

        def new_vreg(self):
            """Allocate a new virtual register."""
            if self.freed:
                return self.freed.pop()
            self.register_count += 1
            return f"v{self.register_count}"

        def free_reg(self, reg):
            """Mark register as freed."""
            self.freed.append(reg)
    
    class TypeManager:
        def __init__(self):
            self.typemap = {}

        def add_type(self, name):
            if name in self.typemap:
                print(f"Error: Type already registered: {name}")
                return
            self.typemap[name] = {}
                    

    class BlockContext:
        """Context for tracking variables and registers."""
        
        def __init__(self, parent=None, name="global"):
            self.name = name
            self.vreg = IRSystem.VirtualRegisterManager()

            self.variables = {}
            self.types = []
            self.parent = parent

        def get_variable_register(self, var_name):
            """Get register for variable."""
            if var_name in self.variables:
                return self.variables[var_name]["register"]
            if self.parent:
                return self.parent.get_variable_register(var_name)
            return None

        def update_variable_register(self, var_name, reg):
            """Update variable register."""
            try:
                reg_num = int(reg)
                reg = f"v{reg_num}"
            except (ValueError, TypeError):
                pass
            if var_name not in self.variables:
                self.variables[var_name] = {}
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
            if vtype not in self.types:
                print(f"Error: Invalid type: {vtype}")
                return
            self.variables[var_name]["type"] = vtype

        def create_child_context(self, name):
            """Create child context."""
            child = IRSystem.BlockContext(parent=self, name=name)
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
