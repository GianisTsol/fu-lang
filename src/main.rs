use core::str;
use std::collections::HashMap;

use std::env;
use std::fmt::format;
use std::ops::Index;
use std::process;

use std::fs;
use std::usize;

type SCOPE = Vec<String>;

#[derive(Clone)]

enum VAR {
    Char,
    Int,
    Ptr,
}

#[derive(Clone, Debug)]
struct FuncCallObj {
    func: String,
    args: Vec<VarType>,
}
#[derive(Clone, Debug)]
struct OpCallObj {
    op: String,
    args: Vec<VarType>,
}

impl OpCallObj {
    fn get_variables_involved(&self) -> Vec<String> {
        let mut g = vec![];
        for arg in &self.args {
            match arg {
                VarType::LocalVar(f, _) => g.push(f.clone()),
                _ => {}
            }
        }
        return g;
    }
}

#[derive(Clone, Debug)]
enum VarType {
    LocalVar(String, i32),
    InitVar(VarInitObj),
    StaticVar(StaticValObj),
    CallOp(OpCallObj),
    CallFunc(FuncCallObj),
}

#[derive(Debug, Clone)]
struct VarInitObj {
    name: String,
    val: Box<VarType>,
    indexes: Box<VarType>, // TODO: implement more types
}

impl VarInitObj {
    fn get_variables_involved(&self) -> Vec<String> {
        let mut g = vec![];
        match &*self.val {
            VarType::LocalVar(f, _) => g.push(f.clone()),
            VarType::CallOp(f) => g.extend(f.get_variables_involved()),
            _ => {}
        }
        return g;
    }
}
struct TempVarObj {
    id: i32,
}
#[derive(Clone, Debug)]
struct StaticValObj {
    val: i32,
}

struct FuncObj {
    name: String,
    locals: SCOPE,
    block: Vec<VarType>,
    args: Vec<String>,
    props: Vec<(String, String)>,
}

impl FuncObj {
    fn new(name: String) -> FuncObj {
        return FuncObj {
            name: name,
            locals: vec![],
            block: vec![],
            args: vec![],
            props: vec![],
        };
    }

    fn get_variables_last_use(&self) -> HashMap<String, usize> {
        let mut p: HashMap<String, usize> = HashMap::new();

        for arg in &self.args {
            p.insert(arg.clone(), 0);
        }
        for (i, v) in self.block.iter().enumerate() {
            match v {
                VarType::CallOp(o) => {
                    for j in o.get_variables_involved() {
                        p.insert(j, i);
                    }
                }
                VarType::InitVar(o) => {
                    p.insert(o.name.clone(), i);
                    for j in o.get_variables_involved() {
                        p.insert(j, i);
                    }
                }
                VarType::CallFunc(o) => {
                    for h in &o.args {
                        match h {
                            VarType::LocalVar(qq, _) => {
                                p.insert(qq.clone(), i);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        return p;
    }

    fn parse_props(&mut self, props: Vec<String>) {
        for i in &props {
            let name = i[..i.find("\n").unwrap()]
                .replace(" ", "")
                .replace("\n", "")
                .replace("\r", "")
                .to_string();
            let args = i[i.find("\n").unwrap()..].to_string();

            if name == "_regsd" {
                for a in args.split(",") {
                    self.props.push(("_regsd".to_string(), a.to_string()));
                }
            } else if name == "_asm" {
                self.props.push(("_asm".to_string(), args));
            }
        }
    }
}

pub struct Parser {
    tree: Vec<VAR>,
    funcs: Vec<FuncObj>,
    name: String,
    imports: Vec<String>,
}

impl Parser {
    fn new() -> Parser {
        return Parser {
            name: "".to_string(),
            tree: vec![],
            funcs: vec![],
            imports: vec![],
        };
    }
}

fn parse_func(inst: String, parser: &Parser) -> FuncObj {
    let name = inst[0..inst.find("(").unwrap_or(0)]
        .to_string()
        .replace("func ", "");

    let mut args: Vec<String> = vec![];
    for i in inst[inst.find("(").unwrap_or(0) + 1..inst.find(")").unwrap_or(1)].split(",") {
        if i.replace(" ", "").len() > 0 {
            args.push(i.replace(" ", ""));
        }
    }
    let mut body = inst[inst.find("{").unwrap_or(0) + 1..inst.find("}").unwrap_or(0)].to_string();

    if name == "" {
        panic!("No function name specified");
    }
    let mut f = FuncObj::new(name.to_string().clone());

    for i in args {
        //let n = VarInitObj {
        //    name: i.to_string(),
        //    val: Box::new(VarType::StaticVar(StaticValObj { val: 0 })),
        //};
        f.args.push(i.clone());
        //f.block.push(VarType::InitVar(n));
        f.locals.push(i.clone());
    }

    let props = split_blocks("_", ";", &body);

    f.parse_props(props);
    for i in &f.props {
        if i.0 == "_asm" {
            return f;
        }
    }

    let body = body.replace('\n', "").replace('\r', "");
    for cmd in body.split(";") {
        let tt = parse_var_init(cmd.to_string(), parser, &mut f);
        if let Some(VarType::InitVar(_)) = tt {
            f.block.push(tt.unwrap());
            continue;
        }
        let op = parse_exp(cmd.to_string(), &parser, &f);
        match op {
            Some(v) => {
                match v {
                    VarType::CallOp(ref _q) => {
                        f.block.push(v);
                        continue;
                    }
                    VarType::CallFunc(ref _m) => {
                        f.block.push(v);
                        continue;
                    }
                    _ => {}
                }
                continue;
            }
            None => {}
        }
    }

    return f;
}

fn parse_call(cmd: String, func: &FuncObj, parser: &Parser) -> Option<FuncCallObj> {
    let name = cmd[0..cmd.find("(").unwrap_or(0)]
        .to_string()
        .replace("func ", "")
        .replace(" ", "");

    for i in ["(", ")"] {
        match cmd.find(i) {
            Some(_a) => {}
            None => {
                return None;
            }
        }
    }
    let mut args: Vec<VarType> = vec![];
    for i in cmd[cmd.find("(").unwrap_or(0) + 1..cmd.find(")").unwrap_or(1)].split(",") {
        let v = i.replace(" ", "");
        if v.len() > 0 {
            args.push(parse_var(v, func, parser));
        }
    }

    if name == "" {
        panic!("No function name specified");
    }

    println!("func call {} - args: {:?}", &name, &args);

    let c = FuncCallObj {
        func: name.to_string().clone(),
        args: args,
    };

    return Some(c);
}
fn parse_var_init(cmd: String, parser: &Parser, f: &mut FuncObj) -> Option<VarType> {
    for dec in vec!["int ", "char "] {
        if cmd.contains(&dec) {
            let ind = cmd.find(dec).unwrap_or(0) + dec.len();
            let mut ne = cmd.len();
            let mut val = VarType::StaticVar(StaticValObj { val: 0 });
            let mut arr_sz = VarType::StaticVar(StaticValObj { val: 1 });

            if cmd.contains("=") {
                ne = cmd.find("=").unwrap() + 1;
                match parse_exp(cmd[ne..].to_string(), &parser, &f) {
                    Some(ops) => {
                        val = ops;
                    }
                    None => {}
                }
            }
            if cmd.contains("[") && cmd.contains("]") {
                let a = cmd.find("[").unwrap();
                let b = cmd.find("]").unwrap();
                if b < ne {
                    ne = a + 1;
                    arr_sz = parse_var(cmd[a + 1..b].to_string(), &f, &parser);
                }
            }
            let name = (cmd[ind..ne - 1]).to_string().replace(" ", "");
            println!("aaaa {}", name.as_str());
            f.locals.push(name.clone());
            let m = VarType::InitVar(VarInitObj {
                name: name,
                val: Box::new(val),
                indexes: Box::new(arr_sz),
            });
            return Some(m);
        }
    }
    None
}
fn parse_exp(acmd: String, parser: &Parser, func: &FuncObj) -> Option<VarType> {
    let mut d = 1000;
    let mut s = "";
    for i in vec!["+", "-", "="] {
        let l = acmd.find(i).unwrap_or(d);
        if l < d {
            d = l + 1;
            s = i;
        }
    }
    let cmd = acmd;
    if s != "" {
        let g = parse_exp(cmd[d..].to_string(), parser, &func);
        match g {
            None => {}
            Some(o) => {
                let obj1 = parse_var(cmd[..d - 1].to_string(), &func, parser);
                return Some(VarType::CallOp(OpCallObj {
                    op: s.to_string(),
                    args: vec![obj1, o],
                }));
            }
        }
    } else {
        if cmd.replace(" ", "").len() > 0 {
            return Some(parse_var(cmd, &func, parser));
        }
    }
    return None;
}

fn parse_var(v: String, func: &FuncObj, parser: &Parser) -> VarType {
    let val = v.replace(" ", "");
    let mut arr_i = 0;
    let mut t: usize = val.len();
    if val.contains("[") && val.contains("]") {
        let a = val.find("[").unwrap();
        let b = val.find("]").unwrap();

        let ggg = parse_var(val[a + 1..b].to_string(), &func, &parser);
        match ggg {
            VarType::StaticVar(q) => arr_i = q.val,
            _ => {}
        }
        println!("{:?}", &arr_i);
        t = a;
    }
    let var_name = val[..t].to_string();
    println!("{:?}", &func.locals);
    for i in &func.locals {
        if i == &var_name {
            return VarType::LocalVar(var_name, arr_i);
        }
    }
    match parse_call(v, func, parser) {
        Some(c) => {
            return VarType::CallFunc(c);
        }
        None => {}
    };
    match val.parse::<i32>() {
        Ok(n) => {
            return VarType::StaticVar(StaticValObj { val: n });
        }
        Err(e) => {
            panic!("Error: {:?} {}", e, val);
        }
    }
}

pub fn parse(file_content: String) -> Parser {
    let content = file_content;

    let mut parser = Parser::new();

    let funcs = split_blocks("func ", "}", &content);
    let imports = split_blocks("import ", ";", &content);

    for f in funcs {
        let mut d = parse_func(f, &parser);

        parser.funcs.push(d)
    }
    for i in imports {
        let fc = fs::read_to_string(format!("std/{}.fu", i[7..i.len() - 1].to_string()))
            .expect("Cant Open File");
        let gg = remove_comments(fc);
        parser.funcs.extend(parse(gg).funcs);
    }

    parser
}

fn execute_start(parser: &Parser) {
    for i in &parser.funcs {
        if i.name == "main" {
            execute(i.block.clone(), &parser);
        }
    }
}

fn execute_set(mut op: OpCallObj, scope: &mut HashMap<String, i32>) -> i32 {
    if op.op == "=" {
        let mut val = 0;
        match op.args.pop().unwrap() {
            VarType::CallOp(t) => {
                val = execute_op_tree(t, scope);
            }
            _ => {}
        };

        let mut name = String::new();
        match op.args.pop().unwrap() {
            VarType::LocalVar(v, _) => name = v,
            _ => {}
        };
        scope.insert(name, val);
    }
    return 0;
}
fn execute_op_tree(op: OpCallObj, scope: &mut HashMap<String, i32>) -> i32 {
    if op.op == "=" {
        return execute_set(op, scope);
    }
    let mut prs: Vec<i32> = vec![];
    for arg in op.args {
        let mut tmp = 0;
        match arg {
            VarType::CallOp(a) => {
                tmp = execute_op_tree(a, scope);
            }
            VarType::StaticVar(a) => {
                tmp = a.val;
            }
            VarType::LocalVar(a, _) => {
                tmp = scope.get(&*a).unwrap().clone();
            }
            _ => {}
        }
        prs.push(tmp);
    }

    match op.op.as_str() {
        "+" => {
            return prs[0] + prs[1];
        }
        "-" => {
            return prs[0] - prs[1];
        }
        _ => {}
    }
    return 0;
}

fn execute(block: Vec<VarType>, parser: &Parser) -> VAR {
    let mut scope: HashMap<String, i32> = HashMap::new();
    for i in block {
        match i {
            VarType::CallOp(a) => {
                let _n = execute_op_tree(a, &mut scope);
            }
            VarType::InitVar(a) => {
                match *a.val {
                    VarType::StaticVar(b) => {
                        scope.insert(a.name, b.val);
                    }
                    VarType::InitVar(_b) => {}
                    VarType::LocalVar(b, _) => {
                        let other = scope.get(&b);
                        scope.insert(a.name, other.unwrap().clone());
                    }
                    VarType::CallOp(b) => {
                        let res = execute_op_tree(b, &mut scope);
                        scope.insert(a.name, res);
                    }
                    VarType::CallFunc(_b) => {}
                };
            }
            VarType::CallFunc(a) => {
                for i in &parser.funcs {
                    if i.name == a.func {
                        let _r = execute(i.block.clone(), parser);
                    }
                }
            }
            _ => {}
        }
    }
    return VAR::Int;
}
fn split_blocks(start: &str, end: &str, content: &String) -> Vec<String> {
    let mut results: Vec<String> = vec![];

    let mut index: usize = 0;
    while true {
        let s = content[index..].find(start);

        let f = content[index..].find(end);

        match (s, f) {
            (Some(a), Some(b)) => {
                results.push(content[index + a..index + b + 1].to_string().clone());
                index += b + 1;
            }
            (None, _) => {
                break;
            }
            (Some(_), None) => {
                panic!("For {} Expected {} ", start, end);
            }
        }
    }

    return results;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <file_path>", args[0]);
        process::exit(0);
    }

    let file_path = args[2].clone();
    println!("Opening {}", &file_path);

    let file_content = fs::read_to_string(file_path).expect("Cant Open File");
    let fi = remove_comments(file_content);

    let mut p = parse(fi);
    let act = args[1].clone();
    p.name = args[2].clone();
    match act.as_str() {
        "build" => assemble(&p),
        "run" => execute_start(&p),
        _ => {}
    }
    println!("Finished");
}

fn remove_comments(s: String) -> String {
    let mut buf = vec![];
    for i in s.lines() {
        let n = i[0..(i.find("//").unwrap_or(i.len()))].to_string();
        if n.replace(" ", "").len() > 0 {
            buf.push(n);
            buf.push("\n".to_string());
        }
    }
    buf.join("")
}
#[derive(Clone)]
struct VarStore {
    name: String,
    register: String,
    var_type: VAR,
    size: usize,
    block_size: usize, // char foo[5 <---]
}

impl VarStore {
    fn asm_move_to_reg(&mut self, reg: String) -> String {
        if self.block_size != 1 {
            println!("Tried moving array in memory to register");
            return "".to_string();
        }
        let t = format!("     mov {}, [{}]", &reg, self.register);
        if self.register != reg {}
        self.register = reg;
        return t;
    }
}
struct RegMan {
    variables: HashMap<String, VarStore>,
    available: Vec<String>,
    total: Vec<String>,
    function_reg_destroy: HashMap<String, Vec<String>>,
    variables_last_mention: HashMap<String, usize>,
}

impl RegMan {
    fn new() -> RegMan {
        let mut s = vec![];
        for i in ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp"] {
            s.push(i.to_string());
        }
        s.reverse();
        RegMan {
            variables: HashMap::new(),
            available: s.clone(),
            total: s.clone(),
            function_reg_destroy: HashMap::new(),
            variables_last_mention: HashMap::new(),
        }
    }

    fn get_free_reg(&mut self) -> String {
        return self.available.pop().unwrap();
    }

    fn free_reg(&mut self, s: String) {
        self.available.push(s);
    }

    fn save_var(&mut self, s: String, name: String) {
        self.variables.insert(
            name.clone(),
            VarStore {
                name: name,
                register: s,
                var_type: VAR::Int,
                size: 8,
                block_size: 1,
            },
        );
    }

    fn save_mem_ptr(&mut self, s: String, name: String, block_size: usize) {
        if let Some(pos) = self.available.iter().position(|x| *x == s) {
            self.available.remove(pos);
        }
        self.variables.insert(
            name.clone(),
            VarStore {
                name: name,
                register: s,
                var_type: VAR::Ptr,
                size: 8,
                block_size: block_size,
            },
        );
    }
    fn get_var_ptr(&self, name: &String) -> String {
        let k = self.variables.get(name).unwrap();
        match k.var_type {
            VAR::Ptr => k.register.clone(),
            _ => "".to_string(),
        }
    }

    fn move_var(&mut self, name: String, index: usize, register: String) -> String {
        let mut buf = vec![];
        println!("{}", &name);
        let k = self.variables.get(&name).unwrap();
        match k.var_type {
            VAR::Ptr => {
                let mut temp_ptr_reg = k.register.clone();
                if index > 0 {
                    temp_ptr_reg = self.get_free_reg();
                    buf.push(format!(
                        "     lea {}, qword [{} + {}]\n",
                        &temp_ptr_reg,
                        self.get_var_ptr(&name),
                        index * 8
                    ));
                    self.free_reg(temp_ptr_reg.clone());
                }
                buf.push(format!(
                    "    mov {}, qword [{}]\n",
                    &register, &temp_ptr_reg
                ));
            }
            _ => {}
        };
        buf.join("")
    }
    fn set_var(&mut self, name: String, index: usize, s: VarType) -> String {
        let mut buf = vec![];
        println!("{}", &name);
        let k = self.variables.get(&name).unwrap();
        match k.var_type {
            VAR::Ptr => {
                let mut temp_ptr_reg = k.register.clone();
                if index > 0 {
                    temp_ptr_reg = self.get_free_reg();
                    buf.push(format!(
                        "     lea {}, qword [{} + {}]\n",
                        &temp_ptr_reg,
                        self.get_var_ptr(&name),
                        index * 8
                    ));
                    self.free_reg(temp_ptr_reg.clone());
                }
                match s {
                    VarType::StaticVar(st) => {
                        buf.push(format!("    mov qword [{}], {}\n", &temp_ptr_reg, &st.val));
                    }
                    VarType::LocalVar(name, ind) => {
                        let t = self.get_free_reg();
                        buf.push(self.move_var(name.clone(), ind as usize, t.clone()));
                        buf.push(format!(
                            "    mov qword [{}], {} ; {}\n",
                            &temp_ptr_reg, &t, &name
                        ));
                        self.free_reg(t);
                    }
                    _ => {}
                }
            }
            _ => {}
        };
        buf.join("")
    }

    fn destroys(&mut self, l: String) -> String {
        let mut buf = vec![];
        for h in self.variables.clone() {
            if h.1.register == l {
                let n = self.available.pop();
                match n {
                    Some(f) => {
                        self.free_reg(h.1.register.clone());
                        match h.1.var_type {
                            VAR::Ptr => {
                                self.save_mem_ptr(f.clone(), h.1.name.clone(), h.1.block_size)
                            }
                            VAR::Int => self.save_var(f.clone(), h.1.name.clone()),
                            _ => {}
                        }
                        buf.push(format!(
                            "     mov {}, {} ; not preserved\n",
                            f,
                            h.1.register.clone()
                        ));
                    }
                    None => {
                        panic!("no available registers, the dev hasnt implemented stack use yet")
                    }
                };
            }
        }
        return buf.join("");
    }

    fn var_out_of_scope(&mut self, v: &String) {
        let mut k = self.variables.get_mut(v).cloned();
        match k {
            Some(gg) => {
                println!("Variable out of scope: {}", &gg.name);
                self.remove(&gg.name.clone());
            }
            None => {
                println!("Unknown var out of scope this should NOT happen");
            }
        }
    }

    fn remove(&mut self, k: &String) {
        let v = self.variables.get(k).unwrap();
        self.available.push(v.register.clone());
        self.variables.remove(k);
    }

    fn prepare_func_call(&mut self, func_name: String) -> String {
        for i in &self.function_reg_destroy {
            if i.0 == &func_name {
                for k in i.1 {
                    return self.destroys(k.clone());
                }
            }
        }
        return "".to_string();
    }
}

fn assemble_inst(op: &&VarType, regs: &mut RegMan, func: &FuncObj) -> String {
    match op {
        VarType::InitVar(q) => {
            let mut sz: usize = 8;
            match *q.indexes.clone() {
                VarType::StaticVar(ss) => sz = ss.val as usize,
                _ => {}
            }
            let mut buf: Vec<String> = vec![];
            buf.push(regs.destroys("rax".to_string()));
            buf.push(format!("     mov rax, {}\n     call alloc\n", sz * 8));
            regs.save_mem_ptr("rax".to_string(), q.name.clone(), sz.clone());

            if sz == 1 {
                buf.push(regs.set_var(q.name.clone(), 0, (*q.val).clone()));
            }

            return buf.join("");
        }
        VarType::CallFunc(q) => {
            let mut buf = vec![];
            for (index, arg) in q.args.iter().enumerate() {
                match arg {
                    VarType::LocalVar(ff, ind) => {
                        let a = &regs.total[regs.total.len() - 1 - index].clone();
                        buf.push(regs.prepare_func_call(q.func.clone()));
                        buf.push(format!(";Load arg {}: {}\n", &index, &ff));
                        buf.push(regs.move_var(ff.clone(), ind.clone() as usize, a.clone()));
                    }
                    _ => {}
                }
            }
            buf.push(format!("     call {}\n", &q.func));

            buf.join("")
        }
        VarType::CallOp(q) => {
            match q.op.as_str() {
                "=" => {
                    match &q.args[0] {
                        VarType::LocalVar(ll, ind) => {
                            return regs.set_var(
                                ll.clone(),
                                ind.clone() as usize,
                                q.args[1].clone(),
                            );
                        }
                        _ => {}
                    };
                }
                _ => {}
            };
            String::new()
        }
        _ => String::new(),
    }
}

fn assemble_new_function(func: &FuncObj, regs: &mut RegMan) -> String {
    for i in &func.props {
        if i.0 == "_asm" {
            return i.1.clone();
        }
    }
    let mut buf = Vec::new();

    for (i, n) in func.args.iter().enumerate() {
        regs.save_var(regs.total[regs.total.len() - 1 - i].clone(), n.clone());
    }

    let lm = func.get_variables_last_use();
    for op in func.block.iter().enumerate() {
        for l in lm.keys() {
            let endl = lm.get(l.as_str()).unwrap();
            if op.0 == *endl + 1 {
                &regs.var_out_of_scope(&l);
            }
        }

        buf.push(assemble_inst(&op.1, regs, &func));
    }
    buf.join("")
}

fn assemble(parser: &Parser) {
    let mut rgs = RegMan::new();
    let mut buf = Vec::new();

    buf.push(
        "section .bss
                buffer resb 8 ; 64 bits \n"
            .to_string(),
    );
    buf.push("section .text\n     global _start\n".to_string());
    for func in &parser.funcs {
        buf.push(format!("{}:\n", func.name));

        for argi in 0..func.args.len() {
            buf.push(format!(
                "     ; {}: {}\n",
                func.args[argi],
                rgs.total[rgs.total.len() - 1 - argi]
            ));
        }
        buf.push(assemble_new_function(&func, &mut rgs));
        buf.push("\n".to_string());
        buf.push("    ret\n".to_string());
    }
    fs::write(format!("{}.asm", parser.name), buf.join("")).expect("Unable to write file");
}
