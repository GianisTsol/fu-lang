use core::str;
use std::collections::HashMap;

use std::env;
use std::process;

use std;

use std::fs;

type VAR = i32;

type SCOPE = Vec<String>;

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
#[derive(Clone, Debug)]
enum VarType {
    LocalVar(String),
    InitVar(VarInitObj),
    StaticVar(StaticValObj),
    CallOp(OpCallObj),
    CallFunc(FuncCallObj),
}

#[derive(Debug, Clone)]
struct VarInitObj {
    name: String,
    val: Box<VarType>,
    // TODO: implement more types
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
}

impl FuncObj {
    fn new(name: String) -> FuncObj {
        return FuncObj {
            name: name,
            locals: vec![],
            block: vec![],
        };
    }
}

pub struct Parser {
    tree: Vec<VAR>,
    funcs: Vec<FuncObj>,
    name: String,
}

impl Parser {
    fn new() -> Parser {
        return Parser {
            name: "".to_string(),
            tree: vec![],
            funcs: vec![],
        };
    }
}

fn parse_func(inst: String, parser: &Parser) -> FuncObj {
    let name = inst[0..inst.find("(").unwrap_or(0)]
        .to_string()
        .replace("func ", "");

    let mut args: Vec<String> = vec![];
    for i in inst[inst.find("(").unwrap_or(0) + 1..inst.find(")").unwrap_or(1)].split(",") {
        args.push(i.replace(" ", ""));
    }
    let body = inst[inst.find("{").unwrap_or(0) + 1..inst.find("}").unwrap_or(0)].to_string();

    if name == "" {
        panic!("No function name specified");
    }

    let mut f = FuncObj::new(name.to_string().clone());

    for i in args {
        let n = VarInitObj {
            name: i.to_string(),
            val: Box::new(VarType::StaticVar(StaticValObj { val: 0 })),
        };
        f.block.push(VarType::InitVar(n));
        f.locals.push(i.clone());
    }

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

fn parse_call(cmd: String) -> Option<FuncCallObj> {
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
    let mut args: Vec<String> = vec![];
    for i in cmd[cmd.find("(").unwrap_or(0) + 1..cmd.find(")").unwrap_or(1)].split(",") {
        args.push(i.replace(" ", ""));
    }

    if name == "" {
        panic!("No function name specified");
    }

    println!("func call {}", &name);

    let c = FuncCallObj {
        func: name.to_string().clone(),
        args: vec![],
    };

    return Some(c);
}
fn parse_var_init(cmd: String, parser: &Parser, f: &mut FuncObj) -> Option<VarType> {
    for dec in vec!["int "] {
        if cmd.contains(&dec) {
            let ind = cmd.find(dec).unwrap_or(0) + dec.len();
            let mut ne = cmd.len();
            let mut val = VarType::StaticVar(StaticValObj { val: 0 });
            if cmd.contains("=") {
                ne = cmd.find("=").unwrap() + 1;
                match parse_exp(cmd[ne..].to_string(), &parser, &f) {
                    Some(ops) => {
                        val = ops;
                    }
                    None => {}
                }
            }
            let name = (cmd[ind..ne - 1]).to_string().replace(" ", "");
            f.locals.push(name.clone());
            let m = VarType::InitVar(VarInitObj {
                name: name,
                val: Box::new(val),
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
        if cmd.replace(" ", "") != "" {
            return Some(parse_var(cmd, &func, parser));
        }
    }
    return None;
}

fn parse_var(v: String, func: &FuncObj, parser: &Parser) -> VarType {
    let val = v.replace(" ", "");
    println!("{}", &v);
    for i in &func.locals {
        if i == &val {
            return VarType::LocalVar(i.clone());
        }
    }
    match parse_call(v) {
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
    let content = file_content.replace('\n', "").replace('\r', "");

    let mut parser = Parser::new();

    let funcs = split_blocks("func ", "}", content);

    for f in funcs {
        let d = parse_func(f, &parser);
        println!("++++ {} ++++", &d.name);
        for i in &d.block {
            println!("{:?}", i);
        }
        println!("++++++++++++++++++++++");
        parser.funcs.push(d)
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
            VarType::LocalVar(v) => name = v,
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
            VarType::LocalVar(a) => {
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
                    VarType::LocalVar(b) => {
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
    println!("{:?}", scope);
    return 0;
}
fn split_blocks(start: &str, end: &str, content: String) -> Vec<String> {
    let mut results: Vec<String> = vec![];

    let mut index: usize = 0;
    let stop = false;
    while !stop {
        let s = content[index..].find(start).unwrap_or(content.len());

        let f = content[index..].find(end).unwrap_or(0);
        if (s == content.len()) && f == 0 {
            return results;
        }
        if s == content.len() {
            panic!("Parsing error, {} not found", start)
        } else if f == 0 {
            panic!("Parsing error, {} not found", end)
        }
        results.push(content[index + s..index + f + 1].to_string().clone());
        index += f + 1;
    }

    return results;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <file_path>", args[0]);
        process::exit(0);
    }

    let file_path = args[1].clone();
    println!("Opening {}", &file_path);

    let file_content = fs::read_to_string(file_path).expect("Cant Open File");

    let mut p = parse(file_content);
    p.name = args[1].clone();
    //execute_start(&p);
    assemble(&p);
}

struct RegMan {
    available: Vec<String>,
    total: Vec<String>,
}

impl RegMan {
    fn new(registers: &Vec<&str>) -> RegMan {
        let mut s = vec![];
        for i in registers {
            s.push(i.to_string());
        }
        RegMan {
            available: s.clone(),
            total: s.clone(),
        }
    }

    fn borrow(&mut self) -> String {
        return self.available.pop().unwrap();
    }

    fn free(&mut self, s: String) {
        self.available.push(s);
    }
}

fn assemble_inst(op: &&VarType, regs: &RegMan) -> String {
    match op {
        VarType::InitVar(q) => {
            let mut buf: Vec<String> = vec![];
            buf.push(format!("{}", "     mov eax, 4\n     call alloc\n"));
            match &*q.val {
                VarType::StaticVar(z) => {
                    buf.push(format!("     mov     dword [eax] , {}\n", z.val));
                }
                _ => {}
            }
            return buf.join("");
        }
        VarType::CallFunc(q) => {
            format!("{}", "")
        }
        VarType::CallOp(q) => {
            format!("{}", "")
        }
        _ => String::new(),
    }
}
fn assemble_new_function(block: &Vec<VarType>, regs: &RegMan) -> String {
    let mut buf = Vec::new();
    for op in block {
        buf.push(assemble_inst(&op, regs));
    }
    buf.join("")
}
fn assemble(parser: &Parser) {
    let mut rgs = RegMan::new(&vec!["eax", "ebx", "ecx", "edx"]);
    let mut buf = Vec::new();
    for func in &parser.funcs {
        buf.push(format!("{}:\n", func.name));
        buf.push(assemble_new_function(&func.block, &rgs));
        buf.push("     ret\n".to_string());
    }
    fs::write(format!("{}.asm", parser.name), buf.join("")).expect("Unable to write file");
}
