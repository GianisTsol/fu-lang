use core::str;
use std::collections::HashMap;

use std::collections::btree_map::Range;
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

impl OpCallObj {
    fn get_variables_involved(&self) -> Vec<String> {
        let mut g = vec![];
        for arg in &self.args {
            match arg {
                VarType::LocalVar(f) => g.push(f.clone()),
                _ => {}
            }
        }
        return g;
    }
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
                }
                VarType::CallFunc(o) => {
                    for h in &o.args {
                        match h {
                            VarType::LocalVar(qq) => {
                                p.insert(qq.clone(), i);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        println!("{:?} gggggg", &p);
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
        println!("{}", &i.0);
        if i.0 == "_asm" {
            println!("{:?}", i.0);
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
        if cmd.replace(" ", "").len() > 0 {
            return Some(parse_var(cmd, &func, parser));
        }
    }
    return None;
}

fn parse_var(v: String, func: &FuncObj, parser: &Parser) -> VarType {
    let val = v.replace(" ", "");
    for i in &func.locals {
        if i == &val {
            return VarType::LocalVar(i.clone());
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

        println!("++++ {} ++++", &d.name);
        for i in &d.block {
            println!("{:?}", i);
        }
        println!("++++++++++++++++++++++");

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
    return 0;
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

    let file_path = args[1].clone();
    println!("Opening {}", &file_path);

    let file_content = fs::read_to_string(file_path).expect("Cant Open File");
    let fi = remove_comments(file_content);

    let mut p = parse(fi);
    p.name = args[1].clone();
    //execute_start(&p);
    assemble(&p);
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
struct RegMan {
    variables: HashMap<String, String>,
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

    fn borrow(&mut self) -> String {
        println!("{:?}", self.available);
        return self.available.pop().unwrap();
    }

    fn free(&mut self, s: String) {
        self.available.push(s);
    }

    fn save_var(&mut self, s: String, name: String) {
        self.variables.insert(name, s);
    }
    fn destroys(&mut self, l: String) -> String {
        let mut buf = vec![];
        for h in self.variables.clone() {
            if h.1 == l {
                let n = self.available.pop();
                match n {
                    Some(f) => {
                        self.free(h.1.clone());
                        self.save_var(f.clone(), h.0.clone());
                        buf.push(format!("     mov {}, {} ; not preserved\n", f, h.1.clone()));
                    }
                    None => {}
                };
            }
        }
        return buf.join("");
    }

    fn var_out_of_scope(&mut self, v: &String) {
        println!("{}", &v);
        let mut k = self.variables.get_mut(v).cloned();
        match k {
            Some(gg) => {
                self.remove(&gg.clone());
            }
            None => {
                println!("Unknown var out of scope this should NOT happen");
            }
        }
    }

    fn remove(&mut self, k: &String) {
        self.available.push(k.clone());
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
            let mut buf: Vec<String> = vec![];
            buf.push(regs.destroys("rax".to_string()));
            buf.push(format!("{}", "     mov rax, 8\n     call alloc\n"));
            let r = regs.borrow();
            match &*q.val {
                VarType::StaticVar(z) => {
                    buf.push(format!("; Initialize variable {}\n", &q.name));
                    buf.push(format!("     mov     dword [rax] , {}\n", z.val));
                    buf.push(format!("     mov {}, [rax]\n", &r));
                }
                _ => {}
            }
            regs.save_var(r, q.name.clone());
            return buf.join("");
        }
        VarType::CallFunc(q) => {
            let mut buf = vec![];
            for (index, arg) in q.args.iter().enumerate() {
                match arg {
                    VarType::LocalVar(ff) => {
                        let a = &regs.total[regs.total.len() - 1 - index].clone();
                        println!("d {}", &ff);
                        println!("dffffff {:?}", &regs.variables);
                        let b = regs.variables.get(ff).unwrap().clone();
                        buf.push(regs.prepare_func_call(q.func.clone()));
                        println!();
                        if a != &b {
                            buf.push(format!(
                                "     mov {}, {} ;Load arg {}: {}\n",
                                a, b, &index, &ff
                            ));
                        }
                    }
                    _ => {}
                }
            }
            buf.push(format!("     call {}\n", &q.func));

            buf.join("")
        }
        VarType::CallOp(q) => {
            format!("{}", "")
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
                println!("sssfff {}", &l);
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
