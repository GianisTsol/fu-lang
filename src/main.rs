use core::str;
use std::collections::HashMap;
use std::env;
use std::env::args;
use std::process;
use std::rc::Rc;

use std;

use std::fs;

type VAR = i32;

type SCOPE = Vec<String>;

struct func_call {
    func: String,
    args: Vec<VAR>,
}

#[derive(Debug)]
struct op_call {
    op: String,
    args: Vec<VarType>,
}

#[derive(Debug)]
enum VarType {
    LocalVar(String),
    InitVar(var_init),
    StaticVar(static_val),
    TempVar(op_call),
}

enum Actions {
    OpCall(op_call),
    InitVar(var_init),
}
#[derive(Debug)]
struct var_init {
    name: String,
    val: Box<VarType>,
    // TODO: implement more types
}

struct temp_var {
    id: i32,
}

#[derive(Debug)]
struct static_val {
    val: i32,
}

impl var_init {
    fn new(name: String, val: VarType) -> var_init {
        var_init {
            name: name,
            val: Box::new(val),
        }
    }
}
trait callable {
    fn ret(&self) -> String;
}

impl callable for static_val {
    fn ret(&self) -> String {
        self.val.to_string()
    }
}

impl callable for op_call {
    fn ret(&self) -> String {
        String::new()
    }
}

struct func_obj {
    name: String,
    locals: SCOPE,
    block: Vec<VarType>,
}

impl func_obj {
    fn new(name: String) -> func_obj {
        return func_obj {
            name: name,
            locals: vec![],
            block: vec![],
        };
    }
}

struct Parser {
    tree: Vec<VAR>,
    globals: SCOPE,
}

impl Parser {
    fn new() -> Parser {
        return Parser {
            tree: vec![],
            globals: vec![],
        };
    }
}

fn parse_func(inst: String, parser: &Parser) -> func_obj {
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

    let mut f = func_obj::new(name.to_string().clone());

    for i in args {
        println!("|{}|", &i);
        let n = var_init {
            name: i.to_string(),
            val: Box::new(VarType::StaticVar(static_val { val: 0 })),
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
        println!("parsing op {:?}", &cmd);
        let op = parse_exp(cmd.to_string(), &parser, &f);
        match op {
            Some(v) => {
                match v {
                    VarType::TempVar(ref q) => {
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

fn parse_var_init(cmd: String, parser: &Parser, f: &mut func_obj) -> Option<VarType> {
    for dec in vec!["int "] {
        if cmd.contains(&dec) {
            let ind = cmd.find(dec).unwrap_or(0) + dec.len();
            let mut ne = cmd.len();
            println!("_____{:?}______", &cmd);
            let mut val = VarType::StaticVar(static_val { val: 0 });
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
            println!("{:?}", &val);
            let m = VarType::InitVar(var_init {
                name: name,
                val: Box::new(val),
            });
            return Some(m);
        }
    }
    None
}
fn parse_exp(acmd: String, parser: &Parser, func: &func_obj) -> Option<VarType> {
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
    if (s != "") {
        let mut g = parse_exp(cmd[d..].to_string(), parser, &func);
        match g {
            None => {
                let obj1 = parse_var(cmd[..d - 1].to_string(), &func.locals);
                let obj2 = parse_var(cmd[d..].to_string(), &func.locals);

                let res = VarType::TempVar(op_call {
                    op: s.to_string().clone(),
                    args: vec![obj1, obj2],
                });
                return Some(res);
            }
            Some(o) => {
                let mut obj1 = parse_var(cmd[..d - 1].to_string(), &func.locals);
                println!("aaa {:?} {:?} a a aaa", &obj1, &o);
                return Some(VarType::TempVar(op_call {
                    op: s.to_string(),
                    args: vec![obj1, o],
                }));
            }
        }
    } else {
        return None;
    }
}

fn parse_var(v: String, scope: &SCOPE) -> VarType {
    let val = v.replace(" ", "");
    for i in scope {
        if *i == val {
            return VarType::LocalVar(i.clone());
        }
    }
    match val.parse::<i32>() {
        Ok(n) => {
            return VarType::StaticVar(static_val { val: n });
        }
        Err(e) => {
            panic!("Error: {:?} {}", e, val);
        }
    }
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

    parse(file_content);
}

fn parse(file_content: String) {
    let content = file_content.replace('\n', "").replace('\r', "");

    let mut parser = Parser::new();

    let funcs = split_blocks("func ", "}", content);

    for f in funcs {
        let d = parse_func(f, &parser);
        println!("++++ INSTRUCTIONS ++++");
        for i in &d.block {
            println!("{:?}", i);
        }
        println!("++++++++++++++++++++++");
        execute(d.block);
    }
}

fn execute_set(mut op: op_call, scope: &mut HashMap<String, i32>) -> i32 {
    if op.op == "=" {
        let mut val = 0;
        match op.args.pop().unwrap() {
            VarType::TempVar(t) => {
                val = execute_op_tree(t, scope);
            }
            _ => {}
        };

        let mut name = String::new();
        match op.args.pop().unwrap() {
            VarType::LocalVar(v) => name = v,
            _ => {}
        };
        println!("{:?} ++++++ {:?}", &name, &val);
        scope.insert(name, val);
    }
    return 0;
}
fn execute_op_tree(op: op_call, scope: &mut HashMap<String, i32>) -> i32 {
    if op.op == "=" {
        return execute_set(op, scope);
    }
    let mut prs: Vec<i32> = vec![];
    for arg in op.args {
        let mut tmp = 0;
        match arg {
            VarType::TempVar(a) => {
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

fn execute(block: Vec<VarType>) {
    let mut scope: HashMap<String, i32> = HashMap::new();
    for i in block {
        match i {
            VarType::LocalVar(a) => {}
            VarType::StaticVar(a) => {}
            VarType::TempVar(a) => {
                let n = execute_op_tree(a, &mut scope);
            }
            VarType::InitVar(a) => {
                match *a.val {
                    VarType::StaticVar(b) => {
                        scope.insert(a.name, b.val);
                    }
                    VarType::InitVar(b) => {}
                    VarType::LocalVar(b) => {
                        let other = scope.get(&b);
                        scope.insert(a.name, other.unwrap().clone());
                    }
                    VarType::TempVar(b) => {
                        let res = execute_op_tree(b, &mut scope);
                        scope.insert(a.name, res);
                    }
                };
            }
        }
    }
    println!("{:?}", scope);
}
fn split_blocks(start: &str, end: &str, content: String) -> Vec<String> {
    let mut word = "".to_owned();

    let mut results: Vec<String> = vec![];

    let mut index: usize = 0;

    let mut stop = false;
    while !stop {
        let s = content[index..].find(start).unwrap_or(content.len());

        let f = content[index..].find(end).unwrap_or(0);

        if (s == content.len()) && f == 0 {
            return results;
        } else if s == content.len() {
            panic!("Parsing error, {} not found", start)
        } else if f == 0 {
            panic!("Parsing error, {} not found", end)
        }

        index += f;

        results.push(content[s..f + 1].to_string().clone());
    }

    return results;
}
