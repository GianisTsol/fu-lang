use core::str;
use std::env;
use std::process;

use std;
use std::collections::HashMap;

use std::fs;

use regex::Regex;

#[derive(Debug, Clone)]
enum VAR {
    Integer(i32),
    Text(String),
    Function(func_obj),
}

type SCOPE = HashMap<String, VAR>;

#[derive(Debug, Clone)]
struct func_obj {
    name: String,
    locals: SCOPE,
}

struct OPERATION {
    keyword: String,
    func_to_call: fn(obj1: VAR, obj2: VAR),
}

struct Parser {
    ops: Vec<OPERATION>,
    tree: Vec<VAR>,
    globals: SCOPE,
}

impl Parser {
    fn new() -> Parser {
        let ops: Vec<OPERATION> = vec![OPERATION {
            keyword: "+".to_string(),
            func_to_call: add,
        }];

        return Parser {
            ops: ops,
            tree: vec![],
            globals: HashMap::new(),
        };
    }
}

fn add(obj1: VAR, obj2: VAR) {}

fn parse_let(inst: String, scope: &mut HashMap<String, VAR>) {
    let binding = inst.replace(" ", "");
    let parts = binding.split("=");
    let collection: Vec<&str> = parts.collect();

    let val = collection[1].to_string();
    let name = collection[0].to_string()[3..].to_string();

    if val.starts_with('"') && val.ends_with('"') {
        let d = val.replace('"', "");
        println!("String {} = {}", &name, &d);
        scope.insert(name, VAR::Text(d));
    } else if scope.contains_key(&val) {
        let other = scope.get(&val);
        let other_var = other.unwrap().clone();
        println!("{:?}", &other_var);
        scope.insert(name, other_var);
    } else {
        // or, to be safe, match the `Err`
        match val.parse::<i32>() {
            Ok(n) => {
                println!("Integer: {} = {}", &name, &n);
                scope.insert(name, VAR::Integer(n));
            }
            Err(e) => eprintln!("Error: {:?}", e),
        }
    }
}

fn parse_func(inst: String, scope: &mut HashMap<String, VAR>) {
    let s: Vec<&str> = inst.split(" ").collect();
    // name regex: (?<=func )[^(-+]+(?=\(.+)
    // arg names from (arg1, arg2): (?<=(\(|\s*,\s*))[^(), ]+(?=\s*(\)|,\s*|))
    // content: (?<={).+(?=})

    let name_regex = Regex::new(r#"(?<=func )[^(-+]+(?=\(.+)"#).expect("Invalid regex pattern");
    let name = s[0];
    let args = "";
}

type MatchedFunction = fn(String, &mut HashMap<String, VAR>);

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <file_path>", args[0]);
        process::exit(1);
    }

    let file_path = args[1].clone();
    println!("Opening {}", &file_path);

    let file_content = fs::read_to_string(file_path).expect("Cant Open File");

    parse(file_content);
}

fn parse(file_content: String) {
    let content = file_content.replace('\n', " ").replace('\r', " ");

    let mut parser = Parser::new();

    let words = content.split_whitespace();

    let k = vec!["let", "func", "+", "-", "/", "(", ")", "{", "}"];

    let mut expect: Vec<&str> = vec![];
    for (word_index, word) in words.enumerate() {
        if let Some(expected) = expect.last() {
            if expected != &word {
                for i in &k {
                    if (&word == i) {
                        println!("Unexpected {}, expected {}", &word, expected);
                        process::exit(0);
                    }
                }
            }
        }
        match word {
            "let" => {
                expect.push("=");
                expect.push(";");
            }
            _ => {
                println!("");
            }
        }
    }
}
