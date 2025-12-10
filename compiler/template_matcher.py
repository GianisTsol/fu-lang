"""Template matching system for pattern recognition."""

from parser import split_sentences
from reader import Reader
from parser import parse as parse_tokens
from handlers import get_parsed_templates

parsed_templates = get_parsed_templates()


def single_template_match(tokens, template, debug=False):
    """Match a single template against tokens."""
    log = (lambda *a: print("[DEBUG]", *a)) if debug else lambda *a: None
    
    obj, wildcard_buf, ti, pi = [], [], 0, 0
    
    while pi < len(template):
        exp_type, exp_val = template[pi]
        
        if ti >= len(tokens):
            if exp_type == "*":
                obj.append((exp_val, wildcard_buf))
                pi += 1
                continue
            log(f"✖ Out of tokens at {ti}, template at {pi}")
            return False, []
        
        act_type, act_val = tokens[ti]
        log(f"Compare token[{ti}] {tokens[ti]} vs template[{pi}] {template[pi]}")

        if exp_type == "$":
            obj.append((exp_val, act_val))
            log(f"→ Captured ${exp_val} = '{act_val}'")
            ti, pi = ti + 1, pi + 1
        
        elif exp_type == "#":
            if exp_val != "*" and (not act_type.startswith("t#") or int(act_type[2:]) != exp_val):
                log(f"✖ Template ref mismatch")
                return False, []
            ti, pi = ti + 1, pi + 1
        
        elif exp_type == "*":
            if pi < len(template) - 1:
                next_type, next_val = template[pi + 1]
                if next_type == act_type and next_val == act_val:
                    obj.append((exp_val, wildcard_buf))
                    log(f"→ Wildcard *{exp_val} = {wildcard_buf}")
                    wildcard_buf = []
                    ti, pi = ti + 1, pi + 2
                    continue
            if pi == len(template) - 1:
                wildcard_buf.extend(tokens[ti:])
                obj.append((exp_val, wildcard_buf))
                log(f"→ Wildcard *{exp_val} = {wildcard_buf}")
                return True, obj
            wildcard_buf.append(tokens[ti])
            ti += 1
        
        elif len(exp_type) > 1:
            block_results = []
            for sent in split_sentences(act_val):
                valid, _ = single_template_match(sent, exp_val, debug)
                if not valid:
                    return False, []
                block_results.extend(template_match(parsed_templates, sent, debug))
            obj.append((exp_val[0][1] if exp_val else "block", block_results))
            ti, pi = ti + 1, pi + 1
        
        elif len(exp_type) == 1:
            if exp_type != act_type or act_val != exp_val:
                log(f"✖ Mismatch: '{act_val}' != '{exp_val}'")
                return False, []
            log(f"✓ Matched '{act_val}'")
            ti, pi = ti + 1, pi + 1
        else:
            ti, pi = ti + 1, pi + 1
    
    if ti < len(tokens):
        log(f"✖ Tokens remaining: {tokens[ti:]}")
        return False, []
    
    log(f"✔ MATCH: {obj}")
    return True, obj


def template_match(parsed_templates, tokens, debug=False):
    """Match tokens against all registered templates."""
    log = (lambda *a: print("[DEBUG]", *a)) if debug else lambda *a: None
    
    matched = []
    for s_idx, sent in enumerate(split_sentences(tokens)):
        for t_idx, tmpl in enumerate(parsed_templates):
            if len(tmpl) > len(sent):
                continue
            valid, obj = single_template_match(sent, tmpl, debug)
            if valid:
                log(f"✔ Match {t_idx} on sentence {s_idx}")
                obj.append(("#", t_idx - 1))
                matched.append(obj)
                break
    return matched

