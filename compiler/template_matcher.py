"""Template matching system for pattern recognition."""
from typing import List, Tuple, Optional
from dataclasses import dataclass
from parser import split_sentences, parse as parse_tokens
from reader import Reader
from handlers import get_parsed_templates

# Token type constants
VARIABLE = "$"
TEMPLATE_REF = "#"
WILDCARD = "*"

@dataclass
class Token:
    """Represents a parsed token."""
    type: str
    value: any
    
    @classmethod
    def from_tuple(cls, t: Tuple) -> 'Token':
        return cls(t[0], t[1])
    
    def to_tuple(self) -> Tuple:
        return (self.type, self.value)

@dataclass
class MatchResult:
    """Represents a template match result."""
    success: bool
    captures: List[Tuple[str, any]]
    
    def add_capture(self, name: str, value: any):
        self.captures.append((name, value))

parsed_templates = get_parsed_templates()

def _match_wildcard(tokens: List[Token], wildcard_name: str, debug: bool) -> Optional[List]:
    """Match wildcard content against templates."""
    log = print if debug else lambda *a: None
    
    if not tokens:
        log(f"  ↳ Wildcard *{wildcard_name}: Empty token list, returning []")
        return []
    
    log(f"  ↳ Wildcard *{wildcard_name}: Attempting to match {len(tokens)} tokens")
    token_tuples = [t.to_tuple() for t in tokens]
    results = template_match(parsed_templates, token_tuples, debug)
    
    if results:
        log(f"  ↳ Wildcard *{wildcard_name}: Successfully matched {len(results)} templates")
    else:
        log(f"  ↳ Wildcard *{wildcard_name}: No template matches found")
    
    return results if results else None

def single_template_match(tokens: List[Tuple], template: List[Tuple], debug: bool = False) -> Tuple[bool, List]:
    """
    Match a single template against tokens.
    
    Args:
        tokens: List of (type, value) token tuples
        template: List of (type, value) template pattern tuples
        debug: Enable debug logging
        
    Returns:
        (success, captures) where captures is list of (name, value) tuples
    """
    log = print if debug else lambda *a: None
    
    log(f"\n{'='*60}")
    log(f"MATCHING ATTEMPT")
    log(f"{'='*60}")
    log(f"Tokens:   {tokens}")
    log(f"Template: {template}")
    log(f"{'='*60}\n")
    
    result = MatchResult(True, [])
    wildcard_buf = []
    ti = pi = 0
    
    while pi < len(template):
        exp_type, exp_val = template[pi]
        
        log(f"[Step {pi}] Pattern position: {pi}/{len(template)-1}")
        log(f"  Token position: {ti}/{len(tokens)-1}")
        log(f"  Expected: ({exp_type}, {exp_val})")
        
        # Handle end of tokens
        if ti >= len(tokens):
            log(f"  Status: Reached end of tokens")
            if exp_type == WILDCARD:
                log(f"  Action: Processing final wildcard *{exp_val}")
                matches = _match_wildcard([Token.from_tuple(t) for t in wildcard_buf], exp_val, debug)
                if matches is None:
                    log(f"  ✖ FAILED: Wildcard *{exp_val} did not match")
                    return False, []
                result.add_capture(exp_val, matches)
                log(f"  ✔ Captured wildcard *{exp_val}: {len(matches)} matches")
                pi += 1
                continue
            log(f"  ✖ FAILED: Out of tokens at position {ti}")
            return False, []
        
        act_type, act_val = tokens[ti]
        log(f"  Actual:   ({act_type}, {act_val})")

        # Variable capture
        if exp_type == VARIABLE:
            log(f"  Action: Capturing variable ${exp_val}")
            result.add_capture(exp_val, act_val)
            log(f"  ✔ Captured ${exp_val} = {act_val}")
            ti += 1
            pi += 1
        
        # Template reference
        elif exp_type == TEMPLATE_REF:
            log(f"  Action: Matching template reference #{exp_val}")
            if exp_val != WILDCARD and (not act_type.startswith("t#") or int(act_type[2:]) != exp_val):
                log(f"  ✖ FAILED: Template reference mismatch")
                log(f"    Expected: #{exp_val}")
                log(f"    Got: {act_type}")
                return False, []
            log(f"  ✔ Template reference matched")
            ti += 1
            pi += 1
        
        # Wildcard matching
        elif exp_type == WILDCARD:
            log(f"  Action: Processing wildcard *{exp_val}")
            log(f"    Wildcard buffer: {wildcard_buf}")
            
            # Check if next token ends wildcard
            if pi < len(template) - 1:
                next_type, next_val = template[pi + 1]
                log(f"    Next pattern: ({next_type}, {next_val})")
                
                if next_type == act_type and next_val == act_val:
                    log(f"    Wildcard boundary detected!")
                    matches = _match_wildcard([Token.from_tuple(t) for t in wildcard_buf], exp_val, debug)
                    if matches is None:
                        log(f"  ✖ FAILED: Wildcard *{exp_val} did not match buffer")
                        return False, []
                    result.add_capture(exp_val, matches)
                    log(f"  ✔ Captured wildcard *{exp_val}: {len(matches)} matches")
                    wildcard_buf = []
                    ti += 1
                    pi += 2
                    continue
            
            # Wildcard at end - consume all remaining tokens
            if pi == len(template) - 1:
                log(f"    Wildcard is at end of pattern")
                log(f"    Consuming remaining {len(tokens) - ti} tokens")
                wildcard_buf.extend(tokens[ti:])
                matches = _match_wildcard([Token.from_tuple(t) for t in wildcard_buf], exp_val, debug)
                if matches is None:
                    log(f"  ✖ FAILED: Wildcard *{exp_val} did not match")
                    return False, []
                result.add_capture(exp_val, matches)
                log(f"  ✔ Captured wildcard *{exp_val}: {len(matches)} matches")
                log(f"\n{'='*60}")
                log(f"✔ MATCH SUCCESSFUL")
                log(f"{'='*60}")
                log(f"Captures: {result.captures}\n")
                return True, result.captures
            
            log(f"    Adding token to wildcard buffer")
            wildcard_buf.append(tokens[ti])
            ti += 1
        
        # Block matching
        elif len(exp_type) > 1:
            log(f"  Action: Matching block")
            log(f"    Expected block type: {exp_type}")
            log(f"    Actual block type: {act_type}")
            
            if len(act_type) != len(exp_type):
                log(f"  ✖ FAILED: Block type length mismatch")
                return False, []
            
            log(f"    Recursing into block content...")
            block_valid, block_captures = single_template_match(act_val, exp_val, debug)
            if not block_valid:
                log(f"  ✖ FAILED: Block content mismatch")
                return False, []
            
            result.captures.extend(block_captures)
            log(f"  ✔ Block matched with {len(block_captures)} captures")
            ti += 1
            pi += 1
        
        # Literal matching
        elif len(exp_type) == 1:
            log(f"  Action: Matching literal")
            if exp_type != act_type or act_val != exp_val:
                log(f"  ✖ FAILED: Literal mismatch")
                log(f"    Expected: ({exp_type}, '{exp_val}')")
                log(f"    Got: ({act_type}, '{act_val}')")
                return False, []
            log(f"  ✔ Literal matched")
            ti += 1
            pi += 1
        
        else:
            log(f"  Action: Skipping unknown type")
            ti += 1
            pi += 1
    
    # Check for unmatched tokens
    if ti < len(tokens):
        log(f"\n✖ FAILED: Unmatched tokens remaining")
        log(f"  Remaining: {tokens[ti:]}")
        return False, []
    
    log(f"\n{'='*60}")
    log(f"✔ MATCH SUCCESSFUL")
    log(f"{'='*60}")
    log(f"Captures: {result.captures}\n")
    return True, result.captures

def template_match(parsed_templates: List, tokens: List[Tuple], debug: bool = False) -> List:
    """
    Match tokens against all registered templates.
    
    Args:
        parsed_templates: List of template patterns
        tokens: List of (type, value) token tuples to match
        debug: Enable debug logging
        
    Returns:
        List of matched template results
    """
    log = print if debug else lambda *a: None
    matched = []
    
    sentences = split_sentences(tokens)
    log(f"\n{'#'*60}")
    log(f"TEMPLATE MATCHING SESSION")
    log(f"{'#'*60}")
    log(f"Input tokens: {tokens}")
    log(f"Split into {len(sentences)} sentence(s)")
    log(f"Available templates: {len(parsed_templates)}")
    log(f"{'#'*60}\n")
    
    for s_idx, sent in enumerate(sentences):
        log(f"\n--- Sentence {s_idx + 1}/{len(sentences)} ---")
        log(f"Tokens: {sent}")
        log(f"Length: {len(sent)}")
        
        for t_idx, tmpl in enumerate(parsed_templates):
            # Quick length check
            if len(tmpl) > len(sent):
                log(f"  Template {t_idx}: Skipped (too long: {len(tmpl)} > {len(sent)})")
                continue
            
            log(f"\n  → Trying template {t_idx}:")
            log(f"    Pattern: {tmpl}")
            
            valid, captures = single_template_match(sent, tmpl, debug)
            if valid:
                # Add template reference
                captures.append((TEMPLATE_REF, t_idx))
                matched.append(captures)
                log(f"  ✔✔✔ SENTENCE MATCHED with template {t_idx}")
                log(f"      Captures: {captures}")
                break
            else:
                log(f"  ✖✖✖ Template {t_idx} did not match")
    
    log(f"\n{'#'*60}")
    log(f"MATCHING COMPLETE")
    log(f"{'#'*60}")
    log(f"Total matches: {len(matched)}")
    if matched:
        for i, match in enumerate(matched):
            log(f"  Match {i + 1}: {match}")
    log(f"{'#'*60}\n")
    
    return matched