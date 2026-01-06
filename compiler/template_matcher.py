"""Template matching system with wildcard group filtering."""
from typing import List, Tuple, Optional, Set, Union
from dataclasses import dataclass
from parser import split_sentences, parse as parse_tokens
from reader import Reader

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

@dataclass
class WildcardSpec:
    """Represents a wildcard with optional group filtering."""
    name: str
    groups: Optional[Set[Union[int, str]]]  # None means match all groups
    
    @classmethod
    def parse(cls, wildcard_value: any) -> 'WildcardSpec':
        """
        Parse wildcard specification.
        
        Formats supported:
        - "name" -> matches all template groups
        - ("name", [0, 1, 2]) -> matches only templates in groups 0, 1, 2
        - ("name", ["noun", "verb"]) -> matches only templates in named groups
        """
        if isinstance(wildcard_value, str):
            return cls(name=wildcard_value, groups=None)
        elif isinstance(wildcard_value, tuple) and len(wildcard_value) == 2:
            name, groups = wildcard_value
            if groups is None:
                return cls(name=name, groups=None)
            return cls(name=name, groups=set(groups))
        else:
            raise ValueError(f"Invalid wildcard specification: {wildcard_value}")
    
    def __str__(self):
        if self.groups is None:
            return f"*{self.name}"
        return f"*{self.name}[{','.join(map(str, self.groups))}]"

def _filter_templates_by_groups(parsed_templates, template_groups, allowed_groups: Optional[Set]) -> List[Tuple[int, List]]:
    """
    Filter templates by group membership.
    
    Args:
        parsed_templates: List of template patterns
        template_groups: Dict mapping template index to group(s)
        allowed_groups: Set of allowed group identifiers, or None for all
        
    Returns:
        List of (template_index, template_pattern) tuples
    """
    if allowed_groups is None:
        # No filtering - return all templates with their indices
        return list(enumerate(parsed_templates))
    
    filtered = []
    for idx, template in enumerate(parsed_templates):
        # Get groups for this template
        groups = set(template_groups.get(idx))
        
        # Check if template belongs to any allowed group
        if allowed_groups & groups:  # Set intersection
            filtered.append((idx, template))
    
    return filtered

def _match_wildcard(parsed_templates, template_groups, tokens: List[Token], 
                    wildcard_spec: WildcardSpec, debug: bool) -> Optional[List]:
    """Match wildcard content against templates, optionally filtered by group."""
    log = print if debug else lambda *a: None
    
    if not tokens:
        log(f"  ↳ Wildcard {wildcard_spec}: Empty token list, returning []")
        return []
    
    log(f"  ↳ Wildcard {wildcard_spec}: Attempting to match {len(tokens)} tokens")
    
    # Filter templates by group if specified
    filtered_templates = _filter_templates_by_groups(
        parsed_templates, 
        template_groups, 
        wildcard_spec.groups
    )
    
    if wildcard_spec.groups is not None:
        log(f"  ↳ Filtered to {len(filtered_templates)} templates in groups {wildcard_spec.groups}")
    
    token_tuples = [t.to_tuple() for t in tokens]
    results = template_match_filtered(
        parsed_templates, 
        template_groups,
        token_tuples, 
        filtered_templates,
        debug
    )
    
    if results:
        log(f"  ↳ Wildcard {wildcard_spec}: Successfully matched {len(results)} templates")
    else:
        log(f"  ↳ Wildcard {wildcard_spec}: No template matches found")
    
    return results if results else None

def single_template_match(tokens: List[Tuple], template: List[Tuple], 
                          parsed_templates, template_groups, debug: bool = False) -> Tuple[bool, List]:
    """
    Match a single template against tokens.
    
    Args:
        tokens: List of (type, value) token tuples
        template: List of (type, value) template pattern tuples
        parsed_templates: List of all template patterns
        template_groups: Dict mapping template index to group(s)
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
                log(f"  Action: Processing final wildcard {exp_val}")
                wildcard_spec = WildcardSpec.parse(exp_val)
                matches = _match_wildcard(
                    parsed_templates, 
                    template_groups,
                    [Token.from_tuple(t) for t in wildcard_buf], 
                    wildcard_spec,
                    debug
                )
                if matches is None:
                    log(f"  ✖ FAILED: Wildcard {wildcard_spec} did not match")
                    return False, []
                result.add_capture(wildcard_spec.name, matches)
                log(f"  ✔ Captured wildcard {wildcard_spec}: {len(matches)} matches")
                pi += 1
                continue
            log(f"  ✖ FAILED: Out of tokens at position {ti}")
            return False, []
        
        act_type, act_val = tokens[ti]
        log(f"  Actual:   ({act_type}, {act_val})")

        # Variable capture
        if exp_type == VARIABLE:
            if isinstance(exp_val, tuple):
                name = exp_val[0]
                chars = exp_val[1]
                if act_val not in chars:
                    return False, []
            else:
                name = exp_val

            log(f"  Action: Capturing matched variable ${name}")
            result.add_capture(name, act_val)
            log(f"  ✔ Captured ${name} = {act_val}")
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
            wildcard_spec = WildcardSpec.parse(exp_val)
            log(f"  Action: Processing wildcard {wildcard_spec}")
            log(f"    Wildcard buffer: {wildcard_buf}")
            
            # Check if next token ends wildcard
            if pi < len(template) - 1:
                next_pattern = template[pi + 1]
                current_token = tokens[ti]
                log(f"    Next pattern: {next_pattern}")
                
                success, matched = single_template_match([current_token], [next_pattern], parsed_templates, template_groups, debug)
                if success:
                    log(f"    Wildcard boundary detected!")
                    matches = _match_wildcard(
                        parsed_templates,
                        template_groups,
                        [Token.from_tuple(t) for t in wildcard_buf],
                        wildcard_spec,
                        debug
                    )
                    
                    if matches is None:
                        log(f"  ✖ FAILED: Wildcard {wildcard_spec} did not match buffer")
                        return False, []
                    result.add_capture(wildcard_spec.name, matches)
                    log(f"  ✔ Captured wildcard {wildcard_spec}: {len(matches)} matches")
                    wildcard_buf = []
                    #ti += 1
                    pi += 1
                    continue
            
            # Wildcard at end - consume all remaining tokens
            if pi == len(template) - 1:
                log(f"    Wildcard is at end of pattern")
                log(f"    Consuming remaining {len(tokens) - ti} tokens")
                wildcard_buf.extend(tokens[ti:])
                matches = _match_wildcard(
                    parsed_templates,
                    template_groups,
                    [Token.from_tuple(t) for t in wildcard_buf],
                    wildcard_spec,
                    debug
                )
                if matches is None:
                    log(f"  ✖ FAILED: Wildcard {wildcard_spec} did not match")
                    return False, []
                result.add_capture(wildcard_spec.name, matches)
                log(f"  ✔ Captured wildcard {wildcard_spec}: {len(matches)} matches")
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

            if act_type != exp_type:
                log(f"  ✖ FAILED: Block mismatch")
                return False, []

            log(f"    Recursing into block content...")
            block_valid, block_captures = single_template_match(
                act_val, exp_val, parsed_templates, template_groups, debug
            )
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

def template_match_filtered(parsed_templates: List, template_groups: dict,
                            tokens: List[Tuple], filtered_templates: List[Tuple[int, List]],
                            debug: bool = False) -> List:
    """
    Match tokens against filtered templates.
    
    Args:
        parsed_templates: List of all template patterns
        template_groups: Dict mapping template index to group(s)
        tokens: List of (type, value) token tuples to match
        filtered_templates: List of (index, template) tuples to try
        debug: Enable debug logging
        
    Returns:
        List of matched template results
    """
    log = print if debug else lambda *a: None
    matched = []
    
    sentences = split_sentences(tokens)
    log(f"\n{'#'*60}")
    log(f"FILTERED TEMPLATE MATCHING")
    log(f"{'#'*60}")
    log(f"Input tokens: {tokens}")
    log(f"Split into {len(sentences)} sentence(s)")
    log(f"Filtered templates: {len(filtered_templates)}")
    log(f"{'#'*60}\n")
    
    for s_idx, sent in enumerate(sentences):
        log(f"\n--- Sentence {s_idx + 1}/{len(sentences)} ---")
        log(f"Tokens: {sent}")
        log(f"Length: {len(sent)}")
        
        for t_idx, tmpl in filtered_templates:
            # Quick length check
            if len(tmpl) > len(sent):
                log(f"  Template {t_idx}: Skipped (too long: {len(tmpl)} > {len(sent)})")
                continue
            
            log(f"\n  → Trying template {t_idx}:")
            log(f"    Pattern: {tmpl}")
            
            valid, captures = single_template_match(
                sent, tmpl, parsed_templates, template_groups, debug
            )
            if valid:
                # Add template reference
                captures.append((TEMPLATE_REF, t_idx))
                matched.append(captures)
                log(f"  ✔✔✔ SENTENCE MATCHED with template {t_idx}")
                log(f"      Captures: {captures}")
                break
            else:
                log(f"  ✖✖✖ Template {t_idx} did not match")
    
    return matched

def template_match(parsed_templates: List, template_groups: dict, tokens: List[Tuple], 
                   debug: bool = False) -> List:
    """
    Match tokens against all registered templates.
    
    Args:
        parsed_templates: List of template patterns
        template_groups: Dict mapping template index to set of group identifiers
        tokens: List of (type, value) token tuples to match
        debug: Enable debug logging
        
    Returns:
        List of matched template results
    """
    log = print if debug else lambda *a: None
    
    # Match against all templates
    #filtered_templates = list(enumerate(parsed_templates))
    
    filtered_templates = _filter_templates_by_groups(
        parsed_templates, 
        template_groups, 
        set(["main"])
    )

    log(f"\n{'#'*60}")
    log(f"TEMPLATE MATCHING SESSION")
    log(f"{'#'*60}")
    log(f"Input tokens: {tokens}")
    log(f"Available templates: {len(parsed_templates)}")
    log(f"Template groups: {template_groups}")
    log(f"{'#'*60}\n")
    
    matched = template_match_filtered(
        parsed_templates, 
        template_groups,
        tokens, 
        filtered_templates,
        debug
    )
    
    log(f"\n{'#'*60}")
    log(f"MATCHING COMPLETE")
    log(f"{'#'*60}")
    log(f"Total matches: {len(matched)}")
    if matched:
        for i, match in enumerate(matched):
            log(f"  Match {i + 1}: {match}")
    log(f"{'#'*60}\n")
    
    return matched