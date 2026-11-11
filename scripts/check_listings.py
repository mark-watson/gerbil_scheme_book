#!/usr/bin/env python3
"""
check_listings.py — Validate that code blocks in manuscript/*.md match source_code/

Scans manuscript markdown files for code blocks that specify their source filename
(via leading comment like ';; File: filename.ss' or ';; test-tool.ss' or prior heading)
and checks for drifts against source_code/.
"""

import os
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANUSCRIPT = ROOT / "manuscript"
SOURCE_CODE = ROOT / "source_code"

# Index all source files in source_code
SOURCE_FILES = {}
for p in SOURCE_CODE.rglob("*"):
    if p.is_file() and not p.name.startswith("."):
        SOURCE_FILES[p.name] = p

def scan_markdown_file(md_path):
    issues = []
    text = md_path.read_text(encoding="utf-8")
    
    # Regex to match code blocks with optional language
    pattern = re.compile(r"```(?:scheme|console|bash|c|makefile)?\n(.*?)```", re.DOTALL)
    for match in pattern.finditer(text):
        block = match.group(1).strip()
        lines = block.splitlines()
        if not lines:
            continue
        
        # Check first few lines for a file reference
        target_name = None
        for line in lines[:3]:
            m = re.search(r";;\s*(?:File:\s*)?([a-zA-Z0-9_\-]+\.(?:ss|c|h|txt))", line)
            if m:
                target_name = m.group(1)
                break
        
        if target_name and target_name in SOURCE_FILES:
            src_file = SOURCE_FILES[target_name]
            src_text = src_file.read_text(encoding="utf-8").strip()
            
            # Compare lines (ignoring leading/trailing blank lines)
            block_lines = [l.rstrip() for l in lines]
            src_lines = [l.rstrip() for l in src_text.splitlines()]
            
            # Check if block is a substring or identical
            block_clean = "\n".join(block_lines)
            if block_clean not in src_text and src_text not in block_clean:
                # Not an exact substring, could be abridged or drifted
                issues.append({
                    "md_file": md_path.name,
                    "target": target_name,
                    "src_path": str(src_file.relative_to(ROOT)),
                    "status": "drift or abridged"
                })
    return issues

def main():
    print("Checking manuscript listings against source_code/...")
    all_issues = []
    for md in sorted(MANUSCRIPT.glob("*.md")):
        issues = scan_markdown_file(md)
        all_issues.extend(issues)
    
    if not all_issues:
        print("All tagged listings match source files!")
        return 0
    
    print(f"Found {len(all_issues)} tagged listings with potential drift or abridgment:")
    for iss in all_issues:
        print(f"  - {iss['md_file']} -> {iss['target']} ({iss['src_path']}): {iss['status']}")
    return 0

if __name__ == "__main__":
    sys.exit(main())
