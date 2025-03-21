#!/usr/bin/python3


import os
import sys
import re

def process_yaml_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content_orig = f.readlines()

    content_new = content_orig

    # Extract name from file name
    base_name = os.path.basename(file_path).replace('.yaml', '')

    # Save modified and original files
    dir_name = os.path.dirname(file_path)
    orig_file = os.path.join(dir_name, f'{base_name}-lc-36.yaml')
    mod_file = os.path.join(dir_name, f'{base_name}-lc-1.yaml')

    # Modify 'name' field
    content_orig = [re.sub(r"(name:\s*)'[^']+'", rf"\g<1>'{base_name}-lc-36'", line) for line in content_orig]
    content_new =  [re.sub(r"(name:\s*)'[^']+'", rf"\g<1>'{base_name}-lc-1'", line) for line in content_new]

    # Modify 'operation: any' to 'operation: none' by context
    found = False
    for i, line in enumerate(content_new):
        if found and re.match(r'\s*operation:\s*\'any\'', line):
            content_new[i] = re.sub(r'any', 'none', line)
            found = False

        if re.match(r'\s*- type:\s*\'composed\'', line):
            found = True

        # print( "LINEA:" + content[i] )

    # Modify 'local-use-section:'
    found = False
    for i, line in enumerate(content_new):
        if found and re.match(r'\s*template-number:', line):
            content_new[i] = re.sub(r'36', '1', line)
            found = False

        if re.match(r'\s*local-use-section:', line):
            found = True

        # print( "LINEA:" + content_new[i] )


    for i, line in enumerate(content_new):
        print( line.rstrip('\n') )




def main():
    if len(sys.argv) < 2:
        print("Usage: python script.py <directory>")
        sys.exit(1)

    file_path = sys.argv[1]

    process_yaml_file(file_path)

if __name__ == "__main__":
    main()
