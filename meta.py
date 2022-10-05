#!/usr/bin/env python3

import sys
import re

spellfile=sys.argv[1]
spell = {}
exp = re.compile('^([^|]*)[|](.*)$')

def main():
    with open(spellfile,'r') as f:
        for line in f:
            [old,new] = line.rstrip().split()
            spell[old] = new
    for line in sys.stdin:
        in_brackets = False
        m=exp.match(line)
        key=m.group(1)
        words = m.group(2).split()            
        nwords = []
        for word in words:
            if word == '[':
                in_brackets = True
            elif word == ']':
                in_brackets = False
            if not in_brackets and word in spell:
                new = " ".join( ["[", "@alt" , spell[word] , word , "]" ])
            else:
                new = word
            nwords.append(new)
        print("|".join((key," ".join(nwords))))


if __name__ == "__main__":
    main()
