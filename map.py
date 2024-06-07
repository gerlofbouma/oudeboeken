#!/usr/bin/env python3

import sys

spellfile="spelling"
spell = {}

def main():
    with open(spellfile,'r') as f:
        for line in f:
            [old,new] = line.rstrip().split()
            spell[old] = new
    for line in sys.stdin:
        word = line.rstrip()
        print(spell.get(word,word))
    


if __name__ == "__main__":
    main()
