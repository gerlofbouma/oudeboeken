#!/usr/bin/env python3

import sys
import re

exp = re.compile('^([^|]*)[|](.*)$')

nouns="nouns"
adj_pair="adj_pair"
det_pair="det_pair"

adj = {}
noun = set()
det = {}

### TODO: nouns should also be map if in spelling...

def main():
    with open(adj_pair,'r') as f:
        for line in f:
            [old,new] = line.rstrip().split()
            adj[old] = new
    with open(det_pair,'r') as f:
        for line in f:
            [old,new] = line.rstrip().split()
            det[old] = new
    with open(nouns,'r') as f:
        for line in f:
            noun.add(line.rstrip())

    for line in sys.stdin:
        m=exp.match(line)
        key=m.group(1)
        words = m.group(2).split()            
        i=0
        nwords = []
        while i < len(words)-2:
            if words[i] in det and words[i+1] in adj and words[i+2] in noun:
                if det[words[i]] == words[i]:
                    ndetlist = [ words[i] ]
                else:
                    ndetlist = ["[","@alt",det[words[i]],words[i],"]" ]
                nwords = nwords + ndetlist + [ "[","@alt",adj[words[i+1]],words[i+1],"]",words[i+2]]
                i=i+3
            else:
                nwords.append(words[i])
                i=i+1
        if i< len(words):
            nwords.append(words[i])
        if i+1 < len(words):
            nwords.append(words[i+1])
        print("|".join((key," ".join(nwords))))


if __name__ == "__main__":
    main()
