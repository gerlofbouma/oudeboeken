#!/usr/bin/env python3

import sys

def main():
    for line in sys.stdin:
        [w1,w2] = line.split()
        alts = w2.split("~")
        n = []
        for alt in alts:
            n.append(alt.capitalize())
        print("{} {}".format(w1.capitalize(),"~".join(n)))


if __name__ == "__main__":
    main()
