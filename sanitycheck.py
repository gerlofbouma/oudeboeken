"""
python3 sanitycheck.py <original_tokenized_text> <annotated_tokenized_text>

Complains about unknown/mistaken annotations and (inadvertedly) changed source strings
"""

import sys
from itertools import count


class ParseError(Exception):
    pass
    

def phantom(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @phantom content')
            case (_, ']'):
                break
            #
            case (_, '['):
                raise ParseError('Unexpected open bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            # 
            case (0, w):
                pass
            case (_, w):
                raise ParseError('Unexpected @phantom content')
            # unreachable, forces generator
            case _:
                yield
    else:
        raise ParseError('Unclosed bracket')

    
def skip(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @skip content')
            case (_, ']'):
                break
            #
            case (_, '['):
                raise ParseError('Unexpected open bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            # 
            case (0, w):
                yield w
            case (_, w):
                raise ParseError('Unexpected @skip content')
    else:
        raise ParseError('Unclosed bracket')

  
def alt(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @alt content')
            case (1, ']'):
                raise ParseError('Missing @alt content')
            case (_, ']'):
                break
            #
            case (0, w) if w.startswith('~'):
                if w.count('~') < 2 or not all(w[1:].split('~')):
                    raise ParseError('Malformed alternatives')
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            #
            case (_, '['):
                raise ParseError('Unexpected bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            #
            case (0, w):
                pass
            case (1, w):
                yield w
            case (_, w):
                raise ParseError('Unexpected @alt content')
    else:
        raise ParseError('Unclosed bracket')

    
def postag(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @postag content')
            case (1, ']'):
                raise ParseError('Missing @postag content')
            case (_, ']'):
                break
            #
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            #
            case (_, '['):
                raise ParseError('Unexpected bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            #
            case (0, w):
                pass
            case (1, w):
                yield w
            case (_, w):
                raise ParseError('Unexpected @postag content')
    else:
        raise ParseError('Unclosed bracket')

    
def mwu_alt(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @mwu_alt content')
            case (1, ']'):
                raise ParseError('Missing @mwu_alt content')
            case (2, ']'):
                raise ParseError('Spurious @mwu_alt')
            case (_, ']'):
                break
            #
            case (0, w) if w.startswith('~'):
                if w.count('~') < 2 or not all(w[1:].split('~')):
                    raise ParseError('Malformed alternative')
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            #
            case (_, '['):
                raise ParseError('Unexpected bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            #
            case (0, w):
                pass
            case (_, w):
                yield w
    else:
        raise ParseError('Unclosed bracket')

    
def mwu(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, ']'):
                raise ParseError('Missing @mwu content')
            case (1, ']'):
                raise ParseError('Spurious @mwu')
            case (_, ']'):
                break
            #
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            case (_, '['):
                raise ParseError('Unexpected bracket')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            #
            case (_, w):
                yield w
    else:
        raise ParseError('Unclosed bracket')

    
def brackets(ws):
    for i, w in enumerate(ws):
        match (i, w):
            case (0, '@phantom'):
                yield from phantom(ws)
                break
            case (0, '@alt'):
                yield from alt(ws)
                break
            case (0, '@mwu_alt'): 
                yield from mwu_alt(ws)
                break
            case (0, '@mwu'):
                yield from mwu(ws)
                break
            case (0, '@postag'):
                yield from postag(ws)
                break
            case (0, '@skip'):
                yield from skip(ws)
                break
            case (0, w) if w.startswith('@'):
                raise ParseError('Unknown instruction')
            case (_, w) if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            case (0, ']'):
                raise ParseError('Empty brackets')
            case (_, w) if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            case (_, '['):
                yield from brackets(ws)
            case (_, ']'):
                break
            case _:
                yield w                
    else:
        raise ParseError('Unclosed bracket')
    



def nobrackets(ws):
    for w in ws:
        match w:
            case '[':
                yield from brackets(ws)
            case ']':
                raise ParseError('Unopened bracket')
            case w if w.startswith('@'):
                raise ParseError('Unexpected instruction')
            case w if w.startswith('~') and len(w) > 1:
                raise ParseError('Unexpected alternatives')
            case w:
                yield w
            
    

        
def check(fn_source, fn_annotated):
    with open(fn_source) as f_source:
        with open(fn_annotated) as f_annotated:
            for i, l_s, l_a in zip(count(1),f_source,f_annotated):
                *key_s, notkey_s = l_s.split('|',1)
                *key_a, notkey_a = l_a.split('|',1)

                if not key_s==key_a:
                    print('Line %s has differing keys: %s vs %s' % (i,key_s,key_a))
                    print()
                
                ws_s = notkey_s.split()
                ws_a = notkey_a.split()

                try:
                    ws_as = list(nobrackets(iter(ws_a)))
                    if not ws_s == ws_as:
                        print('Line %s was changed:\n%s\n%s' % (i,ws_s,ws_as))
                        print()
                except ParseError as p:
                    p.add_note('in line %s: %s' % (i, ws_a))
                    print(p)
                    print(*p.__notes__,sep='\n')
                    print()

                
                
            assert not next(f_source,False), 'Source file, %s, is longer.' % (fn_source,)
            assert not next(f_annotated,False), 'Annotated, %s, is longer.' % (fn_annotated,)
                


if __name__=='__main__':
    check(sys.argv[1], sys.argv[2])
