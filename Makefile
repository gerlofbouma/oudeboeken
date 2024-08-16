## IF you want to generate your own rule set on the basis of a number
## of (tokenized documents), then you need to redefine the novels and
## novelsdir variables
##
## the tok.gz files in the novels directory should be in the tokenized
## Alpino format: every line is a sentence prefixed by a key
##
##
## 1|The first sentence ...
## 2|The scond one !
## 3|Etcetera .


#novels = $(wildcard dbnl_without/*.sents.gz)
novels = $(wildcard $(novelsdir)/*.tok.gz)
novelsdir = /mnt/local/tmp/andreas/DBNL-20230214/output/tokenized
tests = $(wildcard TestWithout/*.tok)
threshold = 5

all: btest.sents all.alts apply-all 

# for now, words including |,[,] are removed because Alpino -lex_all will do funny things
words.freq: $(novels)
	find $(novelsdir) -name '*.tok.gz' | xargs zcat | sed -e 's/^[^|]*[|]//' | tr ' ' '\n' |\
        sort | uniq -c | sort -nr | awk '{ if ($$1 > $(threshold)) print $$0 }'|\
        grep -v '[|]' > words.freq

### TODO: lex_all is overkill, use mlex from pretty.pl instead?
# check unknown words, but (possibly) ignore words starting with capital/consisting of digits
#ignore = grep -v -e '^[A-Z]' -e '^[0-9][0-9]*$$'
ignore = cat
# words.unknowns: words.freq
# 	awk '{ print $$2 }' words.freq | $(ignore)| Alpino -notk pos_tagger=off filter_lexical_analysis=off display_lexical_analysis=unknowns unknowns=off batch_command=lex_all >/dev/null 2> words.unknowns

words.unknowns: words.freq report_missing_lex.pl
	awk '{ print $$2 }' words.freq | $(ignore)|\
        Alpino -notk -l report_missing_lex batch_command=go >words.unknowns

auto: words.unknowns generate_alt_spelling.pl
	grep unknown: words.unknowns |\
        awk '{print $$2 }'|\
        Alpino -notk -l generate_alt_spelling batch_command=go  > auto

spelling: auto hand
	python3 add_cap.py < hand > handc
	sort -u auto hand handc > spelling

check_spelling: spelling
	awk '{ print $$1 }' spelling | sort | uniq -d

pipe=python3 triples.py | python3 meta.py spelling | sed -f map.sed | python3 meta.py hand2

%.sents: % meta.py triples.py nouns adj_pair det_pair map.sed hand2 spelling
	 cat $< |\
         $(pipe) \
         > $*.sents

all.alts: dbnl_with/*.sents.gz
	zcat dbnl_with/*.sents.gz |grep -o ' [[][^]]*[]]' |sort | uniq -c | sort -nr > all.alts

adjn:
	find $(novelsdir) -name '*.tok.gz' | xargs zcat |\
         grep -o ' den [a-z]*[abcdfghjklmnpqrstvwxyz]en [a-z]* ' |\
         awk '{print $$2 }' | sort | uniq -c |sort -nr |\
         awk '{ if ($$1>10) print $$2}' > adjn

adj_pair: adjn generate_adj_pair.pl
	sort adjn | Alpino -notk -l generate_adj_pair batch_command=go |uniq > adj_pair

qnouns:
	find $(novelsdir) -name '*.tok.gz' | xargs zcat |\
	egrep -o '[ |](mijnen|dezen|den|zulken|een|eenen|hunnen|menigen|haren|zijnen|mijnen) [^ ][^ ]*en [^ ][^ ][^ ]* ' | awk '{ print $$3 }' | sort -u > qnouns

nouns: qnouns q.pl
	cat qnouns |\
         Alpino -notk unknowns=off -l q batch_command=noun |\
         uniq > nouns

dbnl_with/%.sents.gz: dbnl_without/%.sents.gz triples.py spelling map.sed hand2 nouns adj_pair det_pair 
	zcat dbnl_without/$*.sents.gz |\
         $(pipe) |\
        gzip > dbnl_with/$*.sents.gz

TestWith/%.tok: TestWithout/%.tok triples.py spelling map.sed hand2 nouns adj_pair det_pair 
	cat TestWithout/$*.tok |\
         $(pipe) \
        > TestWith/$*.tok

apply-all: $(novels:dbnl_without/%.sents.gz=dbnl_with/%.sents.gz) $(tests:TestWithout/%.tok=TestWith/%.tok)

## pipe through cat so make does not complain that there is an error if
## there is no output (which is what we want!)
check:
	grep '[[][^]]*[[]' *.sents |grep -v '[\][[]' |cat
	zgrep '[[][^]]*[[]' *.sents.gz |grep -v '[\][[]' |cat

### this will "apply" the meta-annotation, so that only the "modern" text remains
undo:
	@zcat $(file) | perl -p -e 's!\[ \@alt ~(\w+?)~\w+ .*?\]!$$1!g; s!\[ \@(alt|mwu_alt|phantom) (\w+) .*?\]!$$2!g; s!\[ \@skip .*? \] *!!g;' 
