novels = dbnl_without/*.sents.gz
threshold = 10

all: btest.sents all.alts apply-all 

# for now, words including |,[,] are removed because Alpino -lex_all will do funny things
words.freq: $(novels)
	zcat $(novels) | sed -e 's/^[^|]*[|]//' | tr ' ' '\n' |\
        sort | uniq -c | sort -nr | awk '{ if ($$1 > $(threshold)) print $$0 }'|\
        grep -v '[|]' > words.freq

# check unknown words, but (possibly) ignore words starting with capital/consisting of digits
#ignore = grep -v -e '^[A-Z]' -e '^[0-9][0-9]*$$'
ignore = cat
words.unknowns: words.freq
	awk '{ print $$2 }' words.freq | $(ignore)| Alpino -notk pos_tagger=off display_lexical_analysis=unknowns unknowns=off batch_command=lex_all >/dev/null 2> words.unknowns

auto: words.unknowns o.pl
	grep unknown: words.unknowns | awk '{print $$2 }'| Alpino -notk -l o batch_command=go  > auto

spelling: auto hand
	python3 add_cap.py < hand > handc
	python3 add_cap.py < auto > autoc
	sort -u auto autoc hand handc > spelling

pipe=python3 triples.py | python3 meta.py spelling | sed -f map.sed | python3 meta.py hand2



## dit genereert een test-setje met korte zinnen en relatief veel
## spel-aanpassingen (want "heeren" komt in betreffende boek minstens 10x voor)

btest:
	zgrep heeren -c $(novels) | awk -F: '{if ($$2 > 10) print $$1 }' |\
         xargs zcat | words -t 10> btest

ctest:
	zgrep heeren -c $(novels) | awk -F: '{if ($$2 < 10) print $$1 }' |\
         xargs zcat | words -t 10> ctest


%.sents: % meta.py triples.py nouns adj_pair det_pair map.sed hand2 # spelling
	 cat $< |\
         $(pipe) \
         > $*.sents

all.alts: dbnl_with/*.sents.gz
	zcat dbnl_with/*.sents.gz |grep -o ' [[][^]]*[]]' |sort | uniq -c | sort -nr > all.alts

adjn:
	zcat $(novels) |\
         grep -o ' den [a-z]*[abcdfghjklmnpqrstvwxyz]en [a-z]* ' |\
         awk '{print $$2 }' | sort | uniq -c |sort -nr |\
         awk '{ if ($$1>10) print $$2}' > adjn

adj_pair: adjn
	cat adjn | Alpino -notk -l p batch_command=adj |uniq > adj_pair

qnouns:
	zcat $(novels) | egrep -o '[ |](mijnen|dezen|den|zulken|een|eenen|hunnen|menigen|haren|zijnen|mijnen) [^ ][^ ]*en [^ ][^ ][^ ]* ' | awk '{ print $3 }' | sort -u > qnouns

nouns: qnouns q.pl
	cat qnouns |\
         Alpino -notk unknowns=off -l q batch_command=noun |\
         uniq > nouns

dbnl_with/%: dbnl_without/% triples.py spelling map.sed hand2
	zcat dbnl_without/$* |\
         $(pipe) |\
        gzip > dbnl_with/$*

apply-all: $(novels:dbnl_without/%=dbnl_with/%)

random-without.sents.gz:
	zcat $(novels) |\
          $(ALPINO_HOME)/Suites/random_lines -p 0.001 |\
          gzip > random-without.sents.gz

random-with.sents.gz: random-without.sents.gz spelling meta.py triples.py nouns adj_pair det_pair map.sed hand2
	zcat random-without.sents.gz |\
          $(pipe) |\
          gzip > random-with.sents.gz


## pipe through cat so make does not complain that there is an error if
## there is no output (which is what we want!)
check:
	grep '[[][^]]*[[]' *.sents |grep -v '[\][[]' |cat
	zgrep '[[][^]]*[[]' *.sents.gz |grep -v '[\][[]' |cat

### this will "apply" the meta-annotation, so that only the "modern" text remains
undo:
	@zcat $(file) | perl -p -e 's!\[ \@alt ~(\w+?)~\w+ .*?\]!$$1!g; s!\[ \@(alt|mwu_alt|phantom) (\w+) .*?\]!$$2!g; s!\[ \@skip .*? \] *!!g;' 
