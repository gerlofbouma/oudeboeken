novels = /var/tmp/andreas/DBNLnovels-selection/*.gz
threshold = 10

all: btest.sents all.sents.gz all.alts apply-all split

# for now, words including | are removed because Alpino -lex_all will do funny things
words.freq: $(novels)
	zcat $(novels) | sed -e 's/^[^|]*[|]//' | tr ' ' '\n' |\
        sort | uniq -c | sort -nr | awk '{ if ($$1 > $(threshold) print $$0 }'|\
        grep -v '[|]' > words.freq

# check unknown words, but (possible) ignore words starting with capital/consisting of digits
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

all.sents.gz: spelling meta.py triples.py nouns adj_pair det_pair map.sed hand2
	 zcat /var/tmp/andreas/DBNLnovels-selection/*.gz |\
         $(pipe) |\
         gzip > all.sents.gz

all.alts: all.sents.gz
	zcat all.sents.gz |grep -o ' [[][^]]*[]]' |sort | uniq -c | sort -nr > all.alts

adjn:
	zcat /var/tmp/andreas/DBNLnovels-selection/* |\
         grep -o ' den [a-z]*[abcdfghjklmnpqrstvwxyz]en [a-z]* ' |\
         awk '{print $$2 }' | sort | uniq -c |sort -nr |\
         awk '{ if ($$1>10) print $$2}' > adjn

adj_pair: adjn
	cat adjn | Alpino -notk -l p batch_command=adj |uniq > adj_pair

qnouns:
	zcat /var/tmp/andreas/DBNLnovels-selection/* | egrep -o '[ |](mijnen|dezen|den|zulken|een|eenen|hunnen|menigen|haren|zijnen|mijnen) [^ ][^ ]*en [^ ][^ ][^ ]* ' | awk '{ print $3 }' | sort -u > qnouns

nouns: qnouns q.pl
	cat qnouns |\
         Alpino -notk unknowns=off -l q batch_command=noun |\
         uniq > nouns

apply:
	zcat /var/tmp/andreas/DBNLnovels-selection/$(novel).tok.gz |\
         $(pipe) |\
         gzip >DBNLnovels-selection/$(novel).tok.gz

apply-all:
	for title in `cat titles`; do $(MAKE) -s apply novel=$$title; done

split:
	( cd DBNLnovels-selection ; \
          for file in `cat ../titles` ; \
          do zcat $$file.tok.gz |\
             $(ALPINO_HOME)/Suites/split_in_parts -p 2500 -o $$file ;\
          done \
        )

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
