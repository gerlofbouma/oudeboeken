#!/usr/bin/env python3
import sys
assert sys.version_info >= (3,10) # (match/case, |-operator for dicts, str.removeprefix)


# NB! De ordening van bestandsnamen is relevant, omdat soms mappings
# uit eerdere bestanden gecorrigeerd worden in latere.

               # Adj met -n
               # [man gen/dat/acc ev], [onz gen ev], [onz/man dat mv]
               # en mapping naar (evt) -n loos
adj_pair_fns = ['adj_pair', 'adj_pair_spelling', 'adj_pair_gcramp',
                'adj_pair_stoffen', 'adj_pair_gcramp_manual']
               # Det evt met -n/-en [man dat/acc ev]
det_pair_fns = ['det_pair_sg']
               # N [zij ev] (!!!) voornamelijk als benadering van [man ev]
nounmasc_fns = ['nouns_desg_spelling', 'nouns_desg_gcramp', 'nouns_desg_manual']
               # N [zij ev] met +s als benadering van [man gen ev]
               # en mapping naar s-loos 
noungenmasc_pair_fns = ['noun_pairs_gendesg_gcramp', 'noun_pairs_gendesg_spelling',
                        'noun_pairs_gendesg_manual']
               # N [onz ev]
nounneut_fns = ['nouns_hetsg_spelling', 'nouns_hetsg_gcramp']
               # N [onz ev] met +s/+'s als benadering van [onz gen ev]
               # en mapping naar s-loos
noungenneut_pair_fns = ['noun_pairs_genhetsg_gcramp', 'noun_pairs_genhetsg_spelling',
                        'noun_pairs_genhetsg_manual']
               # N [pl]
nounplur_fns = ['nouns_pl_spelling', 'nouns_pl_gcramp']

               # handmatige selecties uit veelvoorkomende strings rond Det Adj N 
preposition_fns = ['prepositions']
adverb_fns = ['adverbs']

noungentemp_fns = ['noungentemp_manual']


##
## Hulpfuncties
##

def strformat_mapping(fr,to):
    assert ' ' not in to
    if fr == to:
        return fr
    elif fr == '':
        if to in '[]':
            return to
        else:
            return '[ @phantom %s ]' % (to,)
    elif to == '':
        assert ' ' not in fr
        return '[ @skip %s ]' % (fr,)
    elif ' ' in fr:
        return '[ @mwu_alt %s %s ]' % (to, fr)
    else:
        return '[ @alt %s %s ]' % (to, fr)
    
def load_maplist(fns):
    maplist = {}
    for fn in fns:
        with open(fn) as f:
            maplist.update(l.split() for l in f)
    return maplist

            
def load_formlist(fns):
    formlist = set()
    for fn in fns:
        with open(fn) as f:
            formlist.update(l.strip() for l in f)
    return formlist


def copycase(source,target):
    if source.islower():
        return target.lower()
    elif source.isupper():
        return target.upper()
    elif source and source == source.capitalize():
        return '~'.join(alternative.capitalize()
                        for alternative in target.split('~'))
    else:
        return target


##
## Patronen
##

""" seq_XXX

Arguments
 ws  List of strings, representation of whole sentence.
 i   Integer, index into ws, how far the sentence has been processed.

Returns:
 *Either* None if the pattern handled in the function is not found
 *or* a list of pairs of strings, each first element the token as-is, each
 second element the token that should replace it.

 Special cases: mwu_alts are captured by space-separated multiple tokens in
 first elements, phantoms by empty first elements, skips by empty
 second elements. Spaces may not occur in second elements.
"""


def seq_DAN(ws, i):
    # Det(+n) Adj+n N
    if ( len(ws) >= i+3
         and ws[i] in det and ws[i+1] in adj and ws[i+2] in noun
        ):
        return [(ws[i], det[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], ws[i+2])]

    
def seq_DdatplurAN(ws, i):
    # Det(+n) Adj+n N
    if ( len(ws) >= i+3
         and ws[i] in detdatplur and ws[i+1] in adj and ws[i+2] in nounplur
         #
         and not ws[i+1].lower() in ['eenen', 'éénen', 'waren'] # "waren" te veel FP
         and not ws[i+2].lower() in ['waren']
         ):
        return [(ws[i], detdatplur[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], ws[i+2])]


def seq_DgenmascAN(ws, i):
    # Det+s Adj N+s
    if ( len(ws) >= i+3
         and ws[i] in detgenmasc and ws[i+1] in adj and ws[i+2] in noungenmasc
         # uitgezonderd
         and ws[i+2] not in ['huize']
         and ws[i:i+3] not in [['des','anderen','daags'],
                               ['Des','anderen','daags'],
                               ['Des', 'anders', 'daghes'],
                               ]
        ):
        if  (i > 0 and ws[i-1] in prep):
            # des gemenen mans (ijdelheid)
            # veel waarschijnlijker na prepositie
            return [(' '.join(ws[i:i+3]), 'zijn')]
        else:
            # (ijdelheid) des gemenen mans
            # anders meestvoorkomende geval
            return [('', 'van'),
                    (ws[i], detgenmasc[ws[i]]),
                    (ws[i+1], adj[ws[i+1]]),
                    (ws[i+2], noungenmasc[ws[i+2]])]

        
def seq_DgenneutAN(ws, i):
    # Det+s Adj N+s
    if ( len(ws) >= i+3
         and ws[i] in detgenneut and ws[i+1] in adj and ws[i+2] in noungenneut
        ):
        if i > 0 and ws[i-1] in prep:
            return [(' '.join(ws[i:i+3]), 'zijn')]
        else:
            return [('', 'van'),
                    (ws[i], detgenneut[ws[i]]),
                    (ws[i+1], adj[ws[i+1]]),
                    (ws[i+2], noungenneut[ws[i+2]])]

    
def seq_DgenneutN(ws, i):
    # Det+s N+s
    #    FPs:
    #    zag hij [zijns zoons] boedel in November 1656 devolveeren
    #    zijns gelijken (gaat wellicht goed?)
    #
    if ( len(ws) >= i+2
         and ws[i] in detgenneut and ws[i+1] in noungenneut
         # uitgezonderd
         and not ( ws[i].lower() in ['haars', 'huns', 'mijns', 'onzes', 'uws', 'zijns',]
                   and ws[i+1].lower() in ['inziens', 'dunkens', 'menens', 'wetens',
                                           'oordeels','achtens','bedughtens','bedunckens',
                                           'bedunkens','insziens','inzens','inzins', 'weetens',
                                           'zinziens'] )
         and not ( ws[i].lower() == 'diens'
                   and ( ws[i+1] in nounplur
                         or ws[i+1] in noun
                         or ws[i+1] in nounneut ) )
         and not ( ws[i].lower() == 'des'              # levert geen probleem op
                   and ws[i+1] == 'huizes' )
        ):
        if i > 0 and ws[i-1] in prep:
            return [(' '.join(ws[i:i+2]), 'zijn')]
        else:
            return [('', 'van'),
                    (ws[i], detgenneut[ws[i]]),
                    (ws[i+1], noungenneut[ws[i+1]])]

        
def seq_DgenmascN(ws, i):
    # Det+s N+s
    #    FPs:
    #    "zag hij [zijns zoons] boedel in November 1656 devolveeren"
    #    "zijns gelijken" (gaat wellicht goed?)
    #    'des tijds' soms "destijds", maar ook vaak nog "van de tijd"
    #
    if ( len(ws) >= i+2
         and ws[i] in detgenmasc and ws[i+1] in noungenmasc
         # uitgezonderd
         and not ws[i+1].lower() == 'huizes'             # liever bij genneut
         and not ( ws[i].lower() in ['haars', 'huns', 'mijns', 'onzes', 'uws', 'zijns',]
                   and ws[i+1].lower() in ['ondanks'] )
         and not ( ws[i].lower() == 'des'
                   and ws[i+1].lower() in ['noods'] )
         and not ( ws[i].lower() == 'des'
                   and ws[i+1].lower() in noungentemp )
         and not ( ws[i].lower() == 'diens'
                   and ( ws[i+1] in nounplur
                         or ws[i+1] in noun
                         or ws[i+1] in nounneut ) ) 
        ):
        if i > 0 and ws[i-1] in prep:
            # (Met) [ @mwu_alt zijn des leeuweriks ] (jolijt) 
            return [(' '.join(ws[i:i+2]), 'zijn')]
        else:
            return [('', 'van'),
                    (ws[i], detgenmasc[ws[i]]),
                    (ws[i+1], noungenmasc[ws[i+1]])]


def seq_sN(ws, i):
    # 's N+s
    if ( len(ws) >= i+2\
         and ws[i] == "'s" and (ws[i+1] in noungenneut or ws[i+1] in noungenmasc)
         and ws[i+1].lower() not in noungentemp
        ):
        return [(' '.join(ws[i:i+2]), 'zijn')]

    
def seq_DACAN(ws, i):
    # Det Adj , Adj N
    # Det Adj en Adj N
    # Det Adj of Adj N
    if ( len(ws) >= i+5
         and ws[i] in det and ws[i+1] in adj
             and ws[i+2] in (',','en','of')
                 and ws[i+3] in adj and ws[i+4] in noun
        ):
        return [(ws[i],   det[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], ws[i+2]),
                (ws[i+3], adj[ws[i+3]]),
                (ws[i+4], ws[i+4])]

    
def seq_DAACAN(ws, i):
    # Det Adj Adj en Adj N
    # Det Adj Adj of Adj N
    if ( len(ws) >= i+6
         and ws[i] in det and ws[i+1] in adj and ws[i+2] in adj
             and ws[i+3] in ('en','of')
                 and ws[i+4] in adj and ws[i+5] in noun
        ):
        return [(ws[i],   det[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], adj[ws[i+2]]),
                (ws[i+3], ws[i+3]),
                (ws[i+4], adj[ws[i+4]]),
                (ws[i+5], ws[i+5])]

    
def seq_DAAN(ws, i):
    # Det Adj Adj N
    if ( len(ws) >= i+4
         and ws[i] in det and ws[i+1] in adj and ws[i+2] in adj and ws[i+3] in noun ):
        return [(ws[i],   det[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], adj[ws[i+2]]),
                (ws[i+3], ws[i+3])]

    
def seq_DAAAN(ws, i):
    # Det Adj Adj Adj N
    if ( len(ws) >= i+5
         and ws[i] in det and ws[i+1] in adj 
             and ws[i+2] in adj and ws[i+3] in adj 
                 and ws[i+4] in noun ):
        return [(ws[i],   det[ws[i]]),
                (ws[i+1], adj[ws[i+1]]),
                (ws[i+2], adj[ws[i+2]]),
                (ws[i+3], adj[ws[i+3]]),
                (ws[i+4], ws[i+4])]

    
def seq_PAN(ws, i):
    # Prep Adj N
    # NB: lookbehind, prep (ws[i-1]) niet deel van output
    #  "te" niet als prep/adv -- teveel FPs (te als inf. mark)
    #  "als" niet als prep -- teveel FPs ("als waren zij")
    if ( i > 0 and len(ws) >= i+2
         and ws[i-1] in prep and ws[i] in adj and ws[i+1] in noun
        ):
        if ws[i].lower() in ('eenen','enen','éénen','énen',):
            # eig niet (Prep) Adj N, maar (Prep) Det N
            return [(ws[i],   det[ws[i]]),   
                    (ws[i+1], ws[i+1])]
        else:
            return [(ws[i],   adj[ws[i]]),
                    (ws[i+1], ws[i+1])]

        
def seq_DAdvAN(ws, i):
    # Det Adv Adj N
    #  "te" niet als prep/adv -- teveel FPs (te als inf. mark)
    if ( len(ws) >= i+4
         and ws[i] in det and ws[i+1] in adv and ws[i+2] in adj and ws[i+3] in noun
        ):
        return [(ws[i],   det[ws[i]]),
                (ws[i+1], ws[i+1]),
                (ws[i+2], adj[ws[i+2]]),
                (ws[i+3], ws[i+3])]

    
def seq_PAdvAN(ws, i):
    # Prep Adv Adj N
    # (zie seq_PAN)
    if ( i > 0 and len(ws) >= i+3
         and ws[i-1] in prep and ws[i] in adv and ws[i+1] in adj and ws[i+2] in noun
        ):
        if ws[i].lower() in ('eenen','enen','éénen','énen',):
            # eig niet (Prep) Adv Adj N, maar (Prep) Adv Det N
            return [(ws[i],   ws[i]),
                    (ws[i+1], det[ws[i+1]]),   
                    (ws[i+2], ws[i+2])]
        else:
            return [(ws[i],   ws[i]),
                    (ws[i+1], adj[ws[i+1]]),
                    (ws[i+2], ws[i+2])]



def seq_te_DN(ws, i):
    # te zijner tijd
    # te onzer beschikking
    # te harer gunste etc etc etc
    if ( len(ws) >= i+3
         and ws[i].lower() in ['te', 'ter','ten'] # laatste twee eig niet gramm?
         and ws[i+1] in te_det
         # ws[i+2] ...
        ):
        match tuple(map(str.lower, ws[i+1:i+3])):
            case ('zijner', 'tijd')\
               | ('zijner', 'tijt')\
               | ('zijner', 'tyd')\
               | ('zijner', 'tyt')\
               | ('zyner',  'tijd')\
               | ('zyner',  'tijt')\
               | ('zyner',  'tyd')\
               | ('zyner',  'tyt'):
                return [(ws[i],   copycase(ws[i],'te')),
                        (ws[i+1], 'zijner'),
                        (ws[i+2], 'tijd')]
            case (_, 'plaats')\
               | (_, 'plaatse'):
                return [(ws[i], copycase(ws[i],'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'plaats')]
            case (_, 'aanbeveling')\
               | (_, 'aanmoediging')\
               | (_, 'aanprijzing')\
               | (_, 'beantwoording')\
               | (_, 'bediening')\
               | (_, 'bekering')\
               | (_, 'beoordeeling')\
               | (_, 'beoordeling')\
               | (_, 'beschaming')\
               | (_, 'bescherming')\
               | (_, 'beschikking')\
               | (_, 'beschutting')\
               | (_, 'beslissing')\
               | (_, 'bespotting')\
               | (_, 'bestemming')\
               | (_, 'bestrijding')\
               | (_, 'beveiliging')\
               | (_, 'bevestiging')\
               | (_, 'bevordering')\
               | (_, 'bevrediging')\
               | (_, 'bevrijding')\
               | (_, 'bewaking')\
               | (_, 'geruststelling')\
               | (_, 'handhaving')\
               | (_, 'herdenking')\
               | (_, 'herinnering')\
               | (_, 'inlichting')\
               | (_, 'instandhouding')\
               | (_, 'kennis')\
               | (_, 'leering')\
               | (_, 'ontlasting')\
               | (_, 'ontwikkeling')\
               | (_, 'opbeuring')\
               | (_, 'opwekking')\
               | (_, 'overtuiging')\
               | (_, 'rechtvaardiging')\
               | (_, 'redding')\
               | (_, 'regtvaardiging')\
               | (_, 'uitvoering')\
               | (_, 'verantwoording')\
               | (_, 'verdediging')\
               | (_, 'verheerlijking')\
               | (_, 'verjaring')\
               | (_, 'verklaring')\
               | (_, 'verlossing')\
               | (_, 'verontschuldiging')\
               | (_, 'verschoning')\
               | (_, 'verschooning')\
               | (_, 'verstrooijing')\
               | (_, 'vertroosting')\
               | (_, 'vervanging')\
               | (_, 'vervulling')\
               | (_, 'verwelkoming')\
               | (_, 'verwezenlijking')\
               | (_, 'verzorging')\
               | (_, 'voldoening')\
               | (_, 'volmaking')\
               | (_, 'voltooiing')\
               | (_, 'voorlichting')\
               | (_, 'vorming')\
               | (_, 'zelfverdediging'):
                return [(ws[i],   copycase(ws[i], 'tot')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], ws[i+2])]
            case (_, 'zake'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'zaak')]
            case (_, 'tijd')\
               | (_, 'tyd')\
               | (_, 'tijde')\
               | (_, 'tyde'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'tijd')] 
            case ('dier',  'gelegenheid')\
               | ('dezer', 'gelegenheid'):
                return [(ws[i],   copycase(ws[i], 'bij')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], ws[i+2])] 
            case (_, 'eer')\
               | (_, 'eere')\
               | (_, 'ere'):
                return [(ws[i],   copycase(ws[i], 'tot')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'eer')] 
            case (_, 'stede'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'stad')] 
            case (_, 'gedachtenis')\
               | (_, 'nagedachtenis'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], ws[i+2])] 
            case (_, 'keuze'):
                return [(ws[i],   copycase(ws[i], 'naar')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'keus')]
            case ('wier',   'behoeve')\
               | ('welker', 'behoeve'):
                return [(' '.join(ws[i:i+3]), copycase(ws[i], 'waarvoor'))]
            case (_, 'gunste'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'gunst')] # dwz voordeel                         
            case ('wier', 'aanzien'):
                return [(' '.join(ws[i:i+3]), copycase(ws[i], 'waarbij'))]
            case ('dier',  'dage')\
               | ('dezer', 'dage'):
                return [(ws[i],   ''),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'dag')]                        
            case ('dier',  'dagen')\
               | ('dezer', 'dagen'):
                return [(ws[i],   ''),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'dagen')]                        
            case (_, 'ure'):
                return [(ws[i],   'op'),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'tijd')] # anders moeten we dit/dat etc aanpassen... 
            case (_, 'kennisse'):
                return [(ws[i],   copycase(ws[i], 'tot')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'kennis')]
            case (_, 'zijde')\
               | (_, 'rechterzijde')\
               | (_, 'linkerzijde')\
               | (_, 'wederzijde')\
               | (_, 'weerzijde'):
                return [(ws[i],   copycase(ws[i], 'aan')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], ws[i+2])]
            case (_, 'intentie')\
               | (_, 'ontvangst'):
                return [(ws[i],   copycase(ws[i], 'voor')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], ws[i+2])]
            case (_, 'wille'):
                return [(ws[i],   copycase(ws[i], 'voor')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'wil')]
            case (_, 'opzichte'):
                return [(ws[i],   copycase(ws[i], 'in')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'opzicht')] # eig ook onz, maar gaat toevallig goed in GCRAMP?
            case (_, 'dispositie'):
                return [(ws[i],   copycase(ws[i], 'tot')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'beschikking')]
            case (_, 'stond')\
               | (_, 'stonde'):
                return [(ws[i],   copycase(ws[i], 'op')),
                        (ws[i+1], te_det[ws[i+1]]),
                        (ws[i+2], 'tijd')] # anders moeten we dit/dat etc aanpassen...
            
        # spoelen over 'te' en D zodat seq_Dposgfdfgpl de D niet oppikt 
        return [(ws[i], ws[i]), (ws[i+1],ws[i+1])]
            

def seq_Dposgfdfgpl(ws, i):
    if ( ws[i] in detposgfdfgpl
        ):
        if ( len(ws) >= i+2
               and ws[i+1].lower() in ["excellentie's", 'majesteits', "majesteit's"]
              ):
            return [(' '.join(ws[i:i+2]), 'haar')]
        elif ( len(ws) >= i+2
               and ws[i+1].lower() in ["excellentie", 'excellenties', 'majesteit', 'majesteiten']
              ):
            # een houtje-touwtje-oplossing omdat phantom geen alternatieven toelaat
            # OK: waarvan ik de eer heb [ @alt ~van~aan Uwer ] [ @phantom uw ] Excellentie , hiermede ,
            #     de plegtigste verzekering te geven
            # maar: als het [ @alt ~aan~van Harer] [ @phantom haar ] Majesteit behaagt
            #       ( want: behagen (*aan) iem )
            return [(ws[i],   '~van~aan'),
                    ('',      detposgfdfgpl[ws[i].lower()]),
                    (ws[i+1], ws[i+1])]
        elif ( len(ws) >= i+2
               and ws[i+1].lower() in ['waardig', 'waardige', 'waardigen',
                                       'onwaardig', 'onwaardige', 'onwaardigen']
              ):
            return [('','['),
                    (ws[i],   detposgfdfgpl_pers[ws[i]]),
                    (ws[i+1], ws[i+1][:-1] if ws[i+1][-1].lower()=='n' else ws[i+1]),
                    ('',']')]        
        elif ( len(ws) >= i+3
               and ws[i+1] in adv
               and ws[i+2].lower() in ['waardig', 'waardige', 'waardigen',
                                       'onwaardig', 'onwaardige', 'onwaardigen']
              ):
            return [('','['),
                    (ws[i],   detposgfdfgpl_pers[ws[i]]),
                    (ws[i+1], ws[i+1]),
                    (ws[i+2], ws[i+2][:-1] if ws[i+2][-1].lower()=='n' else ws[i+2]),
                    ('',']')]        
        elif ( i > 0
               and ( ws[i-1] in prep or ws[i-1] == 'als' )
              ):
            # die Christus kruis teekent op [ @alt_mwu haar Uwer kinderen ] aangezicht
            # meer dan helft vd (weinige) gevallen prenom gen
            return [(' '.join(ws[i:i+2]), 'haar')] 
        else:
            if detposgfdfgpl[ws[i]] in ['onze','uw','hun']:
                # mv ook als partitief: 
                return [('', 'van'),
                        (ws[i], '~%s~%s' % (detposgfdfgpl[ws[i]].removeprefix('~'),
                                                detposgfdfgpl_pers[ws[i]].removeprefix('~')))]
            else:
                return [('', 'van'),
                        (ws[i], detposgfdfgpl[ws[i]])]
    

            
##
## Main
##

def main():
    # data / woordsoorten / categorien / mappings
    global adj, det, noungenneut, noungenmasc 
    adj = load_maplist(adj_pair_fns) 
    det = load_maplist(det_pair_fns)
    noungenneut = load_maplist(noungenneut_pair_fns)
    noungenmasc = load_maplist(noungenmasc_pair_fns)

    global noun, prep, adv, nounneut, noungentemp, nounplur
    noun = load_formlist(nounmasc_fns)
    prep = load_formlist(preposition_fns)
    adv = load_formlist(adverb_fns)
    noungentemp = load_formlist(noungentemp_fns)
    nounneut = load_formlist(nounneut_fns)
    nounplur = load_formlist(nounplur_fns)

    global detgenneut, detgenmasc, detdatplur, detposgfdfgpl, detposgfdfgpl_pers, te_det
    detgenneut = {'des': 'het',     'dezes': 'dit',   'diens': '~dat~diens',
                  'haars': 'haar',  'heurs': 'haar',  'hunnes': 'hun',
                  'huns': 'hun',    'mijns': 'mijn',  'myns': 'mijn',
                  'onses': 'ons',   'onzes': 'ons',   'uwes': 'uw',
                  'uws': 'uw',      'zijns': 'zijn',  'zyns': 'zijn',
                  }

    detgenmasc = detgenneut | {'des': 'de',      'dezes': 'deze',  'diens': '~die~diens',
                               'onses': 'onze',  'onzes': 'onze',
                               }

    detgenneut.update({copycase('Aaa',k): copycase('Aaa',v)
                       for k, v in detgenneut.items()})

    detgenmasc.update({copycase('Aaa',k): copycase('Aaa',v)
                       for k, v in detgenmasc.items()})
    
    detdatplur = {'allen': 'alle',           "d'n": 'de',          'den': 'de',
                  'denzelfden': 'dezelfde',  'dezen': 'deze',      'dien': 'die',
                  'eenigen': 'enige',        'eenighen': 'enige',  'enigen': 'enige',
                  'enkelen': 'enkele',       'geenen': 'geen',     'haaren': 'haar',
                  'haren': 'haar',           'heuren': 'haar',     'hunnen': 'hun',
                  'menigen': 'menige',       'mijnen': 'mijn',     'mynen': 'mijn',
                  'onsen': 'onze',           'onzen': 'onze',      'uwen': 'uw',
                  'welken': 'welke',         'zijnen': 'zijn',     'zulken': 'zulke',
                  'zynen': 'zijn'
                  }
    detdatplur.update({copycase('Aaa',k): copycase('Aaa',v)
                       for k, v in detdatplur.items()})


    # bezittelijke vormen voor gevallen van gen det
    detposgfdfgpl = {'mijner': 'mijn',  'myner': 'mijn',
                     'uwer': 'uw',
                     'zijner': 'zijn',  'zyner': 'zijn',
                     'harer': 'haar',   'haarer': 'haar',
                     'onzer': 'onze',   'onser': 'onze',
                     'hunner': 'hun',  
                     }
    detposgfdfgpl.update({copycase('Aaa',k): copycase('Aaa',v)
                          for k, v in detposgfdfgpl.items()})

    # persoonlijke vormen (obj) voor partitief (mv) en voor bv "mijner waardig" → "mij waardig"
    detposgfdfgpl_pers = {'mijner': 'mij',       'myner': 'mij',
                          'uwer': 'u',
                          'zijner': 'hem',       'zyner': 'hem',
                          'harer': 'haar',       'haarer': 'haar',
                          'onzer': 'ons',        'onser': 'ons',
                          'hunner': '~hen~hun',  
                     }
    detposgfdfgpl_pers.update({copycase('Aaa',k): copycase('Aaa',v)
                              for k, v in detposgfdfgpl_pers.items()})

    assert detposgfdfgpl.keys() == detposgfdfgpl_pers.keys()
    
    te_det = detposgfdfgpl | {'dezer': 'deze',            'dier': 'die',
                              'diens': 'diens',
                              'welker': '~welke~welker',  'wier': 'wier',    
                              'wiens': 'wiens',  
                              }
    te_det.update({copycase('Aaa',k): copycase('Aaa',v)
                   for k, v in te_det.items()})
    
    
    # process loop
    for line in sys.stdin:
        *key, notkey = line.split('|',1)
        words_in = notkey.split()
        words_out = []

        i = 0
        while i < len(words_in):
            mapping = (
                seq_DAACAN(words_in, i)
                or seq_DAAAN(words_in, i)
                or seq_DACAN(words_in, i)
                or seq_DAAN(words_in, i)
                or seq_DAdvAN(words_in, i)
                or seq_DAN(words_in, i)
                or seq_DdatplurAN(words_in, i)
                or seq_DgenmascAN(words_in, i)
                or seq_DgenneutAN(words_in, i)
                or seq_DgenmascN(words_in, i)
                or seq_DgenneutN(words_in, i)
                or seq_sN(words_in, i)
                or seq_PAdvAN(words_in, i)
                or seq_PAN(words_in, i)
                or seq_te_DN(words_in, i)
                or seq_Dposgfdfgpl(words_in, i)
            )
            
            if mapping:
                words_out.extend(strformat_mapping(fr,to) for fr,to in mapping)
                i += sum(len(fr.split()) for fr,_ in mapping)
            else:
                words_out.append(words_in[i])
                i += 1
                
        print('|'.join((*key,' '.join(words_out))))

if __name__ == '__main__':
    main()





