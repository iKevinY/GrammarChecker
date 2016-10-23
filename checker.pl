% GrammarChecker.pl
/*

-------------- One concret sentences example ---------------------------

1. XX eat food:

Plural:
	We/They/People/Dogs/A group of students

	(same as singular):
	ate/didn't eat/will(not) eat/would(not) eat/can(not) eat/
	could(not) eat/may(not) eat/might(not) eat/
	shall(not) eat/should(not) eat

	(different from singular):
	(don't) eat/were (not) eating/are (not)eating/
	are (not) going to eat/are (not) gonna eat/
	were(not/about) to eat/are(not/about) to eat/
	have (not) eaten/have (not)been eating

First Person Singular:

	I

	Everything is the same except for:
	am(not) eating/am going to eat/am gonna eat/am to eat/was(not) eating

Third Person Singular:

	She/He/It/A person/Nancy/My dad
	eats/ate/will eat/would eat/was eating/is eating/is going to eat/is gonna eat/
	food.

	(different from plural):
	is(not) eating/is(not) to eat/is(not) going to eat/is(not) gonna eat/
	was(not/about) to eat/was(not) going to eat/was(not) gonna eat/was(not) eating
	has(not) eaten/has(not) been eating/

	(some are the same as plural, see above)

------------------------ Other more complicated examples -------------

It doesn't go through.
I like eating.
I like eating chocolates.
I like eating that kind of chocolates.
I like eating chocolates that my mom bought me.

-----------------------------other linguistic facts -----------------------------------

noun:
pronoun: we(us), I(me), they(them), she(her)....
proper nouns: Kevin, Kevin and Victoria, UBC, Google.....
things: pencil(s), water, a school of fish, brother, her heart...

verb:
auxilary: do, has, shall, can, would....
regular: sing, tends to, played with....

For singular, countable nouns, you have to have determinaters

The table is tall.  ----> yes
Table is tall.  ------> no
Let's find a table. -----> yes
Let's find table. ----> no

*/

:- discontiguous aux_verb/4.


% sentence[]
sentence(T0, T2, Person):-
	noun_phrase(T0, T1, Person),
	verb_phrase(T1, T2, Person).



% A noun phrase is an optional determiner followed by
% an optional adjective followed by a simple noun
noun_phrase(T0,T4, Person) :-
	det(T0,T1),
	adjectives(T1, T2),
    noun(T2,T4, Person).

% Alternatively, a proper noun is a noun phrase
noun_phrase(T0, T1, Person) :-
	proper_noun(T0, T1, Person).

% Also, a pronoun is a noun phrase
noun_phrase(T0, T1, Person) :-
	pronoun(T0, T1, Person).



% Verb phrases could either be just a verb...
verb_phrase(T0,T1, Person):-
	verb(T0,T1, Person).

% ...or a verb followed by a noun phrase.
verb_phrase(T0,T2, Person):-
	verb(T0,T1, Person),
	noun_phrase(T1,T2, _).


% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T).
det([a | T],T).
det([an| T], T).
det(T,T,_).

% preposition phrase (keep it simple for now):
% e.g. I want the bag [in the fridge]. She didn't bring me [with her].
prep_phrase(T, T).
prep_phrase(T0, T2):-
	prep(T0, T1),
	noun(T1, T2).



% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2) :-
    adj(T0,T1),
    adjectives(T1,T2).
adjectives(T,T).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing

mp(T0,T1, _) :-
    prep_phrase(T0,T1).
mp([that|T0],T1,Person) :-
    verb_phrase(T0,T1,Person).
mp([that|T0],T1, _) :-
    sentence(T0,T1, _).
mp([who|T0],T1,Person) :-
    verb_phrase(T0,T1,Person).
mp([which|T0],T1,Person) :-
    verb_phrase(T0,T1,Person).
mp([whose|T0],T2,Person) :-
	noun(T0,T1, Person),
    verb_phrase(T1,T2,Person).
mp(T,T,_).


noun(T0, T1, Person):- pronoun(T0, T1, Person).
noun(T0, T1, Person):- proper_noun(T0,T1, Person).
noun(T0, T1, Person):- thing(T0,T1, Person).


verb(T0, T1, Person):- reg_verb(T0, T1, Person, _).
verb(T0, T2, Person):-
	aux_verb(T0, T1, Person, Tense),
	reg_verb(T1, T2, Person, Tense).

%Dictionary

% ----------------- auxilary verbs -------------


% pre: present -> so that the verb should be regular form: draw

aux_verb([can| T], T, _, pre).
aux_verb([cannot| T], T, _, pre).
aux_verb([could| T], T, _, pre).
aux_verb([could, not| T], T, _, pre).
aux_verb([will| T], T, _, pre).
aux_verb([will, not| T], T, _, pre).
aux_verb([would| T], T, _, pre).
aux_verb([would, not| T], T, _, pre).
aux_verb([may| T], T, _, pre).
aux_verb([may, not| T], T, _, pre).
aux_verb([might| T], T, _, pre).
aux_verb([might, not| T], T, _, pre).
aux_verb([do| T], T, P, pre):- P = i; P = p. % I do think it's nice.
aux_verb([do, not| T], T, P, pre):- P = i; P = p.
aux_verb([did| T], T, _, pre).
aux_verb([did, not| T], T, _, pre).
aux_verb([shall| T], T, _, pre).
aux_verb([shall, not| T], T, _, pre).
aux_verb([should| T], T, _, pre).
aux_verb([should, not| T], T, _, pre).
aux_verb([ought, to| T], T, _, pre).
aux_verb([ought, not, to| T], T, _, pre).
aux_verb([must| T], T, _, pre).
aux_verb([must, not| T], T, _, pre).
aux_verb([dare, to| T], T, _, pre).
aux_verb([dare, not, to| T], T, _, pre).

aux_verb([are, to| T], T, p, pre).
aux_verb([are, not, to| T], T, p, pre).
aux_verb([is, to| T], T, s, pre).
aux_verb([is, not, to| T], T, p, pre).
aux_verb([were, to| T], T, p, pre).
aux_verb([were, not, to| T], T, p, pre).
aux_verb([was, to| T], T, P, pre):- P = i; P = s.
aux_verb([was, not, to| T], T, P, pre):- P = i; P = s.

aux_verb([am, going, to| T], T, i, pre).
aux_verb([am, gonna| T], T, i, pre).
aux_verb([am, not, going, to| T], T, i, pre).
aux_verb([am, not, gonna| T], T, i, pre).
aux_verb([are, going, to| T], T, p, pre).
aux_verb([are, gonna| T], T, p, pre).
aux_verb([are, not, going, to| T], T, p, pre).
aux_verb([are, not, gonna| T], T, p, pre).
aux_verb([is, going, to| T], T, s, pre).
aux_verb([is, gonna| T], T, s, pre).
aux_verb([is, not, going, to| T], T, s, pre).
aux_verb([is, not, gonna| T], T, s, pre).


aux_verb([was, going, to| T], T, i, pre).
aux_verb([was, gonna| T], T, i, pre).
aux_verb([was, not, going, to| T], T, i, pre).
aux_verb([was, not, gonna| T], T, i, pre).
aux_verb([were, going, to| T], T, p, pre).
aux_verb([were, gonna| T], T, p, pre).
aux_verb([were, not, going, to| T], T, p, pre).
aux_verb([were, not, gonna| T], T, p, pre).
aux_verb([was, going, to| T], T, s, pre).
aux_verb([was, gonna| T], T, s, pre).
aux_verb([was, not, going, to| T], T, s, pre).
aux_verb([was, not, gonna| T], T, s, pre).


% cont: continuous form -> drawing
% pp: past participle -> used for passive voice or perfect tense

% I was attracted to the music.
% The music has attracted the crowd.

aux_verb([are| T], T, p, Ten):- Ten = cont; Ten = pp.
aux_verb([are, not| T], T, p, Ten):- Ten = cont; Ten = pp.
aux_verb([are, being| T], T, p, pp).
aux_verb([are, not, being| T], T, p, pp).
aux_verb([is| T], T, s, Ten):- Ten = cont; Ten = pp.
aux_verb([is, not| T], T, s, Ten):- Ten = cont; Ten = pp.
aux_verb([is, being| T], T, s, pp).
aux_verb([is, not, being| T], T, s, pp).
aux_verb([am, being| T], T, i, pp).
aux_verb([am, not, being| T], T, i, pp).
aux_verb([am| T], T, i, Ten):- Ten = cont; Ten = pp.
aux_verb([am, not| T], T, i, Ten):- Ten = cont; Ten = pp.

aux_verb([am, going, to, be| T], T, i, Ten) :- Ten = cont; Ten = pp.
aux_verb([am, gonna, be| T], T, i, Ten) :- Ten = cont; Ten = pp.
aux_verb([am, not, going, to, be| T], T, i, Ten) :- Ten = cont; Ten = pp.
aux_verb([am, not, gonna, be| T], T, i, Ten) :- Ten = cont; Ten = pp.
aux_verb([are, going, to, be| T], T, p, Ten) :- Ten = cont; Ten = pp.
aux_verb([are, gonna, be| T], T, p, Ten) :- Ten = cont; Ten = pp.
aux_verb([are, not, going, to, be| T], T, p, Ten) :- Ten = cont; Ten = pp.
aux_verb([are, not, gonna, be| T], T, p, Ten) :- Ten = cont; Ten = pp.
aux_verb([is, going, to, be| T], T, s, Ten) :- Ten = cont; Ten = pp.
aux_verb([is, gonna, be| T], T, s, Ten) :- Ten = cont; Ten = pp.
aux_verb([is, not, going, to, be| T], T, s, Ten) :- Ten = cont; Ten = pp.
aux_verb([is, not, gonna, be| T], T, s, Ten) :- Ten = cont; Ten = pp.

aux_verb([was, going, to, be| T], T, P, Ten):- (P = i; P= s), (Ten = cont; Ten = pp).
aux_verb([was, gonna, be| T], T, P, Ten):- (P = i; P= s), (Ten = cont; Ten = pp).
aux_verb([was, going, to, be| T], T, P, Ten):- (P = i; P= s), (Ten = cont; Ten = pp).
aux_verb([was, not, going, to, be| T], T, P, Ten):- (P = i; P= s), (Ten = cont; Ten = pp).
aux_verb([were, going, to, be| T], T, p, Ten):- Ten = cont; Ten = pp.
aux_verb([were, not, going, to, be| T], T, p, Ten):- Ten = cont; Ten = pp.
aux_verb([were, gonna, be| T], T, p, Ten):- Ten = cont; Ten = pp.
aux_verb([were, not, gonna, be| T], T, p, Ten):- Ten = cont; Ten = pp.


aux_verb([have| T], T, P, pp):- P = i; P = p.
aux_verb([have, not| T], T, P, pp):- P = i; P = p.
aux_verb([has| T], T, s, pp).
aux_verb([has, not| T], T, s, pp).

aux_verb([have, been| T], T, P, Ten):- (Ten = cont; Ten = pp), (P = i; P = p).
aux_verb([have, not, been| T], T, P, Ten):- (Ten = cont; Ten = pp), (P = i; P = p).
aux_verb([has, been| T], T, s, Ten):- Ten = cont; Ten = pp.
aux_verb([has, not, been| T], T, s, Ten):- Ten = cont; Ten = pp.

aux_verb([had| T], T, _, pp).
aux_verb([had, not| T], T, _, pp).
aux_verb([had, been| T], T, Ten):- Ten is cont; Ten = pp.
aux_verb([had, not, been| T], T, Ten):- Ten is cont; Ten = pp.


aux_verb([could, be| T], T, _, cont).
aux_verb([could, not, be| T], T, _, cont).
aux_verb([may, be| T], T, _, cont).
aux_verb([may, not, be| T], T, _, cont).
aux_verb([might, be| T], T, _, cont).
aux_verb([might, not, be| T], T, _, cont).
aux_verb([must, be| T], T, _, cont).
aux_verb([must, not, be| T], T, _, cont).
aux_verb([ought, to, be| T], T, _, cont).
aux_verb([ought, not, to, be| T], T, _, cont).
aux_verb([would, be| T], T, _, cont).
aux_verb([would, not, be| T], T, _, cont).
aux_verb([will, be| T], T, _, cont).
aux_verb([will, not, be| T], T, _, cont).
aux_verb([shall, be| T], T, _, cont).
aux_verb([shall, not, be| T], T, _, cont).
aux_verb([should, be| T], T, _, cont).
aux_verb([should, not, be| T], T, _, cont).

aux_verb([could, have| T], T, _, pp).
aux_verb([could, not, have| T], T, _, pp).
aux_verb([may, have| T], T, _, pp).
aux_verb([may, not, have| T], T, _, pp).
aux_verb([might, have| T], T, _, pp).
aux_verb([might, not, have| T], T, _, pp).
aux_verb([must, have| T], T, _, pp).
aux_verb([must, not, have| T], T, _, pp).
aux_verb([ought, to, have| T], T, _, pp).
aux_verb([ought, not, to, have| T], T, _, pp).
aux_verb([would, have| T], T, _, pp).
aux_verb([would, not, have| T], T, _, pp).
aux_verb([will, have| T], T, _, pp).
aux_verb([will, not, have| T], T, _, pp).
aux_verb([shall, have| T], T, _, pp).
aux_verb([shall, not, have| T], T, _, pp).
aux_verb([should, have| T], T, _, pp).
aux_verb([should, not, have| T], T, _, pp).

aux_verb([could, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([could, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([may, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([may, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([might, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([might, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([must, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([must, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([ought, to, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([ought, not, to, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([would, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([would, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([will, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([will, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([should, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.
aux_verb([should, not, have, been| T], T, _, Ten):- Ten = cont; Ten = pp.



% ------------------ regular verbs ----------------------


reg_verb([like | T], T, i).
reg_verb([likes| T], T, s). %s means singular, p means plural.
reg_verb([like| T], T, p).
reg_verb([liked| T], T, P):- P is p; P is s; P is i.

reg_verb([eats| T], T, s).
reg_verb([eat| T], T, p).
reg_verb([ate| T], T, P):- P is p; P is s; P is i.

reg_verb([thinks| T], T, s).
reg_verb([think| T], T, p).
reg_verb([thought| T], T, P):- P is p; P is s; P is i.

reg_verb([drinks| T], T, s).
reg_verb([drink| T], T, p).
reg_verb([drank| T], T, P):- P is p; P is s; P is i.

reg_verb([talks| T], T, s).
reg_verb([talk| T], T, p).
reg_verb([talked| T], T, P):- P is p; P is s; P is i.

reg_verb([sees| T], T, s).
reg_verb([see| T], T, p).
reg_verb([saw| T], T, P):- P is p; P is s; P is i.

reg_verb([has| T], T, s).
reg_verb([have| T], T, p).
reg_verb([had| T], T, P):- P is p; P is s; P is i.

reg_verb([walks| T], T, s).
reg_verb([walk| T], T, p).
reg_verb([walked| T], T, P):- P is p; P is s; P is i.

reg_verb([puts| T], T, s).
reg_verb([put| T], T, p).
reg_verb([put| T], T, P):- P is p; P is s; P is i.

reg_verb([gets| T], T, s).
reg_verb([get| T], T, p).
reg_verb([got| T], T, P):- P is p; P is s; P is i.

reg_verb([lives| T], T, s).
reg_verb([live| T], T, p).
reg_verb([lived| T], T, P):- P is p; P is s; P is i.

reg_verb([takes| T], T, s).
reg_verb([take| T], T, p).
reg_verb([took| T], T, P):- P is p; P is s; P is i.

reg_verb([goes| T], T, s).
reg_verb([go| T], T, p).
reg_verb([went| T], T, P):- P is p; P is s; P is i.

reg_verb([makes| T], T, s).
reg_verb([make| T], T, p).
reg_verb([made| T], T, P):- P is p; P is s; P is i.

reg_verb([says| T], T, s).
reg_verb([say| T], T, p).
reg_verb([said| T], T, P):- P is p; P is s; P is i.

reg_verb([knows| T], T, s).
reg_verb([know| T], T, p).
reg_verb([knew| T], T, P):- P is p; P is s; P is i.

reg_verb([comes| T], T, s).
reg_verb([come| T], T, p).
reg_verb([came| T], T, P):- P is p; P is s; P is i.

reg_verb([gives| T], T, s).
reg_verb([give| T], T, p).
reg_verb([gave| T], T, P):- P is p; P is s; P is i.

reg_verb([comes| T], T, s).
reg_verb([come| T], T, p).
reg_verb([came| T], T, P):- P is p; P is s; P is i.

reg_verb([wants| T], T, s).
reg_verb([want| T], T, p).
reg_verb([wanted| T], T, P):- P is p; P is s; P is i.

reg_verb([uses| T], T, s).
reg_verb([use| T], T, p).
reg_verb([used| T], T, P):- P is p; P is s; P is i.

reg_verb([finds| T], T, s).
reg_verb([find| T], T, p).
reg_verb([found| T], T, P):- P is p; P is s; P is i.

reg_verb([tells| T], T, s).
reg_verb([tell| T], T, p).
reg_verb([told| T], T, P):- P is p; P is s; P is i.

reg_verb([asks| T], T, s).
reg_verb([ask| T], T, p).
reg_verb([asked| T], T, P):- P is p; P is s; P is i.

reg_verb([is| T], T, s).
reg_verb([are| T], T, p).
reg_verb([were| T], T, P):- P is p; P is s; P is i.

reg_verb([works| T], T, s).
reg_verb([work| T], T, p).
reg_verb([worked| T], T, P):- P is p; P is s; P is i.

reg_verb([feels| T], T, s).
reg_verb([feel| T], T, p).
reg_verb([felt| T], T, P):- P is p; P is s; P is i.

reg_verb([tries| T], T, s).
reg_verb([try| T], T, p).
reg_verb([tried| T], T, P):- P is p; P is s; P is i.

reg_verb([calls| T], T, s).
reg_verb([call| T], T, p).
reg_verb([called| T], T, P):- P is p; P is s; P is i.

reg_verb([does| T], T, s).
reg_verb([do| T], T, p).
reg_verb([did| T], T, P):- P is p; P is s; P is i.

reg_verb([needs| T], T, s).
reg_verb([need| T], T, p).
reg_verb([needed| T], T, P):- P is p; P is s; P is i.

reg_verb([becomes| T], T, s).
reg_verb([become| T], T, p).
reg_verb([became| T], T, P):- P is p; P is s; P is i.

reg_verb([means| T], T, s).
reg_verb([mean| T], T, p).
reg_verb([meant| T], T, P):- P is p; P is s; P is i.

reg_verb([keeps| T], T, s).
reg_verb([keep| T], T, p).
reg_verb([kept| T], T, P):- P is p; P is s; P is i.

reg_verb([lets| T], T, s).
reg_verb([let| T], T, P):- P is p; P is s; P is i.

reg_verb([helps| T], T, s).
reg_verb([help| T], T, p).
reg_verb([helped| T], T, P):- P is p; P is s; P is i.

reg_verb([show| T],T, p).
reg_verb([shows| T], T, s).
reg_verb([showed| T], T, P):- P is p; P is s; P is i.

reg_verb([happens| T], T, s).
reg_verb([happen| T], T, p).
reg_verb([happened| T], T, P):- P is p; P is s; P is i.

reg_verb([believes| T], T, s).
reg_verb([believe| T], T, p).
reg_verb([believed| T], T, P):- P is p; P is s; P is i.

reg_verb([brings| T], T, s).
reg_verb([bring| T], T, p).
reg_verb([brought| T], T, P):- P is p; P is s; P is i.

reg_verb([meets| T], T, s).
reg_verb([meet| T], T, p).
reg_verb([met| T], T, P):- P is p; P is s; P is i.

reg_verb([leaves| T], T, s).
reg_verb([leave| T], T, p).
reg_verb([left| T], T, P):- P is p; P is s; P is i.

% ------------ pronouns -----------------------
pronoun([i| T], T, i).
pronoun([we| T], T, p).
pronoun([you| T], T, p).
pronoun([they| T], T, p).
pronoun([he| T], T, s).
pronoun([she| T], T, s).
pronoun([it| T], T, s).

pronoun([us| T], T, p).
pronoun([me| T], T, p).
pronoun([them| T], T, p).
pronoun([her| T], T, s).
pronoun([him| T], T, s).

% -------------- proper nouns --------------

proper_noun([ubc| T], T, s).
proper_noun([google| T], T, s).
proper_noun([facebook| T], T, s).
proper_noun([linkedin| T], T, s).
proper_noun([instagram| T], T, s).
proper_noun([whatsapp| T], T, s).
proper_noun([wechat| T], T, s).
proper_noun([twitter| T], T, s).
proper_noun([youtube| T], T, s).
proper_noun([netflix| T], T, s).

proper_noun([canada| T], T, s).
proper_noun([bc| T], T, s).
proper_noun([vancouver| T], T, s).
proper_noun([usa| T], T, s).
proper_noun([india| T], T, s).
proper_noun([china| T], T, s).
proper_noun([mexico| T], T, s).

proper_noun([victoria| T], T, s).
proper_noun([kevin| T], T, s).
proper_noun([kelvin| T], T, s).
proper_noun([david| T], T, s).
proper_noun([emily| T], T, s).
proper_noun([rui| T], T, s).
proper_noun([alex| T], T, s).
proper_noun([julin| T], T, s).
proper_noun([peter| T], T, s).
proper_noun([anne| T], T, s).
proper_noun([anna| T], T, s).
proper_noun([annie| T], T, s).
proper_noun([rebecca| T], T, s).
proper_noun([ben| T], T, s).
proper_noun([michael| T], T, s).
proper_noun([michelle| T], T, s).
proper_noun([rachael| T], T, s).
proper_noun([lily| T], T, s).
proper_noun([linda| T], T, s).
proper_noun([richard| T], T, s).
proper_noun([john| T], T, s).
proper_noun([james| T], T, s).
proper_noun([eric| T], T, s).
proper_noun([marry| T], T, s).
proper_noun([matthew| T], T, s).
proper_noun([fiona| T], T, s).
proper_noun([crystal| T], T, s).
proper_noun([jessica| T], T, s).
proper_noun([sherry| T], T, s).
proper_noun([grace| T], T, s).
proper_noun([alan| T], T, s).
proper_noun([janet| T], T, s).
proper_noun([jean| T], T, s).

% ---------------------------- thing ----------------
thing([time| T], T, s).
thing([times| T], T, p).

thing([thing| T], T, s).
thing([things| T], T, p).

thing([person| T], T, s).
thing([people| T], T, p).


thing([day| T], T, s).
thing([days| T], T, p).

thing([way| T], T, s).
thing([ways| T], T, p).

thing([man| T], T, s).
thing([men| T], T, p).

thing([guy| T], T, s).
thing([guys| T], T, p).

thing([woman| T], T, s).
thing([women| T], T, p).

thing([life| T], T, s).
thing([lives| T], T, p).

thing([child| T], T, s).
thing([children| T], T, p).

thing([world| T], T, s).

thing([student| T], T, s).
thing([students| T], T, p).

thing([place| T], T, s).
thing([places| T], T, p).

thing([food| T], T, s).

thing([week| T], T, s).
thing([weeks| T], T, p).

thing([school| T], T, s).
thing([schools| T], T, p).

thing([question| T], T, s).
thing([questions| T], T, p).

thing([color| T], T, s).
thing([people| T], T, p).

thing([money| T], T, s).

thing([water| T], T, s).

thing([home| T], T, s).

thing([work| T], T, s).

%---------- prepositions ------------
prep([to| T], T).
prep([with| T], T).
prep([without| T], T).
prep([in| T], T).
prep([inside| T], T).
prep([on| T], T).
prep([by| T], T).
prep([under| T], T).
prep([beside| T], T).
prep([around| T], T).
prep([through| T], T).
prep([above| T], T).
prep([below| T], T).

% --------- adjectives -----------
adj([other| T], T).
adj([new| T], T).
adj([good| T], T).
adj([great| T], T).
adj([high| T], T).
adj([old| T], T).
adj([big| T], T).
adj([small| T], T).
adj([canadian| T], T).
adj([large| T], T).
adj([young| T], T).
adj([different| T], T).
adj([same| T], T).
adj([similar| T], T).
adj([black| T], T).
adj([red| T], T).
adj([white| T], T).
adj([long| T], T).
adj([bad| T], T).
adj([little| T], T).
adj([important| T], T).
adj([only| T], T).
adj([best| T], T).
adj([sure| T], T).
adj([early| T], T).
adj([able| T], T).
adj([human| T], T).
adj([better| T], T).
adj([worse| T], T).
adj([late| T], T).
adj([hard| T], T).
adj([easy| T], T).
adj([free| T], T).
adj([true| T], T).
adj([right| T], T).
adj([available| T], T).
adj([wrong| T], T).
adj([fine| T], T).
adj([okay| T], T).
adj([hot| T], T).
adj([cool| T], T).
adj([cold| T], T).
adj([nice| T], T).
adj([left| T], T).
adj([general| T], T).
adj([simple| T], T).
adj([serious| T], T).
adj([happy| T], T).
adj([sad| T], T).
adj([tall| T], T).
adj([tired| T], T).

/*
To start testing sentences, run the following:
?- q.
*/
q :-
    write("Type a sentence: "),
    readln(Ln),
    maplist(downcase_atom,Ln,Sentence),
    sentence(Sentence, End, _),
    member(End,[[],['?'],['.']]).
