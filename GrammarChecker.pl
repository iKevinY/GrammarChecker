% GrammarChecker.pl
/* Some sentences

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


    

*/


% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

% Try:
%?- noun_phrase([a,tall,student],T1,I1).
%?- noun_phrase([a,math,course],T2,I2).
%?- noun_phrase([a,tall,student,enrolled,in,a,math,course],T3,I3).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det(T,T,_).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp([that|T0],T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp(T,T,_).

% DICTIONARY

% adj(T0,T1,Ind) is true if T0-T1 is an adjective that is true of Ind
adj([computer, science | T],T,Ind) :- dept(Ind,comp_sci).
adj([math | T],T,Ind) :- dept(Ind,math).
adj([female | T],T,Ind) :- female(Ind).
adj([male | T],T,Ind) :- male(Ind).
adj([tall | T],T,Ind) :- tall(Ind).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- course(Ind).
noun([student | T],T,Ind) :- student(Ind).
noun([building | T],T,Ind) :- building(Ind).
% The following are for proper nouns:
noun([Ind | T],T,Ind) :- course(Ind).
noun([Ind | T],T,Ind) :- student(Ind).


% reln(T0,T1,Subject,Object) is true if T0-T1 is a relation
%   on individuals I1 and I2
reln([enrolled, in | T],T,Subject,Object) :- enrolled_in(Subject,Object).
reln([passed | T],T,Subject,Object) :- passed(Subject,Object).

% Some Example Queries
% ask noun_phrase([a,computer,science,course],R,Ind,[],C).
% ask noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],R,Ind,[],C).

% question(Question,QR,Ind) is true if Question-QR is true of Ind
question([is | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
question([who,is | T0],T1,Ind) :-
    mp(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    noun_phrase(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([what | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
% The following has "is" betten the noun_phrase and the mp:
question([what | T0],T2,Ind) :-
    noun_phrase(T0,[is|T1],Ind),
    mp(T1,T2,Ind).


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).