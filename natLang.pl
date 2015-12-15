sentence(S):- noun_phrase(NP), verb_phrase(VP), append(NP, VP, S).
 

noun_phrase(NP):- article(A), noun(N), append(A, N, NP). 

verb_phrase(V):- verb(V).
verb_phrase(VP):- verb(V), noun_phrase(NP), append(V, NP, VP).

% The Lexicon:

article([the]).
article([a]).
article([an]).

noun([grass]).
noun([cow]).
noun([girl]).
noun([boy]).
noun([apple]).
noun([song]).

verb([eats]).
verb([sings]).
verb([chews]).
verb([kicks]).

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).

adverb([slowly]).
adverb([deliberately]).
adverb([merrily]).
adverb([sweetly]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 1a %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% counts the number of longest sentences between conjuncts
count_sentences( Conj, Accum ):-
  sentence_counter( Conj, Accum, 0 ),!.

% splits sentences from a conjunction of sentences. Adds one to an accumulating
% value for each valid sentence. 
sentence_counter([], Accum, Count):-
  Accum = Count.
sentence_counter(Conj, Accum, Count):-
  (splitAtConj(Conj, _, Rest); sentence(Conj)), 
  NewCount is Count+1,
   sentence_counter( Rest, Accum, NewCount).

% splits a conjunction of sentences into a sentence and a conjunction of
% sentences, headed by the previous conjunct. Removes this conjunct and then
% returns the sentence, and the remaining conjunct of sentences. 
splitAtConj(Conj, S, Rest):-
  sentence(S), H = and, append(S, [H|Rest], Conj).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 1b %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Identify the actions (verbs) that an individual actor performed in a 
% conjunction of sentences. 
actions( Actor, Text, As ):-
  listOfSentences( Text, Sentences), verbList(Actor, Sentences, As),!.

% Generates a list of separate sentences from a conjunct, with the
% conjuncts removed.
listOfSentences( Conj, [HeadS|TailS] ):-
  sentence(Conj), HeadS = Conj, TailS = [].

listOfSentences( Conj, [HeadS|TailS] ):-
  splitAtConj( Conj, S, Rest), HeadS = S, listOfSentences( Rest, TailS ).

% From a single sentence, extracts the the verb which the Actor performs
verbFromSentence( Actor, Sentence, Verb ):-     
  append( NounVS, NounP, Sentence),    % Extract a Noun-Verb sentence
  sentence(NounVS),                     
  (noun_phrase(NounP); NounP = []),                  
  append(NP, V, NounVS),               % Separate Noun from Verb
  verb(V),                          
  append(A, Noun, NP),                 % Identify if noun is actor
  noun(Noun),
  (Noun = [Actor] -> Verb = V).

% From a list of sentences, extract the verb which an actor performed, and
% add this actor as the next item to a list of verbs the actor performed
verbList( Actor, [], [] ).

verbList( Actor,[Sentence|Sentences], List ):-
  (verbFromSentence( Actor, Sentence, [Verb|_]), addToList( List, Verb, Next), 
  verbList( Actor, Sentences, Next)); verbList( Actor, Sentences, List ).

addToList( [H|T], Item, T ):-
  H=Item.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 2a %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% noun_phrase_better, finds the instances of noun phrases, where
% indefinite articles ending in consonants are matched with nouns starting with
% vowels. 
% Note: a grass left in, as correct in some lexicons
noun_phrase_better(NP):-
  article(A), noun(N), append(A, N, NP), 
  flattenList(A, FlatA), atom_chars(FlatA, AChs), 
  flattenList(N, FlatN), atom_chars(FlatN, NChs),
  checkVowel( AChs, NChs).

% checkVowel finds the last character of an article, and the first character of a noun, and 
% passes if:
% -- last char of article is vowel, and first char of noun is not vowel.
% -- last char of article is not vowel, or is vowel e, and first char of noun
%    vowel
checkVowel( AChs, [NounH|NounT] ):-
 (lastElement(AChs, Ch),(\+vowel(Ch); Ch = e), vowel(NounH));
 (lastElement(AChs, Ch), vowel(Ch), \+vowel(NounH)).

% Extracts the element from a single element list.
flattenList( [H|T], H).

% Returns the last element in a list as Last.
lastElement([H|T], Last) :-
  last_(T, H, Last).

last_([], Last, Last).

last_([H|T], _, Last) :-
  last_(T, H, Last).
   
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3a,b %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cadvs parses a conjunction of adverbs of the form: adverb1, adverb2 and adverb3; 
%                                              adverb1 and adverb2
% This conjunction must take for form of a list. Fails if 
% conjunction is not of this form. 
cadvs( Adverbs ):-
  accumulateAds( Adverbs, []), Adverbs \= [].

% accumulateAds set of predicates take a conjunction of adverbs, fails if they
% are not of the form above, and ensures that no repeats of adverbs are 
% identified.  Recurses through the conjunction, accumulating the previously 
% used adverbs, and checks each new element of the conjuction has not been used
% before.
accumulateAds( [], PrevAds).       % Base case. All Advbs in Conjunction checked

accumulateAds( Adverbs , [] ):-    % Seed the accumulating list with first advb
  commaSplitAdverbs( Adverbs, Adv, Rest ), 
  accumulateAds( Rest, Adv ).

accumulateAds( Adverbs, PrevAds):- % Check each new advb against prev advbs
  (commaSplitAdverbs( Adverbs, [Adv|_], Rest );
   andSplitAdverbs( Adverbs, [Adv|_], Rest) ),
  \+member(Adv, PrevAds), NextPrevAds = [Adv|PrevAds], 
  accumulateAds( Rest, NextPrevAds ).


% commaSplitAdverbs splits a conjunction of the form "adverb1, adverb2, adverb3, ..."
% into one variable "adverb1" and the remaining adverbs "adverb2, adverb3, ..."
% the conjunt comma is discared. Also ensures that a comma conjunction 
% cannot be used between the last two adverbs.

commaSplitAdverbs(Adverbs, Adv, Rest):-
  adverb(Adv), H = ',', append(Adv, [H|Rest], Adverbs),
  countItems( Rest, Accum, 0), Accum > 1. % fail if comma is before last element

% Performs similar functionality to commaSplitAdverbs, but for and conjunct.
% Ensures and is used as a conjunct only between penultimate and last adverbs.
andSplitAdverbs(Adverbs, Adv, Rest):-
  (adverb(Adv), H = and, append(Adv, [H|Rest], Adverbs),
  countItems( Rest, Accum, 0), Accum = 1);    % fail if and is not before last element
   adverb(Adverbs), Adv = Adverbs, Rest = []. % Called if only one adverb in Adverbs

% countItems, returns a count of the number of elements in a list
countItems( [], Accum, Count ):-
  Accum = Count.

countItems( [Adverb|Rest], Accum, Count):-
  NewCount is Count+1, countItems(Rest, Accum, NewCount).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% verb_phrase_better passes with verb phrases containing, correct
% noun phrase pairing, correct noun phrase pairing headed by a correctly
% formed conjunct of adverbs
verb_phrase_better(VP):-
  verb(VP);
  possible_verb_one(VP);
  possible_verb_two(VP);
  possible_verb_three(VP).

possible_verb_one(VP):-
  append(V, NP, VP), verb(V), noun_phrase_better(NP).

possible_verb_two(VP):-
  append(Cads, V, VP), cadvs(Cads), verb(V).

possible_verb_three(VP):-
  append(Cads, VNP, VP), cadvs(Cads), possible_verb_one(VNP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3d %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sentence_better passes when correctly formed noun phrases are followed by
% a correctly formed verb phrase, with or without a conjunction of adverbs
% preceeding the verb
sentence_better(Sentence):-
  append(NP, VP, Sentence), noun_phrase_better(NP), verb_phrase_better(VP).
