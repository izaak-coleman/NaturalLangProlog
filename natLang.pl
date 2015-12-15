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

conjunct([and]).
conjunct([',']).
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
  sentence(S), conjunct([H]), append(S, [H|Rest], Conj).


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
noun_phrase_better(NP):-
  article(A), noun(N), append(A, N, NP), 
  flattenList(A, FlatA), atom_chars(FlatA, AChs), 
  flattenList(N, FlatN), atom_chars(FlatN, NChs),
  checkVowel( AChs, NChs).

checkVowel( AChs, [NounH|NounT] ):-
 (lastElement(AChs, Ch),(\+vowel(Ch); Ch = e), vowel(NounH));
 (lastElement(AChs, Ch), vowel(Ch), \+vowel(NounH)).

flattenList( [H|T], H).

lastElement([H|T], Last) :-
  last_(T, H, Last).
 
last_([], Last, Last).

last_([H|T], _, Last) :-
  last_(T, H, Last).
   
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3a,b %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%validAdConj( Adverbs ):-
%  splitAdverbs( Adverbs, Adv, Rest ),
%  addToList( PrevAds, Ad, End),
%  notPreviouslyUsed( PrevAds).

cadvs( Adverbs ):-
  accumulateAds( Adverbs, []).

accumulateAds( [], PrevAds).

accumulateAds( Adverbs , [] ):-
  commaSplitAdverbs( Adverbs, Adv, Rest ), 
  accumulateAds( Rest, Adv ).

accumulateAds( Adverbs, PrevAds):-
  (commaSplitAdverbs( Adverbs, [Adv|_], Rest );
   andSplitAdverbs( Adverbs, [Adv|_], Rest) ),
  \+member(Adv, PrevAds), NextPrevAds = [Adv|PrevAds], 
  accumulateAds( Rest, NextPrevAds ).

commaSplitAdverbs(Adverbs, Adv, Rest):-
  adverb(Adv), H = ',', append(Adv, [H|Rest], Adverbs),
  countItems( Rest, Accum, 0), Accum > 1.

andSplitAdverbs(Adverbs, Adv, Rest):-
  (adverb(Adv), H = and, append(Adv, [H|Rest], Adverbs),
  countItems( Rest, Accum, 0), Accum = 1);
   adverb(Adverbs), Adv = Adverbs, Rest = [].

%countAdverbs([], Accum, Count):-
%  Accum = Count.
%countAdverbs(Adverbs, Accum, Count):-
%  (splitAdverbs(Adverbs, _, Rest); adverb(Adverbs)), 
%  NewCount is Count+1,
%  countAdverbs( Rest, Accum, NewCount),!.

countItems( [], Accum, Count ):-
  Accum = Count.

countItems( [Adverb|Rest], Accum, Count):-
  NewCount is Count+1, countItems(Rest, Accum, NewCount).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Question 3c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

sentence_better(Sentence):-
  append(NP, VP, Sentence), noun_phrase_better(NP), verb_phrase_better(VP).
