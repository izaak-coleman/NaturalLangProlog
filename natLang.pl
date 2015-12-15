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
vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).

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
  checkVowel( AChs, NChs), NP \= [a,grass].% remove NP with collective 
                                           % and indefinite article
checkVowel( AChs, [NounH|NounT] ):-
 (lastElement(AChs, Ch),(\+vowel(Ch); Ch = e), vowel(NounH));
 (lastElement(AChs, Ch), vowel(Ch), \+vowel(NounH)).

flattenList( [H|T], H).

lastElement([H|T], Last) :-
  last_(T, H, Last).
 
last_([], Last, Last).

last_([H|T], _, Last) :-
  last_(T, H, Last).
   
  
