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

actions( Actor, Text, As ):-
  listOfSentences( Text, Sentences), verblist(Actor, Sentences, As),!.

verblist( Actor, [], [] ).
verblist( Actor,[Sentence|Sentences], List ):-
  (verbFromSentence( Actor, Sentence, [Verb|_]), addToList( List, Verb, Next), 
  verblist( Actor, Sentences, Next)); verblist( Actor, Sentences, List ).

addToList( [H|T], Item, T ):-
  H=Item.
  
