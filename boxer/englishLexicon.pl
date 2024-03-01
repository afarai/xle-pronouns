/*************************************************************************
         name: englishLexicon.pl
      version: November 12, 1997; March 9, 1999.
  description: Lexical entries for a small coverage of English
      authors: Patrick Blackburn & Johan Bos
 
This file contains the lexical entries for a small fragment of
English.  Entries have the form lexicon(Cat,Sym,Phrase,Misc), where
Cat is the syntactic category, Sym the predicate symbol introduced
by the phrase, Phrase a list of the words that form the phrase, and
Misc miscellaneous information depending on the the type of entry.
*************************************************************************/

/*========================================================================
   Determiners: lexicon(det,_,Words,Type)
========================================================================*/

lexicon(det,_,[every],uni).
lexicon(det,_,[a],indef).
lexicon(det,_,[the],def).
lexicon(det,_,[one],card(1)).
lexicon(det,_,[another],alt).
lexicon(det,_,[his],poss(male)).
lexicon(det,_,[her],poss(female)).
lexicon(det,_,[its],poss(nonhuman)).

/*========================================================================
   Nouns: lexicon(noun,Symbol,Words,{[],[Hypernym],Hypernym})
========================================================================*/

% lexicon entries relevant for the testsuite

lexicon(noun,human,[human],[organism]).
lexicon(noun,female,[female],[human]).
lexicon(noun,male,[male],[human]).
lexicon(noun,bear,[bear],[animal]).
lexicon(noun,animal,[animal],[organism]).
lexicon(noun,woman,[woman],female).
lexicon(noun,girl,[girl],female).
lexicon(noun,man,[man],male).
lexicon(noun,act,[act],[top]).
lexicon(noun,organism,[organism],[top]).

%classified as nouns, but verbs in event semantics
lexicon(noun,hug,[hug],[act]).
lexicon(noun,laugh,[laugh],[act]).
lexicon(noun,appear,[appear],[act]).
lexicon(noun,see,[see],[act]).
lexicon(noun,cook,[cook],[act]).

%'generic' antecedent
%lexicon(noun,antecedent,[antecedent],female).

  
/*========================================================================
   Proper Names: lexicon(pn,Symbol,Words,{male,female})
========================================================================*/
%name entries relevant for the testsuite
%capitalization and quotes are important 

lexicon(pn,'Mary',['Mary'],female).
lexicon(pn,'Misty',['Misty'],female).
lexicon(pn,'Shauna',['Shauna'],female).
lexicon(pn,'Tom',['Tom'],male).
lexicon(pn,'David',['David'],male).
lexicon(pn,'Jeff',['Jeff'],male).
lexicon(pn,'Ben',['Ben'],male).



/*========================================================================
   Intransitive Verbs: lexicon(iv,Symbol,Words,{fin,inf})
========================================================================*/

lexicon(iv,pretty,[is,pretty],fin).
lexicon(iv,blush,[blushes],fin).

/*
lexicon(iv,collapse,[collapses],fin).
lexicon(iv,collapse,[collapse],inf).
lexicon(iv,dance,[dances],fin).
lexicon(iv,dance,[dance],inf).
lexicon(iv,die,[dies],fin).
lexicon(iv,die,[die],inf).
lexicon(iv,growl,[growls],fin).
lexicon(iv,growl,[growl],inf).
lexicon(iv,okay,[is,okay],fin).
lexicon(iv,outoftown,[is,out,of,town],fin).
lexicon(iv,married,[is,married],fin).
lexicon(iv,playairguitar,[plays,air,guitar],fin).
lexicon(iv,playairguitar,[play,air,guitar],inf).
lexicon(iv,smoke,[smokes],fin).
lexicon(iv,smoke,[smoke],inf).
lexicon(iv,snort,[snorts],fin).
lexicon(iv,snort,[snort],inf).
lexicon(iv,shriek,[shrieks],fin).
lexicon(iv,shriek,[shriek],inf).
lexicon(iv,walk,[walks],fin).
lexicon(iv,walk,[walk],inf).
*/

/*========================================================================
   Transitive Verbs: lexicon(tv,Symbol,Words,{fin,inf})
========================================================================*/

lexicon(tv,love,[loves],fin).
lexicon(tv,love,[love],inf).
lexicon(tv,hate,[hates],fin).
lexicon(tv,hate,[hate],inf).
lexicon(tv,trick,[tricks],fin).
lexicon(tv,trick,[trick],inf).
lexicon(tv,pull,[pulls],fin).
lexicon(tv,pull,[pull],inf).
lexicon(tv,trust,[trusts],fin).
lexicon(tv,trust,[trust],inf).
lexicon(tv,attack,[attacks],fin).
lexicon(tv,attack,[attacked],fin).
lexicon(tv,attack,[attack],inf).
lexicon(tv,create,[creates],fin).
lexicon(tv,create,[created],fin).
lexicon(tv,create,[create],inf).
lexicon(tv,melt,[melts],fin).
lexicon(tv,melt,[melt],inf).

/*
lexicon(tv,clean,[cleans],fin).
lexicon(tv,clean,[clean],inf).
lexicon(tv,drink,[drinks],fin).
lexicon(tv,drink,[drink],inf).
lexicon(tv,date,[dates],fin).
lexicon(tv,date,[date],inf).
lexicon(tv,discard,[discards],fin).
lexicon(tv,discard,[discard],inf).
lexicon(tv,eat,[eats],fin).
lexicon(tv,eat,[eat],inf).
lexicon(tv,enjoy,[enjoys],fin).
lexicon(tv,enjoy,[enjoy],inf).
lexicon(tv,hate,[hates],fin).
lexicon(tv,hate,[hate],inf).
lexicon(tv,have,[has],fin).
lexicon(tv,have,[have],inf).
lexicon(tv,donewith,[is,done,with],fin).
lexicon(tv,kill,[kills],fin).
lexicon(tv,kill,[kill],inf).
lexicon(tv,know,[knows],fin).
lexicon(tv,know,[know],inf).
lexicon(tv,like,[likes],fin).
lexicon(tv,like,[like],inf).
lexicon(tv,love,[loves],fin).
lexicon(tv,love,[love],inf).
lexicon(tv,pickup,[picks,up],fin).
lexicon(tv,pickup,[pick,up],inf).
lexicon(tv,shoot,[shot],fin).
lexicon(tv,shoot,[shoot],inf).
lexicon(tv,tell,[told],fin).
lexicon(tv,tell,[tell],inf).
lexicon(tv,worksfor,[works,for],fin).
lexicon(tv,worksfor,[work,for],inf).
*/

/*========================================================================
   Copula
========================================================================*/

lexicon(cop,eq,[is],fin).

/*========================================================================
   Prepositions: lexicon(prep,Symbol,Words,_)
========================================================================*/

lexicon(prep,in,[in],_).
lexicon(prep,of,[of],_).
lexicon(prep,with,[with],_).

/*========================================================================
   Pronouns: lexicon(pro,Sym,Words,{refl,nonrefl})
========================================================================*/

lexicon(pro,male,[he],nonrefl).
lexicon(pro,female,[she],nonrefl).
lexicon(pro,nonhuman,[it],nonrefl).
lexicon(pro,male,[him],nonrefl).
lexicon(pro,female,[her],nonrefl).
lexicon(pro,male,[himself],refl).
lexicon(pro,female,[herself],refl).
lexicon(pro,nonhuman,[itself],refl).

/*========================================================================
   Relative Pronouns: lexicon(relpro,_,Words,_)
========================================================================*/

lexicon(relpro,_,[who],_).
lexicon(relpro,_,[that],_).

/*========================================================================
   Coordinations: lexicon(coord,_,Words,{conj,disj})
========================================================================*/

lexicon(coord,_,[and],conj).
lexicon(coord,_,[or],disj).

/*========================================================================
   Discontinious Coordinations: lexicon(dcoord,W1,W2,{conj,cond,disj})
========================================================================*/

lexicon(dcoord,[if],[then],cond).
lexicon(dcoord,[if],[],cond).
lexicon(dcoord,[either],[or],disj).
lexicon(dcoord,[],[or],disj).
lexicon(dcoord,[],[and],conj).
lexicon(dcoord,[],[],conj).

/*========================================================================
   Modifiers: lexicon(mod,_,Words,Type)
========================================================================*/

lexicon(mod,_,[does,not],neg).
lexicon(mod,_,[did,not],neg).
