/**********************************************************************

         name: bindingDRT.pl (Chapter 9)
      version: Feb 3, 1999
  description: Check Binding Constraints
      authors: Patrick Blackburn & Johan Bos
 
**********************************************************************/

:- module(bindingDRT,[potentialAntecedent/3,
		      properBinding/3]).

:- use_module(semOntology,[consistent/2]),
   use_module(comsemPredicates,[compose/3,member/2,appendLists/3]).


/*=====================================================================
     Potential Antecedent (Ordinary DRSs)
=====================================================================*/
% merge operations are necessary in order to deal with the format in which the Boxer receives information

drsMerge(drs([],[B]),drs([C],[]),drs([C],[B])).

drsMerge(drs(A,B),drs(C,D),drs(E,F)):-
	appendLists(A,C,E),
	appendLists(B,D,F).

potentialAntecedent([A,B|E],X,pred(Symbol1,D)):-
	drsMerge(A,B,C),
	potentialAntecedent([C|E],X,pred(Symbol1,D)).
	
potentialAntecedent([A,B],X,pred(Symbol1,D)):-
	drsMerge(A,B,C),
	potentialAntecedent([C],X,pred(Symbol1,D)).
	
%potentialAntecedent can check for consistency now

potentialAntecedent([drs(A,B)],X,pred(Symbol1,_)):-
   member(drs(Dom,Conds),[drs(A,B)]),
   member(X,Dom),
 %  compose(Gender,Symbol1,_),
   \+ (
         member(pred(Symbol2,Y),Conds),
%	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ),
	\+ (
         member(eq(Z,Symbol3),Conds),
%	  compose(Cond,Symbol3,[Z]),
	  Z==X,
          \+ consistent(Symbol1,Symbol3)
      ).
	 
	

	  

/*=====================================================================
     Potential Antecedent (Focus DRSs)
=====================================================================*/
%
/*
potentialAntecedent(A,X,Gender):-
   member(drs(Dom,_,_,Conds),A),
   member(X,Dom),
   compose(Gender,Symbol1,_),
   \+ (
          member(Cond,Conds),
	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ).

*/
%
/*=====================================================================
   Check Binding violation.
=====================================================================*/
	      
properBinding(Type,X,Drs):-
	Type=refl, 
	reflexiveBinding(X,Drs).

properBinding(Type,X,Drs):-
	\+ Type=refl,
	(
	    reflexiveBinding(X,Drs),
	    !, fail
	;
	    true
	).

reflexiveBinding(_,[]):- fail.
reflexiveBinding(_,alfa(_,_,_,_)):- fail.
reflexiveBinding(_,merge(_,_)):- fail.
reflexiveBinding(_,not(_)):- !, fail.
reflexiveBinding(X,drs(_,Conds)):-
   reflexiveBinding(X,Conds).


% reflexiveBinding updated to deal with event semantics
  
reflexiveBinding(X,Conds):- !,
	
         
	 member(rel(Sym1,EV,Subj),Conds),
         member(rel(Sym2,EV,Obj),Conds),
         Sym1 == Sym2,
	%    compose(Basic,_Sym,[Subj,Obj]),
	   member(eq(X,Obj),Conds),
	    X==Subj,!.


/*
reflexiveBinding(X,Conds):- !,
	
         
	 member(rel(arg1,EV,Subj),Conds),
         member(rel(arg2,EV,Obj),Conds),
        % Sym1 == Sym2,
	%    compose(Basic,_Sym,[Subj,Obj]),
			(member(eq(X,Subj),Conds),
			X==Obj);
			(member(eq(X,Obj),Conds),
			X==Subj),!.
*/
