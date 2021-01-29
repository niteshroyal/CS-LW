%%% -*- Mode: Prolog; -*-
/*
 *
 * Copyright (C) 2020, 2021 Nitesh Kumar
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

:- module('propositionalProbLogic_BayesBall', [init/0, query/3, query/4, query_timeout/6, set_debug/1, set_approach/1, set_default/1, set_sample_size/1, set_time_out/1, set_seed/1]).

:- load_foreign_library('../Sampling/sampling').

:- dynamic head/2, intervention/1, evidence/1, sample_size/1, debug/1, default/1, approach/1, rule/2, time_out/1.

:- op(690,xfx,user:'::').
:- op(690,xfx,user:'~').
:- op(681,xfx,user:'~=').
:- op(1100,xfx,user:':=').

:- table (herbrand_base/1) as subsumptive.

%:- initialization(init).

init :- \+backward_convert([do(X), X~Y, X::Y, X:=Y, evd(X)]),
	\+forward_convert([X~Y, X::Y, X:=Y]),
	\+define_herbrand_base([do(X), X~Y, X::Y, X:=Y]),
	\+assert_heads,
	connect_sampler(1),
	set_debug(0),
	%set_approach(partial),
	set_approach(full),
	set_default(0),
	set_time_out(0).

release :- connect_sampler(0),
	abolish_all_tables.

set_seed(S) :- set_sampler_seed(S).

/* Rule form conversion for backward-chaining */
backward_convert(All_special_predicates) :- 
	member(Pred,All_special_predicates), 
	backward_convert2(Pred).

backward_convert2(L) :- 
	clause(L,_), 
	new_backward_rule(L), 
	fail.

new_backward_rule(do(X)) :- 
	%writeln(intervention(X)),
	assertz(intervention(X)).
new_backward_rule(evd(X)) :-
	%writeln(evidence(X)),
	assertz(evidence(X)). 
new_backward_rule(H~D) :-
	%writeln(:-(distributional(H),add_distribution(H,D))),
	assertz(:-(distributional(H),add_distribution(H,D))).
new_backward_rule(P::F) :- 
	%writeln(:-(distributional(F),add_distribution(F,bernoulli(P)))),
	assertz(:-(distributional(F),add_distribution(F,bernoulli(P)))).
new_backward_rule(H~D:=R) :- 
	add_at_end(R,add_distribution(H,D),R_distribution),
	%writeln(:-(distributional(H),R_distribution)),
	assertz(:-(distributional(H),R_distribution)).
new_backward_rule(P::F:=R) :- 
	add_at_end(R,add_distribution(F,bernoulli(P)),R_distribution),
	%writeln(:-(distributional(F),R_distribution)),
	assertz(:-(distributional(F),R_distribution)). 
 

add_at_end(A,E,(A,E)) :- \+(A=(_,_)), !.
add_at_end((A,B),E,C) :- \+(B=(_,_)), C=(A,(B,E)), !.
add_at_end((A,B),E,(A,C)) :- add_at_end(B,E,C).

/* Rule form conversion for forward-chaining */
forward_convert(All_special_predicates) :- 
	member(Pred,All_special_predicates), 
	forward_convert2(Pred).

forward_convert2(L) :- 
	clause(L,_), 
	new_forward_rule(L), 
	fail.
new_forward_rule(H~_:=R) :- 
	list_tuple(R,Rlist), 
	%writeln(rule(H,Rlist)),
	assertz(rule(H,Rlist)).
new_forward_rule(_::F:=R) :- 
	list_tuple(R,Rlist), 
	%writeln(rule(F,Rlist)),
	assertz(rule(F,Rlist)).

assert_heads :- 
	rule(L,R), 
	member(G,R), 
	G=F~=_, 
	assert_heads_once(F,L), 
	fail.

assert_heads_once(F,L) :- 
	( head(F,L) ->
		true
	;	assertz(head(F,L))
	).

%% TODO Verify list_tuple
list_tuple(A,[A]) :- \+(A=(_,_)), !.
list_tuple((A,R), [A,B,C|L]) :- R = (B,_), !, list_tuple(R,[B,C|L]). 
list_tuple((A,B), [A,B]).


/* Define herbrand base of the program */
define_herbrand_base(All_special_predicates) :- 
	member(Pred,All_special_predicates), 
	define_herbrand_base2(Pred).
define_herbrand_base2(L) :- 
	clause(L,_), 
	herbrand_base_rule(L), 
	fail.
herbrand_base_rule(do(X)) :- 
	herbrand_base_format(X,Y),
	%writeln(Y),
	assertz(Y).
herbrand_base_rule(H~_) :- 
	%writeln(herbrand_base(H)), 
	assertz(herbrand_base(H)).
herbrand_base_rule(_::F) :- 
	%writeln(herbrand_base(F)), 
	assertz(herbrand_base(F)).
herbrand_base_rule(H~_:=R) :-
	herbrand_base_format(R,R1),
	%writeln(:-(herbrand_base(H),R1)), 
	assertz(:-(herbrand_base(H),R1)).
herbrand_base_rule(_::F:=R) :-
	herbrand_base_format(R,R1),
	%writeln(:-(herbrand_base(F),R1)), 
	assertz(:-(herbrand_base(F),R1)).

herbrand_base_format(A,A1) :- \+(A=(_,_)), process_atoms(A,A1), !. 
herbrand_base_format((A,B),(A1,B1)) :- herbrand_base_format(B,B1), process_atoms(A,A1).

%% TODO take care of built-in predicates (including true) in process_atoms
process_atoms(A,A1) :- A = not(R~=_), A1=herbrand_base(not(R)).
process_atoms(A,A1) :- \+(A = not(_~=_)), A = R~=_, A1=herbrand_base(R).
process_atoms(A,A1) :- \+(A = not(_~=_)), \+(A = _~=_), A=writeln(_), A1=A.
process_atoms(A,A1) :- \+(A = not(_~=_)), \+(A = _~=_), \+(A=writeln(_)), A1=herbrand_base(A).

/* Backward chaining */
prove_all(P) :- not(alltried(P)).
alltried(P) :- call(P), fail.

%% Definition of add_distribution(A,D)
add_distribution(A,D) :- term_hash(distribution(A),H), recordz(H,distribution(A,D)).

%% Definition of X~=Y
X~=Y :- herbrand_base(X), assign(X,Z), Z=Y.

assign(X,Y) :- 	
	( intervention(X~=Y) ->
		true
	; 	( evidence(X~=Y) ->
			( recorded_possible_predictive_evd(X) -> 
				true
			; 	record_possible_predictive_evd(X)
			)
	  	; 	( recorded_top(X) ->
				recorded_assigned(X,Y)
	    		; 	P=distributional(X), 
				prove_all(P), 
				sample(X,Y), 
				record_assigned(X,Y), record_top(X),
				( recorded_bottom(X) -> 
					true
				; 	record_bottom(X), recordz(agenda,X)
				)
	    		)
	  	)
	).


/* Forward chaining */
%% TODO: Current implementation works only for ground rules
%% First-order implementation requires a function like 'member(G,R)' where R is ground. 
forward :- \+recorded(agenda,_), !.
forward :- recorded(agenda,F,R), erase(R), prove_all(pursuit(F)), forward.

pursuit(F) :- head(F,L), infer_head(L).

infer_head(L) :- \+intervention(L~=_), evidence(L~=Y), \+recorded_top(L), record_top(L), 
	P=distributional(L), prove_all(P), weight(L,Y,W), record_diagnostic_evidence(L,Y,W).
infer_head(L) :- \+intervention(L~=_), \+evidence(L~=_), \+recorded_bottom(L), record_bottom(L), recordz(agenda,L).

/* Inference */
query(Q,P) :- check_sample_size, sample_size(N), 
	findall(Entail, (between(1,N,_), prove(Q,Entail)), L), 
	probability(L,P).

query(Q,E,P) :- check_sample_size, debug(F), check_debug, sample_size(N), clean, add_evidence(E),
	findall(Entail, (between(1,N,K), forward_backward(K,Q,Entail)), L),
	( approach(full) -> 
		list_of_full_weights_and_residuals(N, PW, Res), expected_lw(PW,Res,EW)
	;	list_of_partial_weights(N,PW), expected_lw(PW,EW)
	), probability(L,EW,P), analyze_evidence(N,PW,EW,L,F), remove_evidence(E).

query_timeout(Q,E,P,M,Secs,N) :- debug(F), check_debug, clean, add_evidence(E), set_time_out(Secs), 
	findall(Entail, (between(1,M,K), time_out(X), get_time(Y), Y<X, forward_backward(K,Q,Entail)), L),
	length(L,N), 
	( approach(full) -> 
		list_of_full_weights_and_residuals(N, PW, Res), expected_lw(PW,Res,EW)
	;	list_of_partial_weights(N,PW), expected_lw(PW,EW)
	), probability(L,EW,P), analyze_evidence(N,PW,EW,L,F), remove_evidence(E).

query(Q,E,D,P) :- check_sample_size, debug(F), check_debug, sample_size(N), clean, add_intervention(D), add_evidence(E),
	findall(Entail, (between(1,N,K), forward_backward(K,Q,Entail)), L),
	list_of_partial_weights(N,PW), expected_lw(PW,EW), probability(L,EW,P),
	analyze_evidence(N,PW,EW,L,F), remove_evidence(E), remove_intervention(D).

prove(Q,Entail) :- 
	forall(recorded(_,_,R), erase(R)), 
	(call(Q) -> Entail=1; Entail=0).

forward_backward(K,Q,Entail) :- 
	forall(recorded(_,_,R), erase(R)),
	(call(Q) -> Entail=1; Entail=0),
	forward, record(K), debug(F), analyze_sample(K,F).

/* Sampling */
%% Definition of sample(X,Y)
all_distributions(X,D_list) :- term_hash(distribution(X),H), findall(D, recorded(H,distribution(X,D)), D_list).

sample(X,Y) :- all_distributions(X,D_list),
	( D_list=[] -> 
		( default(1) -> 
			sample_default(X,Y)
		;	writeln('Sampling from default distribution not handled')
		)
	; 	combine_distributions(D_list, D_combined), 
		sample_distribution(D_combined,Y)
	).

weight(X,Y,W) :- all_distributions(X,D_list),
	( D_list=[] ->
		( default(1) -> 
			weight_default(X,Y,W)
		;	writeln('Weighting from default distribution not handled')
		)
	; 	combine_distributions(D_list, D_combined), 
		weight_distribution(D_combined,Y,W)
	).

%% TODO Different default sampling rule for different type of random variables
sample_default(_,Y) :- Y=0. % For Boolean random variable
weight_default(_,Y,W) :- (Y=0 -> W is 1; W is 0). % For Boolean random variable

sample_distribution(bernoulli(P),V) :- sample_bernoulli(P,V).
sample_distribution(discrete(L),V) :- sample_discrete_pl(L,V).
weight_distribution(bernoulli(P),V,W) :- weight_bernoulli(P,V,W).
weight_distribution(discrete(L),V,W) :- weight_discrete_pl(L,V,W).

combine_distributions([bernoulli(P)|Tail], D) :- combine_bernoulli([bernoulli(P)|Tail], Q), Q1 is 1-Q, D=bernoulli(Q1).
combine_distributions([discrete(L)|Tail], D) :- 
	( Tail=[] ->
		true
	;	writeln('More than one discrete distribution in a list is not handled')
	), D = discrete(L).

combine_bernoulli([], 1).
combine_bernoulli([bernoulli(P)|T], Q) :- combine_bernoulli(T, Q1), Q is Q1*(1-P). 
weight_bernoulli(P,V,W) :- (V=1 -> W is P; (V=0 -> W is 1-P; fail)).

discrete_get_probs_vals([],[],[]).
discrete_get_probs_vals([H|T],P,V) :- discrete_get_probs_vals(T,P1,V1), P2:V2=H, P=[P2|P1], V=[V2|V1].

sample_discrete_pl(List,Val) :- discrete_get_probs_vals(List,P,V), sample_discrete(P,X), nth0(X,V,Val).

weight_discrete_pl([H|_],Val,W) :- W:Val=H, !.
weight_discrete_pl([_|T],Val,W) :- weight_discrete_pl(T,Val,W).


/* Utilities */
recorded_top(X) :- term_hash(top(X),H), recorded(H, top(X)).
record_top(X) :- term_hash(top(X),H), recordz(H, top(X)).

recorded_bottom(X) :- term_hash(bottom(X),H), recorded(H, bottom(X)).
record_bottom(X) :- term_hash(bottom(X),H), recordz(H, bottom(X)).

recorded_assigned(X,Y) :- term_hash(assigned(X),H), 
	recorded(H, assigned(X,Y)), !.
record_assigned(X,Y) :- term_hash(assigned(X),H), 
	recordz(H, assigned(X,Y)), 
	recordz(assigned, X).
all_assigned(L) :- findall(X~=Y, (recorded(assigned,X), recorded_assigned(X,Y)), L).

recorded_possible_predictive_evd(X) :- term_hash(possible_predictive_evd(X),H), 
	recorded(H,possible_predictive_evd(X)), !.
record_possible_predictive_evd(X) :- term_hash(possible_predictive_evd(X),H),
	recordz(possible_predictive_evd,X), 
	recordz(H,possible_predictive_evd(X)).
all_possible_predictive_evd(L) :- findall(X,(recorded(possible_predictive_evd,X), \+recorded_diagnostic_evidence(X,_,_)),L).

recorded_diagnostic_evidence(X,Y,W) :- term_hash(diagnostic_evidence(X),H),
	recorded(H,diagnostic_evidence(X,[Y,W])), !.
record_diagnostic_evidence(X,Y,W) :- term_hash(diagnostic_evidence(X),H),
	recordz(diagnostic_evidence,X), 
	recordz(H,diagnostic_evidence(X,[Y,W])).
all_diagnostic_evidence(L) :- findall((X,W), (recorded(diagnostic_evidence,X), recorded_diagnostic_evidence(X,_,W)), L).


set_sample_size(N) :- (sample_size(_) ->
	retract(sample_size(_)), asserta(sample_size(N))
	; asserta(sample_size(N)) 
	).

set_debug(N) :- (debug(_) ->
	retract(debug(_)), asserta(debug(N))
	; asserta(debug(N)) 
	).

set_approach(X) :- (approach(_) ->
	retract(approach(_)), asserta(approach(X))
	; asserta(approach(X)) 
	).

set_default(N) :- (default(_) ->
	retract(default(_)), asserta(default(N))
	; asserta(default(N)) 
	).

set_time_out(N) :- 
	( time_out(_) ->
		retract(time_out(_))
	; 	true
	),  get_time(O), M is O + N, asserta(time_out(M)).

check_sample_size :- \+sample_size(_), 
	writeln('Set sample size first.'), fail.
check_sample_size :- sample_size(N), N<1, 
	writeln('Sample size should be greater than zero.'), fail.
check_sample_size :- sample_size(N), N>0.

check_debug :- \+check_debug_fail.
check_debug_fail :- \+debug(1), \+debug(0).


clean :- forall(recorded(_,_,R), erase(R)),
	forall(nb_current(X,_), nb_delete(X)),
	garbage_collect.

record(N) :- all_diagnostic_evidence(V1),
	atom_concat(d,N,K1), nb_setval(K1,V1),
	all_possible_predictive_evd(V2),
	atom_concat(p,N,K2), nb_setval(K2,V2).

list_of_partial_weights(N, List) :- list_of_diagnostic_evd(N,L1), 
	findall(W, (between(1,N,K), atom_concat(d,K,Key), nb_getval(Key,L2), prepare_list(L1,L2,W)), List).


list_of_full_weights_and_residuals(N, List, Res) :- garbage_collect, list_of_diagnostic_evd(N,L1),
	findall((W,R), (between(1,N,K), atom_concat(d,K,Key), nb_getval(Key,L2), 
	forall(recorded(_,_,Ref), erase(Ref)), fill_residuals(L1,L2,W,R,0)), WR), seperate_w_r(WR, List, Res).
	%writeln('List of diagnostic evd'), writeln(L1), writeln('Full Weights'), writeln(List), writeln('Residuals'), writeln(Res).

seperate_w_r([],[],[]).
seperate_w_r([(W,R)|T], List, Res) :- seperate_w_r(T,List1,Res1), List=[W|List1], Res=[R|Res1].


fill_residuals([],_,[],[],_).
fill_residuals([H|T],Partial,Weight,Res,C) :- C1 is C+1, fill_residuals(T,Partial,Weight1,Res1,C1), 
	(is_member(H,Partial,W) -> 
		Weight=[W|Weight1], Res=Res1
	; 	weight_residual(H,W1), Weight=[W1|Weight1], Res=[C|Res1]
	).

weight_residual(X,W) :- P=distributional(X), prove_all(P), evidence(X~=Y), weight(X,Y,W).


list_of_diagnostic_evd(N,List_out) :- N>0, N1 is N-1, list_of_diagnostic_evd(N1,List_out1), 
	atom_concat(d,N,Key), nb_getval(Key,L1), 
	only_evd(L1,L), union(L,List_out1,List_out).
list_of_diagnostic_evd(0,[]).
only_evd([],[]).
only_evd([(E,_)|T],[E|T1]) :- only_evd(T,T1).


list_of_predictive_evd(N,DEvd,PEvd) :- list_of_predictive_evd1(N,PEvd1), subtract(PEvd1,DEvd,PEvd).
list_of_predictive_evd1(N,List_out) :- N>0, N1 is N-1, list_of_predictive_evd1(N1,List_out1), 
	atom_concat(p,N,Key), nb_getval(Key,L), union(L,List_out1,List_out).
list_of_predictive_evd1(0,[]).


prepare_list([],_,[]).
prepare_list([H|T],Partial,Weight) :- prepare_list(T,Partial,Weight1), 
	(is_member(H,Partial,W) -> 
		Weight=[W|Weight1]
	; 	Weight=[-1|Weight1]
	).

is_member(X,[(X,W)|_],W).
is_member(X,[_|L],W) :- is_member(X,L,W).


add_evidence([]).
add_evidence([H|T]) :- add_evidence(T),
	( evidence(H) -> 
		true
	;	assertz(evidence(H))
	).

add_intervention([]).
add_intervention([H|T]) :- add_intervention(T),
	( intervention(H) -> 
		true
	;	assertz(intervention(H))
	).


remove_evidence([]).
remove_evidence([H|T]) :- retract(evidence(H)), remove_evidence(T).

remove_intervention([]).
remove_intervention([H|T]) :- remove_intervention(T), retract(intervention(H)).


probability(L,P) :- count_true(L,M), sample_size(N), P is M/N.


probability(L,EW,P) :- compute_prob(L,EW,P).

count_true([],0).
count_true([H|T],M1) :- count_true(T, M), (H=1 -> M1 is M+1; M1 is M).

analyze_sample(_,0).
analyze_sample(N,1) :- atom_concat('Sample# ',N,S), writeln(''),
	writeln(S), writeln('-------'), writeln(''),
	writeln('partial assignments:'),
	all_assigned(L1), writeln(L1), writeln(''),
	writeln('diagnostic evidence in this sample:'),
	all_diagnostic_evidence(L3), writeln(L3), writeln(''),
	writeln('predictive evidence in this sample:'),
	all_possible_predictive_evd(L2), writeln(L2), writeln('').

analyze_evidence(N,PW,EW,L,1) :- writeln(''), writeln('-------'), writeln('Summary'), writeln('-------'), 
	writeln('diagnostic evidence:'),
	list_of_diagnostic_evd(N,DEvd), writeln(DEvd), writeln(''),
	writeln('predictive evidence:'),
	list_of_predictive_evd(N,DEvd,PEvd), writeln(PEvd), writeln(''),
	write('partial weights = '), writeln(PW), writeln(''),
	write('expected likelihood weights = '), writeln(EW), writeln(''),
	write('entailments of query = '), writeln(L), writeln('').
analyze_evidence(_,_,_,_,0).


%% Sampling speed of GSL library (Intel(R) Core(TM) i7-8550U CPU @ 1.80GHz):
% A Bernoulli distribution:
%				Prolog		Prolog+sampling		Actual Sampling time	Actual Samping time
%									   (in Prolog)              (in C++)
%				------		---------------		--------------------	-------------------
%	0.1 billion samples:	2.781  secs	10.277  secs		7.496  secs		0.842 secs
%	1.0 billion samples:	30.656 secs   	123.786 secs		93.13  secs		8.58 secs
%
% A Gaussian distribution:
%	0.1 billion samples:	2.692 secs	21.188 secs		18.496 secs		6.908 secs
again(0) :- !.
again(N) :- sample_bernoulli(0.9,_), N1 is N-1, again(N1).
%again(N) :- sample_gaussian(0.9,0.3,_), N1 is N-1, again(N1).
%again(N) :- N1 is N-1, again(N1).


