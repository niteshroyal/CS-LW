%%% -*- Mode: Prolog; -*-

:- load_foreign_library(sampling).
%:- debug.

:- initialization(init).

init :- connect_sampler(1).

%:- load_foreign_library('sampling').


discrete_get_probs_vals([],[],[]).
discrete_get_probs_vals([H|T],P,V) :- discrete_get_probs_vals(T,P1,V1), P2:V2=H, P=[P2|P1], V=[V2|V1].

sample_discrete_pl(List,Val) :- discrete_get_probs_vals(List,P,V), sample_discrete(P,X), nth0(X,V,Val).

weight_discrete_pl([H|_],Val,W) :- W:Val=H, !.
weight_discrete_pl([_|T],Val,W) :- weight_discrete_pl(T,Val,W).

smokes(ann).
again(0) :- !.
again(N) :- 
	%discrete_get_probs_vals([0.1:a,0.7:b,0.1:c,0.1:d],_,_),
	%sample_discrete_pl([0.1:a,0.7:b,0.2:c],_),
	%weight_discrete_pl([0.1:a,0.7:b,0.2:c],c,_),
	% weight_discrete([0.1:a,0.7:b,0.2:c],c,_),
	% sample_discrete_new([0.1,0.7,0.2],_), 
	% sample_discrete([0.1:a,0.7:b,0.2:c],_), 
	% sample_bernoulli(0.9,_), 
	%get_time(_),
	smokes(_),
	N1 is N-1, again(N1).

again2(N) :- findall(_,(between(1,N,_), smokes(_)),_).

