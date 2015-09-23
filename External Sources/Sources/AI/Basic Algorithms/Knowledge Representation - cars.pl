%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Luger, G.F and W. A. Stubblefield. 
% Artificial Intelligence: Structures and Strategies for Complex Problem
% Solving, Benjamin/Cummings, Redwood City, California, second edition, 1993.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the sample automotive diagnostic knowledge base for use 
% with the EXSHELL expert system shell in section 12.2 of the text.
% When running it, be sure to load it with the file containing 
% EXSHELL.

% To start it, give PROLOG the goal:
%		solve(fix(X), CF).

% Knowledge Base for simple automotive diagnostic expert system.
% Top level goal, starts search.

rule((fix(Advice) :-
	(bad_component(X),fix(X, Advice))), 100).


% rules to infer bad component:

rule((bad_component(starter) :- 
	(bad_system(starter_system),lights(come_on))),50).
rule((bad_component(battery) :- 
	(bad_system(starter_system),not(lights(come_on)))),90).
rule((bad_component(timing) :- 
	(bad_system(ignition_system), not(tuned_recently))),80).
rule((bad_component(plugs) :- 
	(bad_system(ignition_system),plugs(dirty))),90).
rule((bad_component(ignition_wires) :-
	(bad_system(ignition_system), 
	not(plugs(dirty)), tuned_recently)),80).


% Rules to infer system that failed.

rule((bad_system(starter_system) :- 
	(not(car_starts), not(turns_over))),90).
rule((bad_system(ignition_system) :- 
	(not(car_starts), turns_over,gas_in_carb)),80).
rule((bad_system(ignition_system) :- 
	(runs(rough),gas_in_carb)),80).
rule((bad_system(ignition_system) :- 
	(car_starts, runs(dies),gas_in_carb)),60).

% Rules to make reccommendation for repairs.

rule(fix(starter, 'replace starter'),100).
rule(fix(battery, 'replace or recharge battery'),100).
rule(fix(timing, 'get the timing adjusted'),100).
rule(fix(plugs, 'replace spark plugs'),100).
rule(fix(ignition_wires, 'check ignition wires'),100).

% askable descriptions

askable(car_starts).
askable(turns_over).
askable(lights(_)).
askable(runs(_)).
askable(gas_in_carb).
askable(tuned_recently).
askable(plugs(_)).

