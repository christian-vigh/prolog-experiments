/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * Main file. Loads all necessary predicates and opens the window 
 * of the application
 **/

?- abolish_files(['ppt.pl', 'grammar.pl', 'lexicon.pl', 'gui.pl', 'input.pl']).
?- consult('gui.pl').
?- consult('ppt.pl').
?- consult('grammar.pl').
?- consult('lexicon.pl').
?- consult('input.pl').

?- start.
