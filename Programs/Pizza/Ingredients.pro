
/****************************************************************************

	article -
		Retourne l'article d�fini et ind�fini que l'on peut utiliser
		avec l'ingr�dient sp�cifi� dans une phrase.
			 		
 ****************************************************************************/

article(A, Defini, Indefini, Complement) :-
	atom(A),
	do_article([A], Defini, Indefini, Complement).

article([H|T], Defini, Indefini, Complement) :-
	do_article([H|T], Defini, Indefini, Complement).


%
% do_article -
%	recherche le genre et le nombre de l'ingredient, les v�rifie,
%	et retourne les articles d�fini et ind�fini � utiliser avec.
%
do_article([H|T], Defini, Indefini, Complement) :-
	ingredient([H|T], Genre, Nombre),
	quel_article(H, Genre, Nombre, Defini, Indefini, Complement).


%
% Quels articles utiliser ?
%	
quel_article(Atom,        _, pluriel  , [les]    , [des]	, [d, '''']) :-
	voyelle(Atom).
quel_article(Atom,        _, pluriel  , [les]    , [des]	, [de]) :-
	consonne(Atom).
quel_article(Atom,        _, singulier, [l, ''''], [de, l, ''''], [d, '''']) :-
	voyelle(Atom).
quel_article(Atom, feminin , singulier, [la]     , [de, la]	, [de]) :-
	consonne(Atom).
quel_article(Atom, masculin, singulier, [le]     , [du]		, [de]) :-
	consonne(Atom).

%
% L'ingr�dient commence par une voyelle ou une consonne ?
%
voyelle(Atom) :-
	sub_atom(Atom, 1, 1, X),
	is_member(X, [a, e, i, o, u, y]).
	
consonne(X) :-
	not(voyelle(X)).


%
% verification du genre et du nombre.
%
genre(_, masculin) :- !.
genre(_, feminin)  :- !.
genre(Nom, Genre) :-
	write('Erreur : l''ingr�dient '),
	write(Nom),
	write(' a un genre incorrect : ['),
	write(Genre),
	write(']'),
	nl, 
	!, fail.
	
nombre(_, singulier) :- !.
nombre(_, pluriel)  :- !.
nombre(Nom, Nombre) :-
	write('Erreur : l''ingr�dient '),
	write(Nom),
	write(' a un nombre incorrect : ['),
	write(Nombre),
	write(']'),
	nl, 
	!, fail.


/****************************************************************************

	categorie -
		Retourne la classe d'un article (jambon, fromage, etc.).
			 		
 ****************************************************************************/
categorie(Atom, Cat) :-
	atom(Atom),
	categorie([Atom], Cat).
categorie(List, Cat) :-
	classe(Cat, List).
	