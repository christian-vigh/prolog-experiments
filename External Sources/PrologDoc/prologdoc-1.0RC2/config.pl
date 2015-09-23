/* -*- Mode: Prolog -*-

  This file is part of PrologDoc (http://prologdoc.sourceforge.net/).

  Copyright (C) 2004 by Salvador Fandino <sfandino@yahoo.com>

  PrologDoc is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  PrologDoc is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PrologDoc; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module( config, [setConfig/2,
		    setConfig/1,
		    config/2,
		    config/1] ).

:- dynamic config1/2.

setConfig(K, V) :-
	retractall(config1(K,_)),
	assert(config1(K,V)).

config(K,V) :-
	config1(K,V),
	!.

setConfig(K) :-
	setConfig(K, true).

config(K) :-
	config(K, true).