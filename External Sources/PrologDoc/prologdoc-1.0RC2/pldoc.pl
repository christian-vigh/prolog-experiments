/* -*- Mode: Prolog -*-

  This file is part of PrologDoc (http://prologdoc.sourceforge.net/).

  Copyright (C) 1999 by Elisheva Bonchek (le7bonch AT cs.huji.ac.il) and Sara Cohen (sarac AT ie.technion.ac.il, http://iew3.technion.ac.il/~sarac) 
  Copyright (C) 2004 by Bram Adams (bram.adams AT ugent.be)
  Copyright (C) 2004 by Salvador Fandino (sfandino@yahoo.com)

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

:- consult(prolog_doc(prolog_doc)).

main :-
	style_check(-atom),  %kill string length limit
	current_prolog_flag(argv, AllArgs),
	(   append(_, ['--', Prefix, SideEffects,
		       Footer, Load, To|Files], AllArgs)
	->  setConfig(html_prefix, Prefix),
	    setConfig(include_side_effects, SideEffects),
	    setConfig(html_footer, Footer),
	    prologDoc(To, Files, Load)
	;   writef("invalid arguments: %q", AllArgs) ).
	
