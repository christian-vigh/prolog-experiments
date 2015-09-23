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

/***
  @descr This file creates the <font color=\"red\"><i> PrologDoc </i></font>
         documentation for a list of files, using the module file_parser.
         It also creates an index file with a list of the files documented
         and an alphabetized list of the predicates documented.
  @author Elisheva Bonchek
  @author Sara Cohen
  @date July 26, 1999
*/

:-module(prolog_doc, [ prologDoc/3,
		       setConfig/1,
		       setConfig/2 ]).

:- consult(library(system)).
:- consult(library(lists)).

:-use_module(config).
:-use_module(file_parser).
:-use_module(ascii_symbols).

/*
  default configuration settings
*/

:- setConfig(html_prefix, '').
:- setConfig(html_footer, '').
:- setConfig(include_side_effects, false).

/**
  @form prologDoc(To,FileNames,Load)
  @constraints
       @ground To
       @ground FileNames
       @ground Load
  @descr Creates documentation for the files FileNames
         and generates an index file for them.
*/
prologDoc(DestDir, Files, Load) :-
	(   Load = ''
	->  true
	;   read_here:consult(Load) ),
	calculateFileProps(Files, Props),
	sortFileProps(Props, SortedProps),
	run(DestDir, SortedProps, SortedProps, Links),
	indexFile(DestDir, SortedProps, Links).


/*
   run(FileNames, HMTLFileNames, Links, UsedNames)
      Create documentation for FileNames and retrieve
      the names of the HTMLFileNames and all the
      Links in the file.
      <TEMPORARY>UsedNames used because all files are flattened in same directory instead of hierarchy!!!</TEMPORARY>
*/

run(_,[],_,[]) :- !.
run(DestDir, [File|FL], AllFiles, AllLinks) :-
	fileHuman(File, Human),
	filePath(File, Path),
	writef('Processing %t (%t)... ', [Human, Path]),
	generateHTML(DestDir, File, AllFiles, Links),
	createLinks(Links, File, LinksWithFileName),
	writeln('Done!'),
	run(DestDir, FL, AllFiles, RestLinks),
	append(LinksWithFileName, RestLinks, AllLinks).

/*
   createLinks(Predicates, HTMLFileName, Links)
      Create for Predicates the name of the Link to it
      according to its file HTMLFileName
*/
createLinks([],_,[]).
createLinks([(Pred,RelLink)|Others],Prop, [(Pred,Link)|Links]) :-
	fileHTML(Prop, HTML),
	concat_atom([HTML, '#', RelLink], Link),
	createLinks(Others, Prop, Links).

/*
   indexFile(HTMLFileNames,Links)
      Create an index file from HTMLFileNames and Links
*/
indexFile(DestDir,Files,Links):-
	atom_concat(DestDir, 'index.html', Path),
	tell(Path),
	generateBeginning('Index File'),
	putHeader('Files','File List:'),
	writeln('<ul id=\"files\">'),
	generateFileNames(Files),
	writeln('</ul>'),
	putHeader('Links','Predicate List:'),
	sort(Links, SortedLinks),
	generateLinks(SortedLinks,''),
	generateEnding,
	told.

/*
   generateLinks(Links,Letter)
     Write the list of links, alphabetized
*/

generateLinksClose(Letter) :-
	(   Letter = ''
	->  true
	;   writeln('</ul>') ).
	
generateLinks([], L) :-
	generateLinksClose(L).
generateLinks([(Pred,Link)|Links],Letter):-
	firstLetter(Pred,NewLetter),
	(   NewLetter \== Letter
	->  generateLinksClose(Letter),
	    writef('<h3><i>%t</i></h3>\n<ul class="predicates_index">\n', [NewLetter])
	;   true ),
	write('<li><p>'),
	writeLink(Link,Pred),
	writeln('</p></li>'),
	generateLinks(Links,NewLetter).

/*
  generateFileName(FileNames)
     Write the file names in the index file
*/
generateFileNames([]).
generateFileNames([File|Files]):-
	fileHuman(File, Human),
	fileHTML(File, HTML),
	write('<li><p>'), 
	writeLink(HTML,Human),
	writeln('</p></li>'),
	generateFileNames(Files).

calculateFileProps(Files, Calc) :-
	calculateFileProps(Files, [], Calc).

calculateFileProps([], Calc, Calc).
calculateFileProps([FilePath|FT], Calc, R) :-
	absolute_file_name(FilePath, AbsFilePath),
	file_base_name(AbsFilePath, FilePart),
	(   file_name_extension(Base,Ext,FilePart)
	->  true
	;   Base = FilePart,
	    Ext = '' ),
	fileGetUniqueIndex(Base, Calc, Index),
	calculateFileProps(FT, [file(AbsFilePath, Ext, Base, Index)|Calc], R).

filePath(file(FP,_,_,_), FP).
fileExt(file(_,E,_,_), E).
fileBaseIndex(file(_,_,B, I), B, I).

fileHTML(F, HTML) :-
	fileBaseIndex(F, Base, Index),
	(   Index = 1
	->  atom_concat(Base, '.html', HTML)
	;   concat_atom([Base, '-',Index, '.html'], HTML) ).

fileHuman(F, Human) :-
	fileBaseIndex(F, Base, Index),
	(   Index = 1
	->  Human = Base
	;   concat_atom([Base, ' [', Index,']'], Human) ).

fileGetUniqueIndex(Base, Calc, Result) :-
	fileGetUniqueIndex(Base, Calc, 1, Result).

fileGetUniqueIndex(Base, Calc, Index, Result) :-
	fileBaseIndex(F, Base, Index),
	(   member(F, Calc)
	->  I1 is Index+1,
	    fileGetUniqueIndex(Base, Calc, I1, Result)
	;   Result = Index ).

sortFileProps([], []).
sortFileProps([Prop|L], S) :-
	divideFileProps(L, Prop, Small, Bigger),
	sortFileProps(Small, SmallSorted),
	sortFileProps(Bigger, BiggerSorted),
	append(SmallSorted, [Prop|BiggerSorted], S).

divideFileProps([],_,[],[]).
divideFileProps([Prop|More], Pivot, Small, Bigger) :-
	fileBaseIndex(Pivot, PivotBase, PivotIndex),
	fileBaseIndex(Prop, PropBase, PropIndex),
	(   (   PropBase @< PivotBase
	    ;	PropBase = PivotBase,
		PropIndex < PivotIndex )
	->  Small = [Prop|Small1],
	    divideFileProps(More, Pivot, Small1, Bigger) 
	;   Bigger = [Prop|Bigger1],
	    divideFileProps(More, Pivot, Small, Bigger1) ).

/*
   generateHTML(DestDir, FileName, HMTLFileName, Predicates, UsedNames)
       Write the HTML documentation of one file in the file HMTLFileName
       ( the predicates documented are Predicates)
       <TEMPORARY>UsedNames used because all files are flattened in same directory instead of hierarchy!!!</TEMPORARY>
*/
generateHTML(DestDir, File, AllFiles, Links):-
	filePath(File, FilePath),
	analyze(FilePath, ConsultsList, ModulesList,
		SideEffectsList, DetailsList, General, ModuleName),
	fileHTML(File, HTML),
	atom_concat(DestDir, HTML, FullName),
	tell(FullName),
	fileHuman(File, Human),
	makeTitle(Human, ModuleName, Title),
	generateBeginning(Title),
	generateGeneral(General),
	generateConsults(ConsultsList, File, AllFiles),
	generateModules(ModulesList),
	(   config(include_side_effects)
	->  generateSideEffects(SideEffectsList)
	;   true ),
	generatePredicates(DetailsList, Links),
	generateEnding,
	told.


/*
   generateBegining(Title)
       Write the begining of an HTML file, with title 'Title'
*/
generateBeginning(Title) :-
	config(html_prefix, HTMLPrefix),
	write('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">\n'),
	write('<html>\n<head>\n<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=ISO-8859-1">\n'),
	writef('<title>%t</title>\n<link rel="stylesheet" href="%tstyle.css">\n', [Title, HTMLPrefix]),
	writef('</head>\n<body>\n<h1>%t</h1>\n', [Title]).

/*
   makeTitle(FileName,ModuleName,Title)
       Create the title of a documentation file, 
       according to its FileName and ModuleName
*/

makeTitle(Human, Module, Title):-
	(   Module = ''
	->  concat_atom(['Documentation for Prolog File: ', Human], Title)
	;   concat_atom(['Documentation for Prolog Module: ', Module],Title) ).
	
/*
   generateEnding
       Write the ending of a documentation file
*/
generateEnding:-
	config(html_prefix, Prefix),
	config(html_footer, Footer),
	(   Footer = ''
	->  writef('<p><a href="http://prologdoc.sourceforge.net"><img border="0" '),
	    writef('src="%tprologdoc.png" ', [Prefix]),
	    writeln('alt="Generated by PrologDoc" height="18" width="60"></a></p>\n')
	;   writef('<table width="100%"><tr>'),
	    writef('<td width="100%" align="center"><p id="footer"><i>%t</i></p></td>', [Footer]),
	    writef('<td width="0%" align="right"><a href="http://prologdoc.sourceforge.net"><img border="0" '),
	    writef('src="%tprologdoc.png" ', [Prefix]),
	    writeln('alt="Generated by PrologDoc" height="18" width="60"></a></td></tr></table>')  ),
	writeln('\n</body>'),
	writeln('</html>').
	
/* 
  generateGeneral(GeneralList)
      Generate a general description of the file according to a 
      general comment, in GeneralList
*/

generateGeneral(General):-
	(   General = []
	->  true
	;   putHeader('General', 'General Information:'),
	    (	member((descr, Description), General)
	    ->	writef('<p>%t</p>', [Description]) %there is a description
	    ;	true),
	    (	member((author,_), General)
	    ->	putSubHeader('Authored by:'), %there is an author
		writeHtmlList(General, author, authors)
	    ;	true),
	    (	member((date, Date), General)
	    ->	putSubHeader('Date:'), %there is a date
		writef('<p>%t</p>', [Date])
	    ;	true) ).

/*
  putHeader(Name,Title)
      Write a header of a section, giving it the anchor Name and title Title
*/
putHeader(Name, Title):-
	writeln('<a name="'), write(Name), write('"></a>'),
	write('<h2>'),
	write(Title),
	writeln('</h2>').

/*
  putSubHeader(Title)
       Write a subheading with title Title
*/
putSubHeader(Title):-
	write('<h3>'),
	write(Title),
	writeln('</h3>').

/*
  putSubSubHeader(Title)
       Write a subsubheading with title Title
*/
putSubSubHeader(Title):-
	write('<h4>'),
	write(Title),
	writeln('</h4>').

/*
  generateConsults(ConsultsList)
     Generate the list of files consulted, with a link to
     their HTML documentation
*/
generateConsults(ConsultsList, Current, AllFiles):-
	(   ConsultsList = []
	->  true
	;   putHeader('Consulted', 'Files Consulted:'),
	    writeln('<ul id="consults">'),
	    putConsultsList(ConsultsList, Current, AllFiles),
	    writeln('</ul>') ).

findConsultedFile(Current, Consulted, Files, Result) :-
	filePath(Current, CurrentPath),
	absolute_file_name(Consulted, [relative_to(CurrentPath)], ConsultedPath),
	findConsultedFile(ConsultedPath, Files, Result).

findConsultedFile(ConsultedPath, [File|T], Result) :-
	filePath(File, Path),
	(   (   Path = ConsultedPath
	    ;	fileExt(File, Ext),
		file_name_extension(ConsultedPath, Ext, Path) )
	->  Result = File
	;   findConsultedFile(ConsultedPath, T, Result) ).

/*
   putConsultsList(Consults)
     Used by generateConsults/2 to generate the list of files 
     consulted, with a link to their HTML documentation
*/
putConsultsList([],_,_).
putConsultsList([Consulted|L], Current, All):-
	write('<li><p>'),
	(   findConsultedFile(Current, Consulted, All, CF)
	->  fileHTML(CF, HTML),
	    fileHuman(CF, Human),
	    writeLink(HTML, Human)
	;   write(Consulted) ),
	writeln('</p></li>'),
	putConsultsList(L, Current, All).

/*
    generateModules(Libraries)
        Write the list of Libraries consulted
*/
generateModules(ModuleList):-
	(   ModuleList = []
	->  true
	;   putHeader('Modules', 'Uses libraries:'),
	    writeHtmlList(ModuleList,'modules') ).

/*
    generateSideEffects(SideEffects)
         Write the list of SideEffects in the file
*/
generateSideEffects(SideEffectsList):-
	(   SideEffectsList = []
	->  true
	;   putHeader('SideEffects', 'Side Effects:'),
	    writeHtmlList(SideEffectsList,'side-effects') ).

/*
    generatePredicates(DetailsList) 
        Writes the HTML code for the predicates section.
*/
generatePredicates(DetailsList, Links):-
	(   DetailsList = []
	->  Links = []
	;   putHeader('Predicates', 'Predicates:'),
	    writeln('<ul id="predicates_file">'),
	    putPredicatesList(DetailsList,Links,'',0),
	    writeln('</ul>'),
	    putHeader('Details', 'Predicate Details:'),
	    writeln('<ul id="details">'),
	    putDetailsList(DetailsList, Links),
	    writeln('</ul>') ).

/*
  putPredicatesList(DetailsList) 
     Writes the predicates list.
*/
putPredicatesList([], [], _, _).
putPredicatesList([(NameArity,D)|L], [(Pred,Link)|L1], PrevPred, Index):-
	hiphenatedPredicateName(NameArity, HiphPred),
	predicateName(NameArity,TruePred),
	(   TruePred=PrevPred,
	    member((form,Form),D)
	->  Pred=Form		%use form instead of name/arity
	;   (   TruePred\=PrevPred,
		L = [(NextNameArity, _)|_],
		predicateName(NextNameArity,NextPred),
		TruePred=NextPred,
		member((form,Form),D)
	    ->	Pred=Form	%use form instead of name/arity
	    ;	Pred=TruePred) ), %use name/arity
	atom_concat(HiphPred, '-', PartLink),
	atom_concat(PartLink, Index, Link),
	NextIndex is Index+1,
	write('<li><p>'),
	writeAnchor(Link,Pred),
	write('</p></li>'),	
	putPredicatesList(L, L1, TruePred, NextIndex).


/* 
   putDetailsList(DetailsList) 
      Writes the predicates details list.
*/
putDetailsList([],[]).
putDetailsList([Detail|L],[Link|L1]):-
	write('<li>'),
	putDetail(Detail,Link),
	write('</li>'),	
	putDetailsList(L,L1).

/*
     putDetail(Detail) 
        Writes the details for a given predicate
*/

putDetail((NameArity, L), (_, Link)) :-
	putName(NameArity, Link),
	putForm(L),
	putConstraints(L),
	putDescription(L).

/*
    putName(NameArity,Link) 
      Writes the name of the predicate and the supplied relative link.
*/
putName(NameArity, Link) :-
	predicateName(NameArity,Pred),	
	writef('<h3><a name="%t"></a>', [Link]),
	write(Pred),
	writeln('</h3>').

/*
    putForm(List) 
      Writes the form of the predicate, specified in List.
*/
putForm([]).
putForm([H|L]) :-
	(   H = (form, Form)
	->  putSubSubHeader('Form: '),
	    writef('\n<p>%t</p>\n', [Form])
	;   putForm(L) ).

/*
    putConstraints(List) 
       Writes the constraints of the predicate, specified in List.
*/
putConstraints([]).
putConstraints([H|L]):-
	(   H = (constraints, Constraints)
	->  putSubSubHeader('Constraints: '),
	    write('\n<ul>'),
	    putConstraint(Constraints),
	    write('\n</ul>\n')
	;   true ),
	putConstraints(L).

/*
    putConstraint(Constraints) 
       Writes a set of constraints, according to type.
*/

putConstraint([]).
putConstraint([(Type, Value)|L]) :-
	capitaliseAtom(Type, CType),
	writef('<li><p>%t: %t</p></li>', [CType, Value]),
	putConstraint(L).


capitaliseAtom(A,C) :-
	(   A = ''
	->  C = '' 
	;   atom_chars(A, [H|L1]),
	    upcase_atom(H, CH),
	    atom_chars(C, [CH|L1]) ).

/*
    putDescription(List) 
       Writes the description of the predicate, specified in List.
*/
putDescription([]).
putDescription([Type|L]):-
	(   Type = (descr, Descr)
	->  putSubSubHeader('Description: '),
	    writef('\n<p>%t</p>', [Descr])
	;   putDescription(L) ).


/*
   writeLink(Link, Name)
      Write a link with address Link and name Name
*/
writeLink(Link,Name):-
	writef('<a href="%t">%t</a>', [Link, Name]).

/*
   writeAnchor(Link, Name)
      Write an anchor link with address Link and name Name
*/
writeAnchor(Link,Name):-
	atom_concat('#', Link, AnchorLink),
	writeLink(AnchorLink,Name).

/*
   writeHtmlList(List)
      Write List as an HTML list
*/
writeHtmlList(List,Id):-
	(   Id=''
	->  write('<ul>')
	;   writef('<ul id="%t">', [Id]) ),
	writeHtmlList1(List,Id),
	writeln('</ul>').
		  
writeHtmlList1([],_).
writeHtmlList1([V|Vs],Id):-
	writef('<li><p>%t</p></li>',[V]),
	writeHtmlList1(Vs,Id).

/*
   writeHtmlList(List,Parameter)
      Write List as an HTML list, specifying only those items with 
      parameter Parameter
*/
writeHtmlList(List, Parameter, Id):-
	(   Id = ''
	->  write('<ul>')
	;   writef('<ul id="%t">', [Id] ) ),	
	writeHtmlList1(List, Parameter, Id),
	writeln('</ul>').

writeHtmlList1([],_,_).
writeHtmlList1([H|Vs], Parameter, Id) :-
	(   H = (Parameter, V)
	->  writef('<li><p>%t</p></li>', [V])
	;   true ),
	writeHtmlList1(Vs,Parameter,Id).

/*
   firstLetter(Word, Letter)
      If Letter is the capitalised version of the first letter of Word     
*/
firstLetter(Word,Letter):-
	atom_chars(Word, [L|_]),
	upcase_atom(L, Letter).

/*
    predicateName(PredName, Arity, Name)
       If Name is the Predicate name defined according to Name and Arity
*/
predicateName((PredName, Arity), Name):-
	concat_atom([PredName, '/', Arity], Name).

/*
    hiphenatedPredicateName(PredName, Arity, Name)
       If Name is the Predicate name defined according to Name and Arity, but with a hiphen (no slash)
*/
hiphenatedPredicateName((PredName, Arity), Name):-
	concat_atom([PredName, '-', Arity], Name).

