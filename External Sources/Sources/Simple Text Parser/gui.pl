/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * In this file, the graphical user interface (GUI) of the parser is defined.
 **/
/* The predicate start creates and opens the window of the application */
start :- 
  /* Create a dialog box */
  wdcreate(user_dialog,`Simple text parser (C) 2002, Dimitri PISSARENKO`,
  235,32,626,661,[ws_sysmenu,ws_caption,dlg_ownedbyprolog]), 
  /* Create the "Exit" button */
  wccreate((user_dialog,102),button,`Exit`,208,592,224,32,
	[ws_child,ws_visible,ws_tabstop,bs_pushbutton]), 
  wccreate((user_dialog,1005),static,
	`Dimitri Pissarenko, University Of Derby in Austria, May 2002`,
	30,40,560,16,[ws_child,ws_visible,ss_left]), 
  wccreate((user_dialog,1004),static,
	`Assignment in Computational Linguistics (COL) Module`,
	30,20,560,16,[ws_child,ws_visible,ss_left]), 
  wccreate((user_dialog,1002),static,`Sentence:`,16,192,64,16,
	[ws_child,ws_visible,ss_left]), 
  /* Create a text field for entering the sentence directly */
  wccreate((user_dialog,800),editor,`Enter your sentence here`,
	96,192,416,64,
	[ws_child,ws_visible,ws_tabstop,ws_border,es_left,es_multiline]), 
  /* Create the "Parse" button */
  wccreate((user_dialog,100),button,`Parse`,528,192,80,64,
	[ws_child,ws_visible,ws_tabstop,bs_pushbutton]), 
  /* Create a list box with all the sentences */
  wccreate((user_dialog,400),listbox,`sentenceList`,96,112,512,64,
	[ws_child,ws_visible,ws_tabstop,ws_border,
	ws_vscroll,lbs_notify,lbs_hasstrings]), 
  wccreate((user_dialog,1000),static,`Sentence list:`,
	16,112,64,16,[ws_child,ws_visible,ss_left]), 
  wccreate((user_dialog,1001),static,
	`Choose a sentence from the sentence list (or enter the sentence directly 
into the "Sentence" text field) and then click "Parse" to generate a parse tree.`,
	16,64,592,32,[ws_child,ws_visible,ss_left]), 
  wccreate((user_dialog,1100),button,`Simple text parser`,
	16,0,592,60,[ws_child,bs_groupbox,ws_visible]), 
  /* Create the parse tree text field */
  wccreate((user_dialog,801),editor,
	`Parse tree`,16,288,592,288,[ws_child,ws_visible,ws_tabstop,ws_border,
	es_left,es_multiline,ws_hscroll,ws_vscroll,es_autohscroll,
	es_autovscroll,es_readonly]), 
  wccreate((user_dialog,1003),static,`Parse tree:`,16,256,80,16,
	[ws_child,ws_visible,ss_left]),
  /* The following commands (wlbxadd) fill the sentence list with entries, */
  /* ie sentences of the story. */
  wlbxadd((user_dialog,400), 0, `War is the continuation of politics.`),
  wlbxadd((user_dialog,400), 1, 
  `In this sense war is politics and war itself is a political action; since 
  ancient times there has never been a war that did not have 
  a political character.`),
  wlbxadd((user_dialog,400), 2, 
  `But war has its own particular characteristics and in this sense it 
  cannot be equated with politics in general.`),
  wlbxadd((user_dialog,400), 3, 
  `War is the continuation of politics by other means.`),
  wlbxadd((user_dialog,400), 4, 
  `When politics develops to a certain stage beyond which it cannot proceed 
  by the usual means, war breaks out to sweep the obstacles from the way.`),
  wlbxadd((user_dialog,400), 5, 
  `When the obstacle is removed and our political aim attained, the war will stop.`),
  wlbxadd((user_dialog,400), 6, 
  `But if the obstacle is not completely swept away, the war will have to 
  continue till the aim is fully accomplished.`),
  wlbxadd((user_dialog,400), 7, 
  `It can therefore be said that politics is war without 
  bloodshed while war is politics with bloodshed.`),
  /* Attach an event handler to the dialog box */
  window_handler('user_dialog', windowHandler),
  /* Open the dialog box */
  wshow('user_dialog',1) .

/* The following predicate is called when the user presses the "Parse"       */
/* button and the entered sentence is parsed properly (no errors occur).	 */
windowHandler((user_dialog,100), msg_button, _, _) :-
	/* Save the phrase entered by the user in variable EnteredString */
	wtext((user_dialog,800), EnteredString),
	/* Convert string EnteredString into a PROLOG list Sentence  */
	r(Sentence) <~ EnteredString,
	/* Parse Sentence and save the results in ParseResult        */
	phrase(s(ParseResult),Sentence),
	/* Generate a "graphical" tree from ParseResult and save it  */
	/* in ParseTree                                              */
	ppt(ParseResult) ~> ParseTree,
	/* Display ParseTree in the parse tree text field            */
	wtext((user_dialog,801), ParseTree).

/* The following predicate is called when the user presses the "Parse"       */
/* button and the entered sentence can not be parsed properly because the 	 */
/* either some word is misspelled or the entered sentence has an unknown     */
/* grammar.                                                                  */
windowHandler((user_dialog,100), msg_button, _, _) :-
    /* Print an error message in the parse tree text field. */
	wtext((user_dialog,801), 
	`Parsing error (misspelled words or an unknown phrase grammar).`).

/* The following predicate is called when the user presses the "Parse"       */
/* button and the entered sentence cannot be parsed because a period (.) at  */
/* the end of the sentence is missing.                                       */
'?ERROR?'( 43, Goal ) :-
    /* Print an error message in the parse tree text field. */
	wtext((user_dialog,801), 
	`EOF (#43) error. Possible cause: missing period at the end of sentence.`),
	abort.

/* The following predicate is called when the user presses the "Exit"        */
/* button.                                                                   */
windowHandler((user_dialog,102), msg_button, _, _) :-
	/* Close the window */
	wclose(user_dialog).

/* The following predicate is called when the user calls the "Close"       */
/* menu item in the system menu or the cross (X) in the title bar.         */
windowHandler(user_dialog, msg_close, _, _) :-
	/* Close the window */
	wclose(user_dialog).

/* The following predicate is called when the user selects a sentence from */
/* the sentence list by clicking on the desired item                       */
windowHandler((user_dialog,400), msg_select, _, _) :- 
  /* Get the list of all the selected items and store this list in the     */
  /* variable SelectedItems.                                               */
  get_lbx_selection((user_dialog,400), SelectedItems),
  /* Take the first item from the list of selected items and store it in   */
  /* the variable FirstSelectedItem.                                       */
  head(SelectedItems, FirstSelectedItem),
  /* Print the selected item (sentence) in the text field for entering the */
  /* sentence to parse.                                                    */
  wtext((user_dialog,800), FirstSelectedItem).

/* This predicate determines the first element of the list (first argument)*/
/* and stores this first element in the second argument                    */
head([H|T], H).

/* The predicates get_lbx_selection/2, get_lbx_selection/5, get_selected/5 */
/* were all taken from the example files bundled with LPA WIN-PROLOG 3.300 */
/* These predicates and the documentation can be found in the file         */
/* eg_lbx.pl in the directory examples                                     */

/************************************************************************
** given a listbox return a list of its currently selected items
************************************************************************/
% get the next item after 0 (the first item) in the given listbox,
% start checking the items in the listbox starting at the first item,
% giving the next item and the empty list as the list of selections so
% far and return the result

get_lbx_selection( Lbx, Selections ) :-
  wlbxfnd( Lbx, 0, ``, NextItem ),
  get_lbx_selection( Lbx, 0, NextItem, [], Selections ).

/************************************************************************
** build a list of the current selections in the listbox
************************************************************************/

% check the selection state of the given item in the given listbox
% if it is selected add it to the list of selections found so far.
% depending on the value of the next item either finish or find the item
% following and continue checking from the next item.

% if the next item is 0, we are back at the start of the listbox
% so check the selection of the current item and finish

get_lbx_selection( Lbx, Item, 0, SoFar, Sels ) :-
  wlbxsel( Lbx, Item, Sel ),
  get_selected( Sel, Lbx, Item, SoFar, Sels ).

% if the next item is not 0 check the selection of the current item
% find the item after next and continue checking from the next item

get_lbx_selection( Lbx, Item, NextItem, SoFar, Sels ) :-
  wlbxsel( Lbx, Item, Sel ),
  get_selected( Sel, Lbx, Item, SoFar, NS ),
  wlbxfnd( Lbx, NextItem, ``, AfterNextItem ),
  get_lbx_selection( Lbx, NextItem, AfterNextItem, NS, Sels ).

/************************************************************************
** add a selected menu item to a list
************************************************************************/

% if the selection state is 0, the item is not selected
% so the list of selected items remains unchanged.

get_selected( 0, _,      _,   SoFar, SoFar ).

% if the selection state is 1, the item is selected
% so add the item's string to the list of selected items

get_selected( 1, Lbx, Item, SoFar, [ItemStr|SoFar]  ) :-
  wlbxget( Lbx, Item, ItemStr ).
