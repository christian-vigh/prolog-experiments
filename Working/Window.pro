

init :-
	mainwindow(ID, [
		alias('MainWindow'),
		title('My application window'), 
		size(maximized),
		attributes(ws_overlapped, ws_visible),
		windowproc(mywindowproc),
		menu(
			popup('&Fichier', 
			   	item('&Nouveau', user( command(filenew) ), 'Alt+N', 'Crée un nouveau fichier'),
			   	item('&Enregistrer sous...', user(default), none, none),
			   	separator,
			   	item('&Quitter', default(close), 'Alt+X', 'Quitte l''application')
			     ),
			
			popup('&Edition',
			   	item('&Option à la con', edition_option, '', 'Option à la con')
			    )
		      )
		  ]).
		    

command(filenew) :-
	write('filenew'), nl.
	
	