%%%
% Proiect: Sistem expert cu inlantuire inapoi in Prolog
% Student: Grama Gabriel Claudiu 
% An: 	   1 master

%%%%%%%%%%%%%%%%%%%%%
%%% Descriere Succinta
%
% Proiectul e conceput sa afle, 
%   pe baza datelor date de utilizator,
%   ce animal descrie utilizatorul.
% 
% Raspunsurile sunt valori ale atributelor,
%   care pot fi si de tip "da" sau "nu" sau ceva mai concret (ex: "blue")

%%%%%%%%%%%%%%%%%%%%%
%%% Cum pornim sistemul expert? 
% R:
%    top_goal.

%%%%%%%%%%%%%%%%%%%%%
%%% Cum raspundem intrebarilor sistemului expert ?
% R:
%    Odata cu intrebarile, primim si optiunile de raspuns.
%    Raspunsurile ce pot avea valori multiple (ex: culoarea) 
%     se pot da ca lista ( ex: [white, brown, black] )

%%%%%%%%%%%%%%%%%%%%%
%%% Detalii reguli
%
% saveInformation/2 ------- pentru memorarea raspunsurilor utilizatorului,
% 							  deci nu va fi nevoit sa se repete
%
% menuAsk/2 --------------- pentru a intreba utilizatorul
%
% has/2 ------------------- pentru a verifica daca animalul este 
%							  caracterizat de perechea Atribut:Valoare.
%							  daca nu, atunci va apela menuAsk/2
%
% menuInputValidation/3 --- pentru validarea inputului:
%								- de exmplu nu putem introduce valori care
%									nu apar la optiuni
%								- nu putem introduce "no" si alte valori concrete
%									in acelasi timp ( nu are sens )
%						    daca inputul nu e valid va apela menuAsk/2
%
% processInput/2  --------- pentru procesarea inputului:
%								- daca raspunsul nu este o lista, atunci 
%								    va fi transformata intr-una
%								- daca raspunsul este "n" sau "y", atunci
%									va fi transformat in "no" respectiv "yes"

%%%%%%%%%%%%%%%%%%%%%
%%% Alte detalii
%
% Regula "saveInformation" este folosita pentru memorarea raspunsurilor,
%   deci nu va fi nevoit utilizatorul sa se repete
% 
% Un exemplu de atribut este "feet" cu valoare "yes", 
%   aceasta insemnand ca animalul are picioare
% 
% Sunt alte atribute, care pot avea si alte tipuri de raspuns
%   (ex: o culoare concreta), aceste atribute nu pot avea ca raspuns
%   un "yes", deoarece daca i se da o culoare concreta, atunci
%   se subantelege ca are culoare, dar se poate raspunde cu "no".
%
% Pentru a afla in cod ce animale sunt si 
%    cum sunt descrise ele, avem regula animal/1

%%%%%%%%%%%%%%%%%%%%%
%%% Exemplu date de intrare, iesire
%
% iesire:
% Type the animal's eggs value! 
% Options: yes no y n.
%
% intrare: y
% 
% iesire: 
% Type the animal's feet value! 
% Options: yes no y n.
%
% intrare: yes
% 
% iesire:
% Type the animal's wings value! 
% Options: yes no y n.
% 
% intrare: y
% 
% iesire:
% Type the animal's neck value! 
% Options: yes no y n.
% 
% intrare: y
% 
% Type the animal's neck value! 
% Options: yes no y n.
% 
% intrare: y
% 
% iesire:
% Type the animal's limbs value! 
% ptions: yes no y n.
% 
% intrare: y
% 
% iesire:
% Type the animal's feathers value! 
% Options: yes no y n.
% 
% intrare: y
% 
% iesire:
% Type the animal's color value! 
% Options: n no black white brown blue green yellow red.
% 
% intrare: [brown, white]
% 
% iesire: 
% Type the animal's size value! 
% Options: tiny small medium big large huge.
% 
% intrare: small
% 
% iesire:
% RESULT: chicken
% The animal is 'chicken' !
% true
% 



% ========================================================================

:-dynamic(known/2).

top_goal:-
    animal( Animal ),
    nl, write("RESULT: "), write( Animal ), nl,
    write(" The animal is '"), write( Animal ), write("' !"), nl,
    !.
top_goal:-
    nl, write("RESULT: unknown"), nl,
    write("There is no known animal with the the given attribute values!"), nl,
    !, fail.

animal_type(bird):-
    eggs(yes),
    feet(yes),
	wings(yes),
    neck(yes),
    beak(yes),
    limbs(yes),
    feathers(yes).
animal_type(fish):-
    eggs(yes),
    fin(yes).
animal_type(mammal):-
    milk(yes).
animal_type(reptile):-
    limbs(yes),
    eggs(yes).
    
animal(goose):-
    animal_type(bird),
    limbs(yes),
    color(white),
    size(medium).
animal(chicken):-
    animal_type(bird),
	limbs(yes),
    color(brown),
    color(white),
    countValues( color, 2 ), % ca sa aiba strict doar cele 2 culori specifice
    size(small).
animal(parrot):-
    animal_type(bird),
    limbs(yes),
    color(green),
    size(tiny).
animal(crocodile):-
    animal_type(reptile),
    teeth(yes),
    limbs(yes),
    color(green),
    size(big).
animal(lizard):-
    limbs(yes),
    animal_type(reptile),
    color(green),
    size(tiny).
animal(whale):-
    animal_type(mammal),
    teeth(yes),
    fin(yes),
    teeth(yes),
    size(huge),
    ( 
      color(black);
      color(blue);
      color(white)
    ). % poate avea oricare din cele 3 culori
animal(antEater):-
    animal_type(mammal),
    hair(yes),
    limbs(yes),
    tongue(long),
    size(medium),
    color(brown).

size( Value ):- has(size, Value).
eggs( Value ):- has(eggs, Value).
feet( Value ):- has(feet, Value).
fin( Value ):- has(fin, Value).
neck( Value ):- has(neck, Value).
beak( Value ):- has(beak, Value).
hair( Value ):- has(hair, Value).
milk( Value ):- has(milk, Value).
wings( Value ):- has(wings, Value).
teeth( Value ):- has(teeth, Value).
color( Value ):- has(color, Value).
limbs( Value ):- has(limbs, Value).
tongue( Value ):- has(tongue, Value).
feathers( Value ):- has(feathers, Value).

% has( Attribute, Value ).
has(Attribute, Value):-
    known(Attribute, ListValues),
    contains(Value, ListValues),
    !.
has(Attribute, Value):-
	\+ known( Attribute, _ ),
    menuAsk( Attribute, _ ),
    has(Attribute, Value),
    !.

countValues( Attribute, Count ):-
    known(Attribute, ListValues),
    length(ListValues, Count),
    !.

saveInformation( _, [] ):-
    !,fail.
saveInformation( Attribute, Value ):-
    \+ isList(Value),
    assertz( known( Attribute, [Value] ) ),
    !.
saveInformation( Attribute, ListValues ):-
    assertz( known( Attribute, ListValues ) ).

% menuAsk( Attribute, InputListValues )
menuAsk( Attribute, InputValues ):-
    nl, write( "Type the animal's " ), 
    write( Attribute ),
    write( " value! " ),
    displayMenu( Attribute ), nl,
	read( Input ),
    processInput( Input, ProcessedInput ),
    menuInputValidation( Attribute, ProcessedInput, InputValues ),
    saveInformation( Attribute, InputValues ).

multivaluedInput( [_,_|_] ).
    
inputMap( y, yes ):-!.
inputMap( n, no ):-!.
inputMap( RawInput, RawInput ):-!.

processInput( H1, [H2] ):-
    \+ isList(H1),
    inputMap( H1, H2 ),
    !.
processInput( [H1], [H2] ):-
    inputMap( H1, H2 ), 
    !.
processInput( [H1|T1], [H2|T2] ):-
    inputMap( H1, H2 ),
    processInput( T1, T2 ),
    !.

% menuInputValidation( Attribute, Input, ValidInput_ListValues )
menuInputValidation( Attribute, Input, ValidInput ):-
	isList( Input ),
    contains( no, Input ),
    multivaluedInput( Input ),
    nl, write("Invalid input, you cannot type no and multiple answers at the same time !"), nl,
	menuAsk( Attribute, ValidInput ),
    !.
menuInputValidation( Attribute, Input, ValidInput ):-
	multivaluedInput( Input ),
    \+ multivalued( Attribute ),
    nl, write("Invalid input, too many values !"), nl,
	menuAsk( Attribute, ValidInput ),
    !.
menuInputValidation( Attribute, Input, Input ):-
    menuList( Attribute, MenuList ),
    contains( Input, MenuList ),
    !.
menuInputValidation( Attribute, _, Value ):-
    nl, write("Invalid input !"), nl,
	menuAsk( Attribute, Value ),
    !.

multivalued(color).

yesNoAnswers( [yes, no, y, n] ).

% menuList( Attribute, MenuList ).
menuList( yes_no, List ):- yesNoAnswers( List ).
menuList( fin, List ):- yesNoAnswers( List ).
menuList( eggs, List ):- yesNoAnswers( List ).
menuList( feet, List ):- yesNoAnswers( List ).
menuList( neck, List ):- yesNoAnswers( List ).
menuList( beak, List ):- yesNoAnswers( List ).
menuList( hair, List ):- yesNoAnswers( List ).
menuList( milk, List ):- yesNoAnswers( List ).
menuList( wings, List ):- yesNoAnswers( List ).
menuList( limbs, List ):- yesNoAnswers( List ).
menuList( teeth, List ):- yesNoAnswers( List ).
menuList( feathers, List ):- yesNoAnswers( List ).
menuList( size,   [tiny, small, medium, big, large, huge ] ).
menuList( tongue, [n, no, long, normal] ).
menuList( color,  [ n, no, black, white, brown, blue, green, yellow, red] ).

% known(Attribute, ListValues).
known(_,_):-fail. % must exist

displayMenu( MenuType ):-
    nl, write( "Options:"),
    menuList( MenuType, MenuList ),
    displayMenuList(MenuList).
displayMenuList([]):-
    write(".").
displayMenuList( [H|T] ):-
    write(" "),
    write(H),
    displayMenuList(T).

% ========================================================================

% ======  utils ========

%contains( EL, List )
contains( X, List ):-
	isList( X ),
    containsHandlerList(X, List),
    !.
contains( EL, List ):-
    containsHandlerElement(EL, List).
    
containsHandlerElement( EL, [EL|_] ):-!.
containsHandlerElement( EL, [_|T] ):-
	contains( EL, T).

containsHandlerList( [], _ ):-!.
containsHandlerList( [H|T], List ):-
    containsHandlerElement( H, List ),
    containsHandlerList( T, List ).

isList([_|_]):-!.
isList([]):-!.

breakList( [ Element | _ ], Element ).
breakList( [ _ | Tail ], Element ):-
	breakList( Tail, Element ).
    
% ==== unitary tests ====

unitTest:-
	test_saveInformation,
    nl, write("saveInfo"), nl,
    test_processInput,
    nl, write("processInput"), nl,
    test_has,
	nl, write("has"), nl.
    
test_saveInformation:-
    \+ saveInformation( color, [] ),
    
    saveInformation( color, [blue] ),
    known(color, [blue]),
    countValues(color, 1),
    
    saveInformation( color, [red] ),
    known(color, [red]),
    countValues(color, 1),
    
    saveInformation( color, [yellow, green] ),
    known(color, [yellow, green]),   
    countValues(color, 2),
  	!.

test_processInput:-
    processInput([a,b,c,a,y,yes,n,no,a], [a,b,c,a,y,yes,n,no,a]),
    processInput(a,[a]),
    processInput(y,[yes]),
    processInput(n,[no]),
    processInput(no,[no]),
    processInput(yes,[yes]),
    processInput([a],[a]),
    processInput([y],[yes]),
    processInput([n],[no]),
    processInput([no],[no]),
    processInput([yes],[yes]).

test_has:-
    assertz( known(color, [red,green,blue]) ),
	has(color, red),
	has(color, green),
    has(color, blue).

% ===== draft =====

% only 1 true needed
saveInfo( Attribute, Value ):-
    \+ isList( Value ),
    assertz( known( Attribute, Value ) ),!.
saveInfo( Attribute, Values ):-
    breakList( Values, Value),
    saveInfo( Attribute, Value ).

saveInfo( Attribute, Value ):-
    \+ isList( Value ),
    assertz( known( Attribute, Value ) ),!.
saveInfo( Attribute, [Value] ):-
    saveInfo( Attribute, Value ),!.
saveInfo( Attribute, [Value | List ] ):-
    saveInfo( Attribute, List ),
    saveInfo( Attribute, Value ).

%=========================

