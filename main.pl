:-dynamic(known/2).

start:-
    %init,
    top_goal(X),
    write("The animal is: "),
    write(X), nl.

% not used
% init:-
%   abolish(known, 2),
%  	define(known, 2).

top_goal( Animal ):-
    animal( Animal ).
    
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
    hair(yes),
    limbs(yes),
    milk(yes).
animal_type(reptile):-
    limbs(yes),
    eggs(yes).
    
animal(goose):-
    animal_type(bird),
    color(white),
    size(medium).
animal(chicken):-
    animal_type(bird),
    color(brown),
    color(white),
    countValues( color, 2 ),
    size(small).
animal(parrot):-
    animal_type(bird),
    color(green),
    size(tiny).
animal(crocodle):-
    animal_type(reptile),
    teeth(yes),
    color(green),
    size(big).
animal(lizard):-
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
    ).
animal(antEater):-
    animal_type(mammal),
    tongue(long),
    size(medium),
    color(brown).

size( Value ):- check(size, Value).
eggs( Value ):- check(eggs, Value).
feet( Value ):- check(feet, Value).
fin( Value ):- check(fin, Value).
neck( Value ):- check(neck, Value).
beak( Value ):- check(beak, Value).
hair( Value ):- check(hair, Value).
milk( Value ):- check(milk, Value).
wings( Value ):- check(wings, Value).
teeth( Value ):- check(teeth, Value).
color( Value ):- check(color, Value).
limbs( Value ):- check(limbs, Value).
tongue( Value ):- check(tongue, Value).
feathers( Value ):- check(feathers, Value).

countValues( Attribute, Count ):-
    findall(Value, menuAsk(Attribute, Value), L),
    length(L, Count),
    !.

%TODO menuAsk( color, _), known(color, Value). creeaza dubluri

info(Attribute, Value):-
    known( Attribute, Value).
info(Attribute, Value):-
    \+ known(_,_),
	menuAsk(Attribute, Value).

% menuAsk( Attribute, Value )
menuAsk( Attribute, InputValue ):-
    write( "Type the animal's " ), 
    write( Attribute ),
    write( " value. " ),
    displayMenu( Attribute ),
    read( Input ),
    processInput( Input, ProcessedInput ),
    menuInputValidation( Attribute, ProcessedInput, InputValue ),
    saveInformation( Attribute, InputValue ).


menuAsk_YesNo( Answer ):-
    displayMenu( yes_no ),
    read( RawInput ),
    yesNoValidation( RawInput, Answer ).

% menuInputValidation( Attribute, Input, ValidInput )
menuInputValidation( Attribute, Input, Value ):-
	isList( Input ),
    contains( no, Input ),
    multivaluedInput( Input ),
    write("Invalid input, you cannot type no and multiple answers at the same time !"), nl,
	menuAsk( Attribute, Value ),
    !.
menuInputValidation( Attribute, Input, Value ):-
	multivaluedInput( Input ),
    \+ multivalued( Attribute ),
    write("Invalid input, too many values !"), nl,
	menuAsk( Attribute, Value ),
    !.
menuInputValidation( Attribute, Input, Input ):-
    menuList( Attribute, MenuList ),
    contains( Input, MenuList ),
    !.
menuInputValidation( Attribute, _, Value ):-
    write("Invalid input !"), nl,
	menuAsk( Attribute, Value ).

multivaluedInput( [_,_|_] ).

yesNoValidation( X, X ):-
    yesNoAnswers( List ),
    contains( X, List ),
    !.
yesNoValidation( _, X ):-
    write("Invalid input !"), nl,
    menuAsk_YesNo( X ). 
    

processInputHandler( y, yes ):-!.
processInputHandler( n, no ):-!.
processInputHandler( RawInputValue, RawInputValue ).

processInput( [RawInput|_], Input ):-
    processInputHandler( RawInput, Input ).
processInput( [_|RawInput_T], Input ):-
    processInput( RawInput_T, Input ).

processInput( RawInputValue, InputValue ):-
    \+ isList(RawInputValue),
	processInputHandler( RawInputValue, InputValue ).

saveInformation( Attribute, Value ):-
    known(Attribute, Value),
    !.
saveInformation( Attribute, Value ):-
	assertz( known( Attribute, Value ) ).

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
menuList( color,  [ n, no, black, white, brown, blue, green, yellow] ).

% known( YES_or_NO, Attr).
known(_,_):-fail. % must exist

displayMenu( MenuType ):-
    write( "Possible answers:"),
    menuList( MenuType, MenuList ),
    displayMenuList(MenuList).
displayMenuList([]):-
    write(".").
displayMenuList( [H|T] ):-
    write(" "),
    write(H),
    displayMenuList(T).



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






