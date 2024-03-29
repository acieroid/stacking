%% TODO:
%% Constraints
%%   - Cannot put a heavier item on top of a lighter one
%%
%% Improve results:
%%   ✔ change the pick policy (eg. biggest first): it would probably
%%   improve a bit the result, but is not very flexible
%%   OR
%%   - generate the tree and explore it in a best-first search way,
%%   which would be more flexible
%% (Both could be made in order to compare the results of the two approaches).
%%
%% Extensions:
%%  - Rotate objects
%%  - Try 3D
%%  - Improve output
%%  - Generate an image for the visualization

%% strategy(-Strategy)
% Defines the strategy to use to compute the stackings.
strategy(basic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                     Container representation                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% container_size(+Container, -Size)
% Specifies the size of the container (10x10x1 for each container in
% the original assignment)
container_size(1, size(10, 10, 1)).
container_size(2, size(10, 10, 1)).

%% is_at(?Container, ?Pos, ?Obj)
% Dynamic predicate that associates an object (functor object(Id,
% Size), as defined in the data sets) to a position (functor
% position(X, Y, Z)) inside a container (number). The position
% indicates the bottom left point of the box
:- dynamic is_at/3.

%% clear_containers
% Remove all the boxes from a container
clear_containers :-
    retractall(is_at(_, _, _)).

%% evaluate(+Method, -Score)
% Evaluate the current configuration, based on a certain method:
%   - count: maximize the number of objects placed in the containers
%TODO.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                          Object representation                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% volume(+Object, -Volume)
% Compute the volume of an object
weight(object(_, size(W, H, D)), Volume) :-
    Volume is W*H*D.

%% weight(+Object, -Weight)
% Compute the weight of an object (which is proportional to the volume)
weight(Object, Weight) :-
    volume(Object, Weight).

%% heavier(-Pred, +Obj1, +Obj2)
% Predicate to test if an object is heavier than another
heavier(Pred, Obj1, Obj2) :-
    weight(Obj1, N1),
    weight(Obj2, N2),
    compare(Pred, N1, N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            Display functions                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% display_line(+Size)
% Display a line of underscores of size Size
display_line(0) :- !.
display_line(N) :-
    N > 0,
    N1 is N-1,
    write('_'),
    display_line(N1).

%% display_container(+Container)
% Nicely displays a container, one level of depth at a time (Z=1, then
% Z=2, etc.)
display_container(Container) :-
    container_size(Container, size(W, H, D)),
    % red cut, since it could cut branches if multiple sizes are
    % defined for the same container, but since it should not be the
    % case, that's fine
    write('|'),
    display_container(Container, position(1, H, 1), size(W, H, D)),
    nl, nl.

display_container(Container, position(W, 1, D), size(W, _, D)) :-
    display_pos(Container, position(W, 1, D)),
    write('|'), nl,
    W2 is W*2-1,
    write('+'), display_line(W2), write('+'), nl, !.
display_container(Container, position(W, 1, Z), size(W, H, D)) :-
    display_pos(Container, position(W, 1, Z)),
    write('|'), nl,
    W2 is W*2-1,
    write('+'), display_line(W2), write('+'),
    nl, nl,
    Z1 is Z+1,
    display_container(Container, position(1, H, Z1), size(W, H, D)), !.
display_container(Container, position(W, Y, Z), size(W, H, D)) :-
    display_pos(Container, position(W, Y, Z)),
    write('|'), nl, write('|'),
    Y1 is Y-1,
    display_container(Container, position(1, Y1, Z), size(W, H, D)), !.
display_container(Container, position(X, Y, Z), size(W, H, D)) :-
    X =< W, Y =< H, Z =< D,
    display_pos(Container, position(X, Y, Z)),
    tab(1),
    X1 is X+1,
    display_container(Container, position(X1, Y, Z), size(W, H, D)), !.

%% display_pos(+Container, +Pos)
% Display what is contained at the given position in the given container
display_pos(Container, position(X, Y, Z)) :-
    % red cut, since if a position is occupied multiple times it will
    % cut successful branches, but it should never be the case
    occupied_by(Container, position(X, Y, Z), Id), !,
    write(Id).
display_pos(_, _) :-
    write(' ').

%% display_containers
% Display all the containers
% TODO: don't depend on the fact that there is two containers
% TODO: display containers side by side (more compact, more intuitive).
display_containers :-
    write('Container 1:'), nl,
    display_container(1),
    write('Container 2:'), nl,
    display_container(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Placement logic                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Is_inside(+Container, +Pos)
% verify if a position is correctly inside a container
is_inside(Container, position(X, Y, Z)) :-
    container_size(Container, size(SX, SY, SZ)),
    between(1, SX, X),
    between(1, SY, Y),
    between(1, SZ, Z).

%% occupied_by(+Container, +Pos, -Id)
% Return the object that occupies a certain position in a container.
occupied_by(Container, position(X, Y, Z), Id) :-
    is_at(Container, position(OX, OY, OZ), object(Id, size(SX, SY, SZ))),
    EX is OX+SX-1, EY is OY+SY-1, EZ is OZ+SZ-1,
    between(OX, EX, X),
    between(OY, EY, Y),
    between(OZ, EZ, Z).

%% has_base(+Container, +Pos)
% Verify if the position has something beneath it, to serve as a
% base
has_base(Container, position(X, Y, Z)) :-
    Y > 1,
    Y1 is Y-1,
    is_occupied(Container, position(X, Y1, Z)).
has_base(_, position(_, 1, _)).

%% is_occupied(+Container, +Pos)
% Verify if a position of a container is occupied
is_occupied(Container, Pos) :-
    occupied_by(Container, Pos, _).

%% is_free(+Container, +Pos)
% Verify if a position of a container is free
is_free(Container, Pos) :-
    not(is_occupied(Container, Pos)).

%% is_valid(+Container, +Pos)
% Verify if a position can be occupied by a new object
is_valid(Container, Pos) :-
    is_inside(Container, Pos),
    is_free(Container, Pos).

%% bases(+Object, +Pos, -Positions)
% Bind Positions to the positions of the base of the object (its
% bottom layer)
bases(object(_, size(SX, _, SZ)), position(X, Y, Z), Positions) :-
    EX is X+SX-1, EZ is Z+SZ-1,
    findall(Pos, in_square(position(X, Y, Z), position(EX, Y, EZ), Pos),
            Positions).

%% is_legal(+Container, +Object, +Pos)
% Check if it is legal to put an object at a given position in a
% container
is_legal(Container, object(Id, size(SX, SY, SZ)), position(X, Y, Z)) :-
    EX is X+SX-1, EY is Y+SY-1, EZ is Z+SZ-1,
    findall(Pos, in_square(position(X, Y, Z), position(EX, EY, EZ), Pos),
            Positions),
    exclude(is_valid(Container), Positions, Invalid),
    length(Positions, N),
    N > 0,
    length(Invalid, 0),
    bases(object(Id, size(SX, SY, SZ)), position(X, Y, Z), BasePositions),
    include(has_base(Container), BasePositions, ValidBases),
    length(BasePositions, N1),
    length(ValidBases, N1).

%% place(+Container, +Pos, +Object)
% Place an object at a certain position in a container. Check if the
% position is legal (ie. the object will fit into the container), but
% do not check if there is something else already there or if it
% possible to place it there from the top.
place(Container, position(X, Y, Z), object(Id, size(SX, SY, SZ))) :-
    is_inside(Container, position(X, Y, Z)),
    X1 is X+SX-1, Y1 is Y+SY-1, Z1 is Z+SZ-1,
    is_inside(Container, position(X1, Y1, Z1)),
    assert(is_at(Container, position(X, Y, Z),
                 object(Id, size(SX, SY, SZ)))).
place(Container, Pos, Object) :-
    % This will retract the asserted predicates when backtracking
    % http://awarth.blogspot.be/2008/08/asserts-and-retracts-with-automatic.html
    retract(is_at(Container, Pos, Object)),
    fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               Main loop                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% pick(+List, -Element)
% Pick an element from a list
pick([H|_], H).
pick([_|T], Element) :-
    pick(T, Element).

%% pick_and_remove(+List, -Element, -NewList)
% Pick an element from a list and removes it
pick_and_remove([H|T], T, H).
pick_and_remove([H|T], [H|T1], Object) :-
    pick_and_remove(T, T1, Object).

%% in_square(+BottomLeft, +TopRight, ?Position)
% Bind Position to a position in the square described by BottomLeft
% and TopRight. Can be used to check that Position is inside this
% square too.
in_square(position(X1, Y1, Z1), position(X2, Y2, Z2), position(X, Y, Z)) :-
    between(X1, X2, X),
    between(Y1, Y2, Y),
    between(Z1, Z2, Z).

%% possible_positions(+Container, +Object, -Positions)
% Bind Positions to all the possible (valid) positions that can take
% Object in Container.
possible_positions(Container, Object, Positions) :-
    container_size(Container, size(SX, SY, SZ)),
    findall(Pos, in_square(position(1, 1, 1), position(SX, SY, SZ), Pos),
            InSquare),
    include(is_legal(Container, Object), InSquare, Positions).

%% stack_basic(+Objects, +Containers, -ObjectsAndPositions)
% Stack some objects into the given containers. Try to stack the
% elements of Objects in the given order
stack_basic(_, _, []).
stack_basic(Objects, Containers, [[Object, Container, Position]|Res]) :-
    pick(Containers, Container),
    pick_and_remove(Objects, NewObjects, Object),
    possible_positions(Container, Object, Positions),
    pick(Positions, Position),
    place(Container, Position, Object),
    stack_basic(NewObjects, Containers, Res).

%% stack_heaviest_first(+Objects, +Containers, -ObjectsAndPositions)
% Like stack_basic, but place the heaviest objects first
stack_heaviest_first(Objects, Containers, ObjectsAndPositions) :-
    predsort(heavier, Objects, SortedObjects),
    stack_basic(SortedObjects, Containers, ObjectsAndPositions).

%% stacking_strategy(+Strategy, -Stacking)
% Find the stacking function to use, given a strategy
stacking_strategy(basic, stack_basic).
stacking_strategy(heaviest_first, stack_heaviest_first).

%% stack(+Objects, +Containers, -ObjectsAndPositions)
% Stack objects according to the strategy defined with strategy/1
stack(Objects, Containers, ObjectsAndPositions) :-
    strategy(Strategy),
    stacking_strategy(Strategy, Stacking),
    apply(Stacking, [Objects, Containers, ObjectsAndPositions]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 Examples                                 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% stack_n(+Data, +N) :-
% Try to stack n objects contained in Data inside the containers
stack_n(Data, N) :-
    clear_containers,
    length(L, N),
    length(Data, N1),
    N =< N1,
    stack(Data,
          [1, 2],
          L),
    display_containers.

%% stack_all(+Data) :-
% Try to stack all the objects contained in Data inside the containers
stack_all(Data) :-
    length(Data, N),
    stack_n(Data, N).

%% stack_debug1
% Run on a simple debug dataset
stack_debug1 :-
    stack_all([object(1, size(1,1,1))]).

 %% stack_data1
% Run on data1.txt dataset
stack_data1 :-
    stack_n([object(1,size(5,1,1)), object(2,size(5,7,1)),
             object(3,size(2,1,1)), object(4,size(1,1,1)),
             object(5,size(3,2,1)), object(6,size(5,1,1)),
             object(7,size(1,5,1)), object(8,size(4,3,1)),
             object(9,size(4,7,1)), object(10,size(4,2,1)),
             object(11,size(1,6,1)), object(12,size(2,4,1)),
             object(13,size(7,4,1)), object(14,size(6,7,1)),
             object(15,size(4,7,1)), object(16,size(5,3,1)),
             object(17,size(3,5,1)), object(18,size(1,1,1))],
           15).

%% stack_data2
% Run on data2.txt dataset
stack_data2 :-
    stack_n([object(1,size(4,4,1)), object(2,size(7,1,1)),
             object(3,size(5,2,1)), object(4,size(8,1,1)),
             object(5,size(6,5,1)), object(6,size(8,3,1)),
             object(7,size(3,2,1)), object(8,size(1,10,1)),
             object(9,size(4,4,1)), object(10,size(5,6,1)),
             object(11,size(3,9,1)), object(12,size(2,1,1)),
             object(13,size(7,4,1)), object(14,size(3,10,1)),
             object(15,size(1,6,1))],
            11).

%% stack_data3
% Run on data3.txt dataset
stack_data3 :-
    stack_n([object(1,size(2,3,1)), object(2,size(2,1,1)),
             object(3,size(2,2,1)), object(4,size(1,3,1)),
             object(5,size(3,2,1)), object(6,size(2,3,1)),
             object(7,size(3,3,1)), object(8,size(2,3,1)),
             object(9,size(3,1,1)), object(10,size(1,1,1)),
             object(11,size(2,1,1)), object(12,size(1,2,1)),
             object(13,size(3,1,1)), object(14,size(2,3,1)),
             object(15,size(1,2,1)), object(16,size(1,2,1)),
             object(17,size(3,1,1)), object(18,size(2,3,1)),
             object(19,size(3,3,1)), object(20,size(3,3,1)),
             object(21,size(2,1,1)), object(22,size(3,1,1)),
             object(23,size(1,3,1)), object(24,size(2,1,1)),
             object(25,size(1,1,1))],
            25).

%% stack_data4
% Run on data4.txt dataset
stack_data4 :-
    stack_n([object(1,size(4,3,1)), object(2,size(2,2,1)),
             object(3,size(1,4,1)), object(4,size(3,4,1)),
             object(5,size(4,4,1)), object(6,size(1,4,1)),
             object(7,size(1,5,1)), object(8,size(3,2,1)),
             object(9,size(3,2,1)), object(10,size(1,3,1)),
             object(11,size(1,5,1)), object(12,size(1,4,1)),
             object(13,size(3,3,1)), object(14,size(1,5,1)),
             object(15,size(3,2,1)), object(16,size(4,3,1)),
             object(17,size(1,1,1)), object(18,size(2,4,1)),
             object(19,size(2,2,1)), object(20,size(4,2,1)),
             object(21,size(4,4,1)), object(22,size(4,4,1)),
             object(23,size(5,3,1)), object(24,size(3,4,1)),
             object(25,size(1,5,1)), object(26,size(1,2,1))],
            23).

%% stack_data5
% Run on data5.txt dataset
stack_data5 :-
    stack_n([oject(1,size(3,9,1)), object(2,size(6,2,1)),
             object(3,size(3,1,1)), object(4,size(8,4,1)),
             object(5,size(4,5,1)), object(6,size(1,6,1))],
            5).

