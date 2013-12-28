%% Things to do:
%%  - no bigger objects on smaller ones
%%  - see TODOs in the code
%% Possible extensions:
%%  - Rotate objects
%%  - Try 3D
%%  - Improve output
%%  - Visualisation

%% Example run:
%% ?- [data1].
%% ?- [stacking].
%% ?- run(Best)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Main predicate                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% run
% Do a best-first search on the dataset currently loaded and display
% the best the best configuration found, according to eval/2.
run(Best) :-
    objects(Objects),
    containers(Containers),
    empty(Objects, Containers, EmptyWorld),
    best(EmptyWorld, Best),
    display_verbose(Best),
    display(Best).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                         World representation                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For object representation, see the data*.pl files. Objects Ids
% should start from 1 and increase 1 by 1.

%% objects(-Objects)
% Find all the objects
objects(Objects) :-
    findall(Obj, object(Obj, _), Objects).

%% X at Y
% Operator used to identify position of objects
:- op(500, xfx, at).

%% container_size(+Container, -Size)
% Specifies the size of the container (10x10x1 for each container in
% the original assignment)
container_size(1, size(10, 10, 1)).
container_size(2, size(10, 10, 1)).

%% containers(-Containers)
% Return the list of existing containers
containers(Containers) :-
    findall(C, container_size(C, _), Containers).

%% placement_lists(+Containers, -PlacementLists)
% Generate the placement lists. There is one placement list per
% container. Each placement list contains the list of objects that
% have been placed inside the corresponding container, along with the
% position. For example, if Containers = [1, 2], the placement lists
% could be [[1 at pos(1,1,1)] []].
placement_lists([], []).
placement_lists([_|Containers], [[]|Rest]) :-
    placement_lists(Containers, Rest).

%% placement_lists_count(+PlacementLists, -Count)
% Count the number of objects inside all the containers
placement_lists_count([], 0).
placement_lists_count([P|Ps], N) :-
    length(P, N1),
    placement_lists_count(Ps, N2),
    N is N1 + N2.

%% placement_list(+World, +Container, -PlacementList)
% Find the placement list for a container
placement_list(world(_, Containers, PlacementLists), Container,
               PlacementList) :-
    placement_list(PlacementLists, Containers, Container, PlacementList).

placement_list([P|_], [Container|_], Container, P) :-
    !. % red cut, but doesn't cut any success branch for our use
placement_list([_|Ps], [_|Containers], Container, P) :-
    placement_list(Ps, Containers, Container, P).

%% place(+PlacementLists, +Containers, +Container, +Content, -NewPlacementLists)
% Place an object in a container, updating the given placement
% lists. No checks are done.
place([P|Ps], [Container|_], Container, Content, [[Content|P]|Ps]) :-
    !. % red cut, but doesn't cut any success branch for our use
place([P|Ps], [_|Containers], Container, Content, [P|NewPs]) :-
    place(Ps, Containers, Container, Content, NewPs).

%% empty(+Objects, +Containers, -World)
% The empty world (no objects placed inside any container). Objects
% are identified by their id.
% Example: empty([1], [1,2], World)
empty(Objects, Containers, World) :-
    placement_lists(Containers, PlacementLists),
    World = world(Objects, Containers, PlacementLists).

%% filled(+World)
% Check if all the objects are placed.
filled(world([], _, _)).

%% put(World, Object, Container, Position, NewWorld)
% Put an object at a given position in a container. No checks are done
% to see if the object can be put there nor if the object placed
% exists in the world.
put(world(Objects, Containers, PlacementLists),
    Object, Container, Position,
    world(NewObjects, Containers, NewPlacementLists)) :-
    delete(Objects, Object, NewObjects),
    place(PlacementLists, Containers, Container,
          Object at Position, NewPlacementLists).

%% compare_worlds(?Order, +World1, +World2)
% Compare two world using eval
compare_worlds(Order, World1, World2) :-
    eval(World1, Score1),
    eval(World2, Score2),
    compare(Order, Score2, Score1). % smaller is better

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Heuristics                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% strategy(-Strategy)
% Defines the strategy to use to compute the stackings.
strategy(max_objects).

%% eval_strategy(+Strategy, +World, -Score)
% Evaluate the score of a board with the given strategy
eval_strategy(max_objects,
              world(_, _, PlacementLists), Score) :-
    % Maximize the number of objects placed
    placement_lists_count(PlacementLists, Score).
eval_strategy(max_balance, _, _) :-
    % Maximize the balance between the containers, by weight
    % TODO: not implemented yet
    fail.
eval_strategy(max_weight, _, _) :-
    % Maximize the total weight placed
    % TODO: not implemented yet
    fail.
eval_strategy(min_space, _, _) :-
    % Minimize the free spaces
    % TODO: not implemented yet
    fail.

%% eval(+World, -Score)
% Evaluate the score of a board based on the strategy defined by
% strategy/1 (only one strategy can be defined at a time).
eval(World, Score) :-
    strategy(Strategy), !, % only try the first strategy
    eval_strategy(Strategy, World, Score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Object Placement                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% is_inside(+Container, +Pos)
% Verify if a position is correctly inside a container.
is_inside(Container, position(X, Y, Z)) :-
    container_size(Container, size(Width, Height, Depth)),
    between(1, Width, X),
    between(1, Height, Y),
    between(1, Depth, Z).

%% occupied_by(+Container, +Pos, -Id)
% Return the object that occupies a certain position in a
% container. Fail if no object occupies this position.
occupied_by(World, Container, position(X, Y, Z), Id) :-
    placement_list(World, Container, PlacementList),
    member(Id at position(StartX, StartY, StartZ), PlacementList),
    object(Id, size(Width, Height, Depth)),
    EndX is StartX+Width-1, EndY is StartY+Height-1, EndZ is StartZ+Depth-1,
    between(StartX, EndX, X),
    between(StartY, EndY, Y),
    between(StartZ, EndZ, Z),
    !. % red cut, but it's fine since two objects cannot occupy the same spot

%% is_occupied(+World, +Container, +Pos)
% Verify if a position of a container is occupied
is_occupied(World, Container, Pos) :-
    occupied_by(World, Container, Pos, _).

%% is_free(+World, +Container, +Pos)
% Verify if a position of a container is free
is_free(World, Container, Pos) :-
    not(is_occupied(World, Container, Pos)).

%% is_valid(+World, +Container, +Pos)
% Verify if a position can be occupied by a new object
is_valid(World, Container, Pos) :-
    is_inside(Container, Pos),
    is_free(World, Container, Pos).

%% in_square(+BottomLeft, +TopRight, ?Position)
% Bind Position to a position in the square described by BottomLeft
% and TopRight. Can be used to check that Position is inside this
% square too.
in_square(position(X1, Y1, Z1), position(X2, Y2, Z2), position(X, Y, Z)) :-
    between(X1, X2, X),
    between(Y1, Y2, Y),
    between(Z1, Z2, Z).

%% bases(+Object, +Position, -Positions)
% Bind Positions to the positions of the base of the object (its
% bottom layer), given that it is place at Position
bases(Object, position(X, Y, Z), Positions) :-
    object(Object, size(Width, _, Depth)),
    EndX is X+Width-1, EndZ is Z+Depth-1,
    findall(Position,
            in_square(position(X, Y, Z), position(EndX, Y, EndZ), Position),
            Positions).

%% has_base(+World, +Container, +Pos)
% Verify if the position has something beneath it, to serve as a
% base
has_base(_, _, position(_, 1, _)) :-  % the bottom of the container has a base
  !. % might cut some success branch, but not when used correctly
has_base(World, Container, position(X, Y, Z)) :-
    Y > 1,
    % a position has a base if the position beneath it is occupied
    Y1 is Y-1,
    is_occupied(World, Container, position(X, Y1, Z)).

%% is_legal(+Container, +Object, +Pos)
% Check if it is legal to put an object at a given position in a
% container
is_legal(World, Container, Object, position(X, Y, Z)) :-
    object(Object, size(Width, Height, Depth)),
    EndX is X+Width-1, EndY is Y+Height-1, EndZ is Z+Depth-1,
    % Find all the positions that the object will take
    findall(Pos, in_square(position(X, Y, Z), position(EndX, EndY, EndZ), Pos),
            Positions),
    % Filter out the valid position to keep the invalid ones
    exclude(is_valid(World, Container), Positions, Invalid),
    length(Invalid, 0), % should not have invalid positions
    length(Positions, N), % and at least a valid one
    N > 0,
    % Get the base positions of the object
    bases(Object, position(X, Y, Z), BasePositions),
    % Filter out the positions that have a base
    exclude(has_base(World, Container), BasePositions, InvalidBases),
    length(InvalidBases, 0). % should not have any invalid base

%% possible_positions(+World, +Container, +Object, -Positions)
% Bind Positions to all the possible (legal) positions that can take
% Object in Container.
possible_positions(World, Container, Object, Positions) :-
    container_size(Container, size(Width, Height, Depth)),
    % TODO: this kind of findall followed by include/exclude can
    % probably be factored in only one findall
    findall(Pos,
            in_square(position(1, 1, 1), position(Width, Height, Depth), Pos),
            InSquare),
    include(is_legal(World, Container, Object), InSquare, Positions).

%% place_one(+World, -NewWorld)
% Nondeterministically pick an object and put it in some position of a
% container
place_one(World, NewWorld) :-
    World = world(Objects, Containers, _),
    pick(Containers, Container),
    pick(Objects, Object),
    possible_positions(World, Container, Object, Positions),
    pick(Positions, Position),
    put(World, Object, Container, Position, NewWorld).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            Best-First Search                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% children(+World, -Children)
% Compute the children of a world
children(World, Children) :-
    findall(W, place_one(World, W), Children).

%% add_best_first(+Children, +Agenda, -NewAgenda)
% Merge new childrens in agenda
add_best_first(Children, Agenda, NewAgenda) :-
    predmsort(compare_worlds, Children, SortedChildren),
    predmmerge(compare_worlds, SortedChildren, Agenda, NewAgenda).

%% search(+Agenda, -FinalWorld)
% Do a best-first search on the search space. Taken from Simply
% Logical, chapter 6.
search([World|_], World). % no specific condition to stop (yet)
search([World|Rest], FinalWorld) :-
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    search(NewAgenda, FinalWorld).

%% best(+Initial, -Best)
% Do a best-first search and find the best world, according to eval/2.
% TODO: implement a predicate like findall that take the max of an
% increasing sequence of computations.
best(Initial, Best) :-
    eval(Initial, InitialScore),
    children(Initial, Children),
    best(Children, Initial, InitialScore, Best).

best([], Best, _, Best).
best([World|Rest], _CurrentBest, BestScore, Best) :-
    eval(World, ThisScore),
    ThisScore >= BestScore,
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    best(NewAgenda, World, ThisScore, Best).
best([World|_], Best, BestScore, Best) :-
    eval(World, ThisScore),
    ThisScore =< BestScore.

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
display_container(World, Container) :-
    container_size(Container, size(W, H, D)),
    % red cut, since it could cut branches if multiple sizes are
    % defined for the same container, but since it should not be the
    % case, that's fine
    write('|'),
    display_container(World, Container, position(1, H, 1), size(W, H, D)),
    nl, nl.

display_container(World, Container, position(W, 1, D), size(W, _, D)) :-
    display_pos(World, Container, position(W, 1, D)),
    write('|'), nl,
    W2 is W*2-1,
    write('+'), display_line(W2), write('+'), nl, !.
display_container(World, Container, position(W, 1, Z), size(W, H, D)) :-
    display_pos(World, Container, position(W, 1, Z)),
    write('|'), nl,
    W2 is W*2-1,
    write('+'), display_line(W2), write('+'),
    nl, nl,
    Z1 is Z+1,
    display_container(World, Container, position(1, H, Z1), size(W, H, D)), !.
display_container(World, Container, position(W, Y, Z), size(W, H, D)) :-
    display_pos(World, Container, position(W, Y, Z)),
    write('|'), nl, write('|'),
    Y1 is Y-1,
    display_container(World, Container, position(1, Y1, Z), size(W, H, D)), !.
display_container(World, Container, position(X, Y, Z), size(W, H, D)) :-
    X =< W, Y =< H, Z =< D,
    display_pos(World, Container, position(X, Y, Z)),
    tab(1),
    X1 is X+1,
    display_container(World, Container, position(X1, Y, Z), size(W, H, D)), !.

%% display_pos(+Container, +Pos)
% Display what is contained at the given position in the given container
display_pos(World, Container, position(X, Y, Z)) :-
    % red cut, since if a position is occupied multiple times it will
    % cut successful branches, but it should never be the case
    occupied_by(World, Container, position(X, Y, Z), Id), !,
    write(Id).
display_pos(_, _, _) :-
    write(' ').

%% display(+World)
% Display all the containers of this world
% TODO: don't depend on the fact that there is two containers
display(World) :-
    write('Container 1:'), nl,
    display_container(World, 1),
    write('Container 2:'), nl,
    display_container(World, 2).

%% display_verbose(+World)
% Verbosely display a world
display_verbose(world(Objects, Containers, PlacementLists)) :-
    write('Remaining objects:'), nl,
    display_list(Objects),
    write('Placements:'), nl,
    display_plists(PlacementLists),
    write('Score: '),
    eval(world(Objects, Containers, PlacementLists), Score),
    write(Score), nl, nl.

%% display_plists(+PlacementLists)
% Verbosely display a placement list
display_plists([]).
display_plists([P|Ps]) :-
    display_list(P), nl,
    display_plists(Ps).

%% display_list(+List)
% Verbosely display a list
display_list([]).
display_list([H|T]) :-
    write(H), nl,
    display_list(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            Useful functions                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% pick(+List, -Element)
% Nondeterministically pick an element from a list
pick([H|_], H).
pick([_|T], Element) :-
    pick(T, Element).

%% pick_and_remove(+List, -Element, -NewList)
% Nondeterministically pick an element from a list and removes it
pick_and_remove([H|T], T, H).
pick_and_remove([H|T], [H|T1], Object) :-
    pick_and_remove(T, T1, Object).

%% predmerge(+Pred, +List1, +List2, -List3)
% Same as merge/3 but with a predicate
% Taken from http://web.mit.edu/course/6/6.863/src/pl-5.6.54/library/sort.pl
predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2),
	predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	predmerge(P, T1, [H2|T2], R).

%% predsort(+Pred, +List, -Sorted)
% like predsort, but keep duplicates
% Implementation adapted from SWI-Prolog's implementation
predmsort(P, L, R) :-
    '$skip_list'(N, L, Tail),
    (Tail == []
     ->  predmsort(P, N, L, _, R1),
         R = R1
     ;   must_be(L, list)
    ).

predmsort(P, 2, [X1, X2|L], L, R) :-
    !,
    call(P, Delta, X1, X2),
    msort2(Delta, X1, X2, R). 
predmsort(_, 1, [X|L], L, [X]) :- !.
predmsort(_, 0, L, L, []) :- !.
predmsort(P, N, L1, L3, R) :-
    N1 is N // 2,
    plus(N1, N2, N), 
    predmsort(P, N1, L1, L2, R1),
    predmsort(P, N2, L2, L3, R2),
    predmmerge(P, R1, R2, R). 

msort2(<, X1, X2, [X1, X2]).
msort2(=, X1, X2,  [X1, X2]).
msort2(>, X1, X2, [X2, X1]).

predmmerge(_, [], R, R) :- !.
predmmerge(_, R, [], R) :- !.
predmmerge(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2), !,
    predmmerge(Delta, P, H1, H2, T1, T2, Result).

predmmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
    predmmerge(P, [H1|T1], T2, R). 
predmmerge(=, P, H1, H2, T1, T2, [H1, H2|R]) :-
    predmmerge(P, T1, T2, R).
predmmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
    predmmerge(P, T1, [H2|T2], R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               Unit tests                                 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tests :-
    % tests done on data1.txt
    test_valid_after_place,
    test_legal,
    test_is_occupied,
    test_has_base.

test_valid_after_place :-
    objects(Objs),
    empty(Objs, [1, 2], EmptyWorld),
    put(EmptyWorld, 8, 1, position(1, 1, 1), NewWorld),
    not(is_valid(NewWorld, 1, position(1, 1, 1))).

test_legal :-
    objects(Objs),
    empty(Objs, [1, 2], EmptyWorld),
    is_legal(EmptyWorld, 1, 1, position(1, 1, 1)).

test_is_occupied :-
    objects(Objs),
    empty(Objs, [1, 2], EmptyWorld),
    put(EmptyWorld, 1, 1, position(1, 1, 1), NewWorld),
    is_occupied(NewWorld, 1, position(3, 1, 1)),
    not(is_occupied(NewWorld, 1, position(3, 2, 1))).

test_has_base :-
    objects(Objs),
    empty(Objs, [1, 2], EmptyWorld),
    put(EmptyWorld, 1, 1, position(1, 1, 1), NewWorld),
    has_base(EmptyWorld, 1, position(1, 1, 1)),
    has_base(EmptyWorld, 1, position(2, 1, 1)),
    has_base(EmptyWorld, 1, position(5, 1, 1)),
    not(has_base(EmptyWorld, 1, position(5, 5, 1))),
    has_base(NewWorld, 1, position(3, 2, 1)),
    not(has_base(NewWorld, 1, position(3, 3, 1))).
