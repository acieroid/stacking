%% Example run:
%% ?- [data1].
%% ?- [stacking].
%% ?- run(Best, 15)
%% OR
%% ?- best(Best)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             Main predicates                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% run
% Do a best-first search on the dataset currently loaded and display
% configurations found that have their score greater than MinimalScore.
run(Best, MinimalScore) :-
    objects(Objects),
    containers(Containers),
    empty(Objects, Containers, EmptyWorld),
    search([EmptyWorld], MinimalScore, Best),
    display(Best).

%% best
% Try to find the best configurations, according to eval/2. Return
% results of increasing scores (only one result per score, ie. if
% multiple worlds have the same score, only the first found will be
% proposed to the user).
best(Best) :-
    objects(Objects),
    containers(Containers),
    empty(Objects, Containers, EmptyWorld),
    eval(EmptyWorld, EmptyScore),
    search_best_incr([EmptyWorld], EmptyScore, [EmptyWorld], Best),
    display(Best).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                Parameters                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% strategy(-Strategy)
% Defines the strategy to use to compute the stackings.
% Possible strategies: min_unused_space, max_objects, max_balance,
% max_objects_max_balance, max_weight
strategy(min_unused_space).

%% base_factor(-BaseFactor)
% The percentage of invalid base that we can accept for an object.
% If 1, objects can be placed anywhere (that doesn't make much sense).
% If 0, objects can only be placed if they have a full base.
% For a real-world stacking problem, 1 would be a good value.
% For a tetris-like problem, a small value > 0 (eg. 0.001) would be a
% good value.
base_factor(0.5).

%% container_size(+Container, -Size)
% Specifies the size of the container (10x10x1 for each container in
% the original assignment)
container_size(1, size(10, 10, 1)).
container_size(2, size(10, 10, 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                         World representation                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For object representation, see the data*.pl files. Objects Ids
% should start from 1 and increase 1 by 1.

%% objects(-Objects)
% Find all the objects
objects(Objects) :-
    findall(Obj, object(Obj, _), Objects).

%% volume(+Object, -Volume)
% Compute the volume of an object
volume(Id, Volume) :-
    object(Id, size(W, H, D)),
    Volume is W * H * D.

%% weight(+Object, -Weight)
% Compute the weight of an object
weight(Object, Weight) :-
    % weight is proportional to volume
    volume(Object, Weight).

%% X at Y
% Operator used to identify position of objects
:- op(500, xfx, at).

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

%% placement_list_count(+PlacementList, -Count)
% Count the number of objects inside a container
placement_list_count(P, N) :-
    length(P, N).

%% placement_list_volume(+PlacementList, -Volume)
% Measure the total volume inside a placement list
placement_list_volume([], 0).
placement_list_volume([Id at _|Rest], Volume) :-
    volume(Id, Volume1),
    placement_list_volume(Rest, Volume2),
    Volume is Volume1 + Volume2.

%% placement_list_weight(+PlacementList, -Weight)
% Measure the total weight inside a placement list
placement_list_weight([], 0).
placement_list_weight([Id at _|Rest], Weight) :-
    weight(Id, Weight1),
    placement_list_weight(Rest, Weight2),
    Weight is Weight1 + Weight2.

%% placement_lists_count(+PlacementLists, -Count)
% Count the number of objects inside all the containers
placement_lists_count([], 0).
placement_lists_count([P|Ps], N) :-
    placement_list_count(P, N1),
    placement_lists_count(Ps, N2),
    N is N1 + N2.

%% placement_lists_volume(+PlacementList, -Volume)
% Measure the total volume inside all containers
placement_lists_volume([], 0).
placement_lists_volume([P|Ps], Volume) :-
    placement_list_volume(P, Volume1),
    placement_lists_volume(Ps, Volume2),
    Volume is Volume1 + Volume2.

%% placement_lists_weight(+PlacementList, -Weight)
% Measure the total weight put inside all containers
placement_lists_weight([], 0).
placement_lists_weight([P|Ps], Weight) :-
    placement_list_weight(P, Weight1),
    placement_lists_weight(Ps, Weight2),
    Weight is Weight1 + Weight2.

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
% Compare two world using eval, in the reverse order (a smaller world
% is better, has a higher score), to be easier to use with sort
compare_worlds(Order, World1, World2) :-
    eval(World1, Score1),
    eval(World2, Score2),
    compare(Order, Score2, Score1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Heuristics                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% eval_strategy(+Strategy, +World, -Score)
% Evaluate the score of a board with the given strategy
eval_strategy(max_objects,
              world(_, _, PlacementLists), Score) :-
    % Maximize the number of objects placed
    placement_lists_count(PlacementLists, Score).
eval_strategy(max_balance,
              world(_, _, PlacementLists), Score) :-
    % Maximize the balance between the containers, by weight
    placement_lists_weight(PlacementLists, TotalWeight),
    length(PlacementLists, N),
    AverageWeight is TotalWeight / N,
    findall(Diff,
            (member(P, PlacementLists),
             placement_list_weight(P, W),
             Diff is abs(AverageWeight - W)),
            Diffs),
    sum_list(Diffs, WeightDiff),
    Score is TotalWeight - WeightDiff.
eval_strategy(max_objects_max_balance, World, Score) :-
    % Maximize the number of objects *and* the balance between both
    % containers (but gives more importance to the number of objects)
    eval_strategy(max_objects, World, ScoreNumberObjects),
    eval_strategy(max_balance, World, ScoreBalance),
    Score is ScoreNumberObjects * 100 + ScoreBalance.
eval_strategy(max_weight,
              world(_, _, PlacementLists), Score) :-
    % Maximize the total weight placed
    placement_lists_weight(PlacementLists, Score).
eval_strategy(min_unused_space,
              world(_, _, PlacementLists), Score) :-
    % Minimize the unused spaces
    % This is the same as maximizing the volume
    placement_lists_volume(PlacementLists, Score).

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

%% base_of(+World, +Container, +Pos, -Base)
% Find the base of a position. The base can either be another object
% (in which case Base is its Id), or the bottom of the container (and
% Base = bottom).
base_of(_, _, position(_, 1, _), bottom) :-
    !. % might cut some success branch, but not when used correctly
base_of(World, Container, position(X, Y, Z), Base) :-
    Y > 1,
    % a position has a base if the position beneath it is occupied
    Y1 is Y-1,
    occupied_by(World, Container, position(X, Y1, Z), Base).

%% has_base(+World, +Container, +Pos)
% Verify if the position has something beneath it, to serve as a
% base.
has_base(World, Container, Pos) :-
    base_of(World, Container, Pos, _).

%% has_heavier_base(+World, +Container, +Object, +Pos)
% Similar has_base, but true only if the base on which the object will
% be put is heavier than the object itself.
has_heavier_base(World, Container, _, Pos) :-
    % bottom is considered heavier
    base_of(World, Container, Pos, bottom),
    % red cut but weight should fail if Base = bottom, so it's OK
    !.
has_heavier_base(World, Container, Object, Pos) :-
    base_of(World, Container, Pos, Base),
    weight(Object, Weight),
    weight(Base, BaseWeight),
    Weight < BaseWeight.

%% is_legal(+Container, +Object, +Pos)
% Check if it is legal to put an object at a given position in a
% container.
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
    % Filter out the positions that have a valid base
    exclude(has_heavier_base(World, Container, Object), BasePositions, InvalidBases),
    % Variant: if we accept heavier objects on top of lighter ones:
    %   exclude(has_base(World, Container), BasePositions, InvalidBases),

    % Should at least have half of the bases that are valid
    length(InvalidBases, NInvalidBases),
    length(BasePositions, NBases),
    base_factor(BaseFactor),
    NInvalidBases =< NBases * BaseFactor.

%% possible_positions(+World, +Container, +Object, -Positions)
% Bind Positions to all the possible (legal) positions that can take
% Object in Container.
possible_positions(World, Container, Object, Positions) :-
    container_size(Container, size(Width, Height, Depth)),
    findall(Pos,
            (in_square(position(1, 1, 1), position(Width, Height, Depth), Pos),
             is_legal(World, Container, Object, Pos)),
            Positions).

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

%% search(+Agenda, +ScoreWanted, -FinalWorld)
% Do a best-first search on the search space, accepting all results
% that have a score higher than ScoreWanted. Taken from Simply
% Logical, chapter 6.
search([World|_], ScoreWanted, World) :-
    eval(World, Score),
    Score >= ScoreWanted.
search([World|Rest], ScoreWanted, FinalWorld) :-
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    search(NewAgenda, ScoreWanted, FinalWorld).

%% search_best(+Agenda, +MaxScore, +CurrentBests, -Best)
% Find one of the worlds with the highest score. Might take a long
% time if the state space is big. Will explore the entire state space
% (but only once).
search_best([], _, Bests, Best) :-
    member(Best, Bests).
search_best([World|Rest], MaxScore, Bests, Best) :-
    eval(World, Score),
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    ( Score > MaxScore ->
          search_best(NewAgenda, Score, [World], Best);
      Score = MaxScore ->
          search_best(NewAgenda, Score, [World|Bests], Best);
      Score < MaxScore ->
          search_best(NewAgenda, MaxScore, Bests, Best)
    ).

%% search_best_incr(+Agenda, +MaxScore, +CurrentBests, -Best)
% Like search_best, but returns results increasing in score until
% finds the best
search_best_incr([], _, Bests, Best) :-
    member(Best, Bests).
search_best_incr([World|_], MaxScore, _, World) :-
    eval(World, Score),
    Score > MaxScore.
search_best_incr([World|Rest], MaxScore, Bests, Best) :-
    eval(World, Score),
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    ( Score > MaxScore ->
          search_best_incr(NewAgenda, Score, [World], Best);
      Score = MaxScore ->
          search_best_incr(NewAgenda, Score, [World|Bests], Best);
      Score < MaxScore ->
          search_best_incr(NewAgenda, MaxScore, Bests, Best)
    ).

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
    W2 is W*3-1,
    write('+'), display_line(W2), write('+'), nl, !.
display_container(World, Container, position(W, 1, Z), size(W, H, D)) :-
    display_pos(World, Container, position(W, 1, Z)),
    write('|'), nl,
    W2 is W*3-1,
    write('+'), display_line(W2), write('+'),
    nl, nl,
    Z1 is Z+1, write('|'),
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

%% display_pos(+World, +Container, +Pos)
% Display what is contained at the given position in the given container
display_pos(World, Container, position(X, Y, Z)) :-
    % red cut, since if a position is occupied multiple times it will
    % cut successful branches, but it should never be the case
    occupied_by(World, Container, position(X, Y, Z), Id), !,
    ( Id < 10 ->
          write('_'), write(Id);
      true ->
          write(Id)
    ).
display_pos(_, _, _) :-
    write('  ').

%% display_containers(+World, +Containers)
% Display the containers of a world
display_containers(_, []).
display_containers(World, [C|Cs]) :-
    write('Container '), write(C), nl,
    display_container(World, C), nl,
    display_containers(World, Cs).

%% display(+World)
% Display all the containers of this world
display(World) :-
    World = world(Objects, Containers, PlacementLists),
    length(Objects, Remaining),
    placement_lists_count(PlacementLists, Placed),
    placement_lists_weight(PlacementLists, Weight),
    eval(World, Score),
    format('Objects placed: ~d (~d remaining)~n', [Placed, Remaining]),
    format('Weight placed: ~d~n', [Weight]),
    format('Score: ~1f~n', [Score]),
    display_containers(World, Containers).

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
    test_has_base,
    test_score.

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

test_score :-
    objects(Objs),
    empty(Objs, [1, 2], EmptyWorld),
    put(EmptyWorld, 1, 1, position(1, 1, 1), W1),
    put(W1, 2, 2, position(1, 1, 1), W2),
    eval(EmptyWorld, Score0),
    eval(W1, Score1),
    eval(W2, Score2),
    Score0 =< Score1,
    Score1 =< Score2.
