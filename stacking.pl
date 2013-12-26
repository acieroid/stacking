%% TODO:
%% Constraints
%%   - Cannot put a heavier item on top of a lighter one
%%
%% Improve results:
%%   - generate the tree and explore it in a best-first search way,
%%   which would be more flexible
%%
%% Extensions:
%%  - Rotate objects
%%  - Try 3D
%%  - Improve output
%%  - Generate an image for the visualization

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                         World representation                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% X at Y
% Operator used to identify position of objects
:- op(500, xfx, at).

%% container_size(+Container, -Size)
% Specifies the size of the container (10x10x1 for each container in
% the original assignment)
container_size(1, size(10, 10, 1)).
container_size(2, size(10, 10, 1)).

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
    placement_list(Containers, PlacementLists, Container, PlacementList).

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
    EndX is StartX+Width-1, EndY is StartY+Height-Y, EndZ is StartZ+Depth-1,
    between(StartX, EndX, X),
    between(StartY, EndY, Y),
    between(StartZ, EndZ, Z).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            Best-First Search                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% add_best_first(+Children, +Agenda, -NewAgenda)
% Merge new childrens in agenda
add_best_first(Children, Agenda, NewAgenda) :-
    predsort(compare_world, Children, SortedChildren),
    predmerge(compare_world, SortedChildren, Agenda, NewAgenda).

%% search(+Agenda, -FinalWorld)
% Do a best-first search on the search space. Taken from Simply
% Logical, chapter 6.
search([World|_], World) :-
    filled(World).
search([World|Rest], FinalWorld) :-
    children(World, Children),
    add_best_first(Children, Rest, NewAgenda),
    search(NewAgenda, FinalWorld).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                    Test                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% data1.txt
object(1,size(5,1,1)).
object(2,size(5,7,1)).
object(3,size(2,1,1)).
object(4,size(1,1,1)).
object(5,size(3,2,1)).
object(6,size(5,1,1)).
object(7,size(1,5,1)).
object(8,size(4,3,1)).
object(9,size(4,7,1)).
object(10,size(4,2,1)).
object(11,size(1,6,1)).
object(12,size(2,4,1)).
object(13,size(7,4,1)).
object(14,size(6,7,1)).
object(15,size(4,7,1)).
object(16,size(5,3,1)).
object(17,size(3,5,1)).
object(18,size(1,1,1)).

%% objects(+EndId, -Objects)
% Generate objects identifiers from 1 to N
objects(1, []) :- !. % green cut
objects(N, [N|Rest]) :-
    N > 0,
    N1 is N-1,
    objects(N1, Rest).

%% test
% Some tests
test :-
    objects(18, Objs),
    empty(Objs, [1, 2], EmptyWorld),
    put(EmptyWorld, 8, 1, position(1, 1, 1), NewWorld),
    is_valid(NewWorld, 1, position(1, 1, 1)).
