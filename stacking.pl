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

%% place(+PlacementLists, +Containers, +Container, +Content, -NewPlacementLists)
% Place an object in a container, updating the given placement
% lists. No checks are done.
place([P|Ps], [Container|_], Container, Content, [[Content|P]|Ps]) :- !.
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
    eval_strategy(Strategy, World, Score)
