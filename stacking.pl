[data1].

%% container_size(+Container, -Size)
% specifies the size of the container (10x10x1 for each container in
% the original assignment)
container_size(1, size(10, 10, 1)).
container_size(2, size(10, 10, 1)).

%% is_at(?Container, ?Pos, ?Obj)
% dynamic predicate that associates an object (functor object(Id,
% Size), as defined in the data sets) to a position (functor
% position(X, Y, Z)) inside a container (number). The position
% indicates the bottom left point of the box
:- dynamic is_at/3.

%% evaluate(+Method, -Score)
% evaluate the current configuration, based on a certain method:
%   - count: maximize the number of objects placed in the containers
%TODO.

%% display_container(+Container)
% nicely displays a container, one level of depth at a time (Z=1, then
% Z=2, etc.)
display_container(Container) :-
    container_size(Container, size(W, H, D)),
    % red cut, since it could cut branches if multiple sizes are
    % defined for the same container, but since it should not be the
    % case, that's fine
    display_container(Container, position(1, H, 1), size(W, H, D)).

display_container(_, position(W, 1, D), size(W, _, D)) :- !.
display_container(Container, position(W, 1, Z), size(W, H, D)) :-
    display_pos(Container, position(W, 1, Z)),
    nl, nl,
    Z1 is Z+1,
    display_container(Container, position(1, H, Z1), size(W, H, D)), !.
display_container(Container, position(W, Y, Z), size(W, H, D)) :-
    display_pos(Container, position(W, Y, Z)),
    nl,
    Y1 is Y-1,
    display_container(Container, position(1, Y1, Z), size(W, H, D)), !.
display_container(Container, position(X, Y, Z), size(W, H, D)) :-
    X =< W, Y =< H, Z =< D,
    display_pos(Container, position(X, Y, Z)),
    tab(1),
    X1 is X+1,
    display_container(Container, position(X1, Y, Z), size(W, H, D)), !.

display_pos(Container, position(X, Y, Z)) :-
    % red cut, since if a position is occupied multiple times it will
    % cut successful branches, but it should never be the case
    occupied_by(Container, position(X, Y, Z), Id), !,
    write(Id).
display_pos(_, _) :-
    write(' ').

%% is_inside(?Container, +Pos)
% verify if a position is legal for a container
is_inside(Container, position(X, Y, Z)) :-
    X > 0, Y > 0, Z > 0,
    container_size(Container, size(SX, SY, SZ)),
    X =< SX, Y =< SY, Z =< SZ.

%% occupied_by(?Container, +Pos, -Id)
% Return the object that occupies a certain position in a container.
occupied_by(Container, position(X, Y, Z), Id) :-
    is_at(Container, position(OX, OY, OZ), object(Id, size(SX, SY, SZ))),
    X >= OX, X < OX+SX,
    Y >= OY, Y < OY+SY,
    Z >= OZ, Z < OZ+SZ.

%% is_occupied(?Container, +Pos)
% Verify if a position of a container is occupied
is_occupied(Container, Pos) :-
    occupied_by(Container, Pos, _).

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
