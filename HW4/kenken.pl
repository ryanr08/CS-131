%code to get transpose of matrix T
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).

transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for kenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% kenken took 41 cpu_time in order to solve the 4 x 4 given in the spec.

%addition of all elements in list
add_list([H], H).
add_list([H | T], S) :- add_list(T, R), S #= H + R.

%multiplication of all elements in list
mult_list([H], H).
mult_list([H | T], P) :- mult_list(T, R), P #= H * R.

%get the elements of the matrix T that correspond to the sqaures given by the constraint and place them into a list
int_list(_,[],A,A).
int_list(G, [ [H|Tl] | T ], A, R) :- nth(H, G, V), nth(Tl,V,X) ,int_list(G,T,[X|A],R). 

%line constraint for addition of multiple squares
fd_line_constraint(T, +(S, L)) :-
    int_list(T, L, [], A),
    add_list(A, S).

%line constraint for multiplication of multiple squares
fd_line_constraint(T, *(P, L)) :-
    int_list(T, L, [], A),
    mult_list(A, P).

%line constraint for subtraction of two squares
fd_line_constraint(T, -(D, [Hj | Tj], [Hk | Tk])) :-
    nth(Hj, T, V), nth(Tj,V,X),
    nth(Hk, T, W), nth(Tk,W,Y),
    (D #= X - Y; D #= Y - X).

%line constraint for division of two squares
fd_line_constraint(T, /(D, [Hj | Tj], [Hk | Tk])) :-
    nth(Hj, T, V), nth(Tj,V,X),
    nth(Hk, T, W), nth(Tk,W,Y),
    (X * D #= Y; Y * D #= X).

%add length, domain, and unique constrains to each row of matrix
add_constraints([],_).
add_constraints([H | T], N) :-
    length(H, N),
    fd_domain(H, 1, N),
    fd_all_different(H),
    add_constraints(T, N).

%call fd_labeling on each row of T
label([]).
label([H | T]) :- fd_labeling(H), label(T).

kenken(N, C, T) :-
    length(T, N),           % ensure T is of length N
    add_constraints(T, N),                  % add constraints to each row
    transpose(T, Tr), add_constraints(Tr, N),           % add constraints to each column
    maplist(fd_line_constraint(T), C),                     % map all constraints in C to the proper elements
    label(T).                                               % solve for T

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for plain_kenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plain_kenken took 676 cpu_time in order to solve the 4 x 4 given in the spec.

%addition of all elements in list
add_list2([H], H).
add_list2([H | T], S) :- add_list2(T, R), S is H + R.

%multiplication of all elements in list
mult_list2([H], H).
mult_list2([H | T], P) :- mult_list2(T, R), P is H * R.

%line constraint for addition of multiple squares
line_constraint(T, +(S, L)) :-
    int_list(T, L, [], A),
    add_list2(A, S).

%line constraint for multiplication of multiple squares
line_constraint(T, *(P, L)) :-
    int_list(T, L, [], A),
    mult_list2(A, P).

%line constraint for subtraction of two squares
line_constraint(T, -(D, [Hj | Tj], [Hk | Tk])) :-
    nth(Hj, T, V), nth(Tj,V,X),
    nth(Hk, T, W), nth(Tk,W,Y),
    (D is X - Y; D is Y - X).

%line constraint for division of two squares
line_constraint(T, /(D, [Hj | Tj], [Hk | Tk])) :-
    nth(Hj, T, V), nth(Tj,V,X),
    nth(Hk, T, W), nth(Tk,W,Y),
    (Y is D * X; X is D * Y).

%obtain a list of numbers from 1 through N
do_list(N, L):- 
  findall(Num, between(1, N, Num), L).

%add proper constraints to rows
add_constraints2([],_).
add_constraints2([H | T], N) :-
    length(H, N),
    do_list(N, L), permutation(L,H),
    add_constraints2(T, N).

%function to solve kenken without fd_domain solver
plain_kenken(N, C, T) :-
    length(T, N),
    add_constraints2(T, N),
    transpose(T, Tr), add_constraints2(Tr, N),
    maplist(line_constraint(T), C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% noop_kenken api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Template:
%   noop_kenken(N, C, C_new, T).
%
%   noop_kenken takes in an integer N to specift the NxN grind, the list of constraints C without operations, and two variables for C_new and T.
%   noop_kenken will output a filled T and C_new where C_new is the constraint list C except with operations specified, and the T will be a specific solution
%   for that C_new. noop_kenken will output all possible C_new + T combinations given C and N.
%
% A sample call to noop_kenken is as follows:
%noop_kenken_testcase :- 
%    noop_kenken(
%  6,
%  [
%   (11, [[1|1], [2|1]]),
%   (2, [1|2], [1|3]),
%   (20, [[1|4], [2|4]]),
%   (6, [[1|5], [1|6], [2|6], [3|6]]),
%   (3, [2|2], [2|3]),
%   (3, [2|5], [3|5]),
%   (240, [[3|1], [3|2], [4|1], [4|2]]),
%   (6, [[3|3], [3|4]]),
%   (6, [[4|3], [5|3]]),
%   (7, [[4|4], [5|4], [5|5]]),
%   (30, [[4|5], [4|6]]),
%   (6, [[5|1], [5|2]]),
%   (9, [[5|6], [6|6]]),
%   (8, [[6|1], [6|2], [6|3]]),
%   (2, [6|4], [6|5])
%  ], 
%  C_new,
%  T
%).
%
%  C_new and T will be filled for every output until all possible solutions are exhausted.