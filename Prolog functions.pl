
%returns true if for a given course, the unit number is 3 or 4
fc_course(Course):-
    course(Course,_,X),
    X >= 3,
    X =< 4.

%finds all courses that ecs110 is a prereq for by using member function    
prereq_110(Prerequisites):- 
    course(Prerequisites, Y, _),
    member(ecs110, Y).

%same as prereq but for students in 140a    
ecs140a_students(Students):- 
    student(Students, X),
    member(ecs140a, X).

%finds all of john's instructors    
instructor_names(Instructor):-
    student(john, Y),
    instructor(Instructor, Z), %checks all the teachers that intersect with john's courses
    intersects(Y,Z).
    

intersects([H|_],List) :- %for finding when elements of one list appears in another list at least once
    member(H,List), !.    %if head is a member of the list, we exit
intersects([_|T],List) :- %otherwise we check if tail has anything that intersects
    intersects(T,List).

students(STUDENTS) :-
    instructor(jim, Y),
    student(STUDENTS, Z),
    intersects(Y,Z).
    


allprereq(Course,Output) :- 
    findall(A, trans_prereq(A, Course), Output).
    
%below are helper fxns for allprereq


prereq(X, Y) :- %base case is a direct relationship
    course(Y, Prereqs, _),
    member(X, Prereqs).
    
%we first find the prereq of the one before it, and then use 
%transitive closure to find the prereqs of that one
trans_prereq(X, Y) :-
    prereq(X, Z),
    trans_prereq(Z, Y).
    
trans_prereq(X, Y) :-
    prereq(X, Y).
    
%all_length is really similiar to what we had in Lisp
all_length([],0) :- !.

all_length([[]|T], All_Len) :- %checks if it's an empty [] which counts as an atom
    all_length(T, TailLen), %counts number in tail
    All_Len is TailLen + 1, %add 1 for the empty [] which is an atom
    !.
    
all_length([H|T],All_Len) :- %counts length of head and tail, similar to equal
    all_length(H, HeadLen),
    all_length(T, TailLen),
    All_Len is HeadLen + TailLen,
    !.

all_length(_, 1).


equal_a_b(L) :-
    count_term(a, L, ATotal), %counts number of a atoms
    count_term(b, L, BTotal), %counts number of b atoms
    ATotal = BTotal, %equal succeeds if number of a and b atoms is the same
    !.

%helper functions for equal_a_b
count_term(_, [], 0) :- !.

%Recursively checks if it's A or B, if so, we increment it
count_term(AorB, [H|T], NumTerm) :-
    count_term(AorB, H, HeadNum), %puts count of Head into HeadNum
    count_term(AorB, T, TailNum), %puts count of tail into TailNum
    NumTerm is HeadNum + TailNum, %final result is the sum of the two above
    !.

count_term(AorB, AorB, 1) :- %if term we're looking for matches term in list then that counts as 1 item
    AorB = H, %only true if AorB is at the head of the list we're looking for
    !.
    
count_term(_,_,0). %otherwise anything else other than what we're looking for counts as 0


%if the reverse of a word = the original, then it's a palindrome
palin(A):-
    reverse(A, X), %reverses list
    A = X, %if A = X true, otherwise false
    !.

reverse([],[]). /* helper function for palin, done in class*/
reverse([H|T], L1):-
    reverse(T,L1), 
    append(L1,[H],L1).    
    

swap_prefix_suffix(K, L, Output) :-
    append(Prefix_K, Suffix, L), %defines variable prefix_k which is prefix and k put together
    append(Prefix, K, Prefix_K), %gets prefix out, we get suffix from above append
    append(K, Prefix, Temp), %append prefix to end of sublist, put it in a temp list
    append(Suffix, Temp, Output). %put the suffix at the front and then append the temp
    !.
    
    
good([Z]):- %base case is that we have a good sequence which is just 0
    Z = 0.
    
good([1|T]):- %if we encounter a 1
    append(A, B, T), %sequence is good if we have two good sequences afterward
    good(A),
    good(B).
    
    
%below are for part 3    
opposite(left, right).
opposite(right, left).

%(Famer, Wolf, Goat, Cabbage).
state(_,_,_,_).

%unsafe if farmer on one side, with goat/cabbage on other
%or if farmer on one side with wolf/goat on the other
unsafe(state(Farm,Wolf,Goat,Cab)):- Wolf = Goat, opposite(Farm,Goat).
unsafe(state(Farm,Wolf,Goat,Cab)):- Cab = Goat, opposite(Farm,Goat).

%if it is not unsafe, it is safe
safe(state(Farm,Wolf,Goat,Cab)):- \+unsafe(state(Farm,Wolf,Goat,Cab)).

%X goes from A to B, so opposite will work.
take(X,A,B):- opposite(A,B).

%(Famer, Wolf, Goat, Cabbage).
%defines possible edges and states
arc(take(nobody, A, B), state(A, C, D, E), state(B, C, D, E))
			:- take(nobody, A, B),
			   safe(state(B,C,D,E)).
			   
arc(take(wolf, A, B), state(A, A, C, D), state(B, B, C, D))
			:- take(wolf, A, B),
			   safe(state(B,B,C,D)).

arc(take(goat, A, B), state(A, D, A, C), state(B, D, B, C))
			:- take(goat, A, B),
			   safe(state(B,D,B,C)).

arc(take(cab, A, B), state(A, D, C, A), state(B, D, C, B))
			:- take(cab, A, B),
			   safe(state(B,D,C,B)).


go(State2, State2, T):- !. %base case to check when they're at same destination
go(State1, State2, T) :-
    arc(X, State1, State3), %checks if there's an edge between an intermediate location
    safe(State3), %checks if intermediate location is safe
    legal(State3,T), %makes sure we don't repeat a move
    print(X),
    nl,
    go(State3, State2, [State3|T]). %transitive relationship between state1 and state2
    

%makes sure we're not just going back and forth    
legal(X,[]).
legal(X, [H|T]):-
    \+X = state(left,left,left,left),
    %\+X = state(left,left,left,right),
    \+X = H, legal(X,T).
    
%prints out all the moves    
print(X):-
    X.

%solves puzzle
go(state(left,left,left,left), state(right,right,right,right), X).

