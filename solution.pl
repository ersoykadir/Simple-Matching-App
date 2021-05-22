% kadir ersoy
% 2018400252
% compiling: yes
% complete: yes


% include the knowledge base
:- ['load.pro'].

fillList(0,[]):-!.
fillList(Len,[H|T]):-
    Temp is Len-1,
    fillList(Temp,T),
    H = 1.

subNsqWLists([],[],[],[]).
subNsqWLists([-1|T],[_|T2],[_|Wt],Res):-subNsqWLists(T,T2,Wt,Res),!.
subNsqWLists([H1|T1],[H2|T2],[Wh|Wt],[H3|T3]):-
    subNsqWLists(T1,T2,Wt,T3),
    H3 is Wh*(H1-H2)*(H1-H2).

sumEl([],Res):-Res is 0.
sumEl([H|T],Sum):-
    sumEl(T,Res),
    Sum is H+Res.

% 3.1 glanian_distance(+Name1, +Name2, -Distance) 5 points
glanian_distance(Name1, Name2, Distance):-
	expects(Name1,_,E),
	glanian(Name2,_,F),
    length(E,Len),
    fillList(Len,W),
	subNsqWLists(E,F,W,Res),sumEl(Res,Sum),
    Distance is Sum**(1/2).
% 3.2 weighted_glanian_distance(Name1, Name2, Distance) 10 points
weighted_glanian_distance(Name1, Name2, Distance):-
    expects(Name1,_,E),
	glanian(Name2,_,F),
    weight(Name1,W),
	subNsqWLists(E,F,W,Res),sumEl(Res,Sum),
    Distance is Sum**(1/2).
% 3.3 find_possible_cities(Name, CityList) 5 points
findCity(Name,City):-
    city(City,ResList,_),
    member(Name,ResList),!.
find_possible_cities(Name, CityList):-
    likes(Name,_,LikedCities),
    findCity(Name,City),
    City1 = [City],
    list_to_set(City1,S1),list_to_set(LikedCities,S2),
    union(S1,S2,CityList).
    %Cities = [City1,LikedCities],
    %append(Cities,TempList),%this might not work, need to use a set to avoid repetition
    %list_to_set(TempList,CityList).

% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points
merge_possible_cities(Name1, Name2, MergedCities):-
	find_possible_cities(Name1, CityList1),
	find_possible_cities(Name2, CityList2),
	list_to_set(CityList1,S1),list_to_set(CityList2,S2),
    union(S1,S2,MergedCities).
	%Cities = [CityList1,CityList2],
	%append(Cities,TempList),
	%list_to_set(TempList,MergedCities).

% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points
find_mutual_activities(Name1, Name2, MutualActivities):-
	likes(Name1,LikedActivities1,_),
    likes(Name2,LikedActivities2,_),
    list_to_set(LikedActivities1,S1),list_to_set(LikedActivities2,S2),
    intersection(S1,S2,MutualActivities).
    %findall(Mutual,(member(Mutual,LikedActivities1),member(Mutual,LikedActivities2)), MutualActivities).

% 3.6 find_possible_targets(Name, Distances, TargetList) 10 points
find_possible_targets(Name,Distances,TargetList):-
    expects(Name,ExpectedGenders,_),
    %glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),
    %findall(Distance,(glanian_distance(Name,T,Distance),member(T,TargetList)), Distances),
    %findall(TargetName,(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders)), TargetList),
    findall([TargetDistance,TargetName],(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),glanian_distance(Name,TargetName,TargetDistance)), T),
   	sort(T,SortedT),
    findall(Distance,(nth0(0,E,Distance),member(E,SortedT)), Distances),
    findall(TargetName,(nth0(1,E,TargetName),member(E,SortedT)), TargetList).
	%findall(TargetName,(glanian_distance(Name,TargetName,Distance),member(Distance,Distances),glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders)), Distances).

% 3.7 find_weighted_targets(Name, Distances, TargetList) 15 points
find_weighted_targets(Name, Distances, TargetList):-
	expects(Name,ExpectedGenders,_),
    %glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),
    %findall(Distance,(glanian_distance(Name,T,Distance),member(T,TargetList)), Distances),
    %findall(TargetName,(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders)), TargetList),
    findall([TargetDistance,TargetName],(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),weighted_glanian_distance(Name,TargetName,TargetDistance)), T),
   	sort(T,SortedT),
    findall(Distance,(nth0(0,E,Distance),member(E,SortedT)), Distances),
    findall(TargetName,(nth0(1,E,TargetName),member(E,SortedT)), TargetList).
	%findall(TargetName,(glanian_distance(Name,TargetName,Distance),member(Distance,Distances),glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders)), Distances).

% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points
find_weighted_targets_wout_old(Name,TargetList):-
	expects(Name,ExpectedGenders,_),
	%dislikes(Name,_,_,Limits),
	%findall(TargetName,(glanian(TargetName,TargetGender,_),(member(TargetGender,ExpectedGenders),\+member(TargetGender,OldRelations))), TargetList),
	findall(TargetName,(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),(\+old_relation([Name,TargetName]),\+old_relation([TargetName,Name]))), TargetList).

compar([],[]):-!.
compar([_|T1],[[]|T2]):-compar(T1,T2),!.
compar([H1|T1],[H2|T2]):-
    compar(T1,T2),
    H2 = [Lmin,Lmax],
    (   H1 >= Lmin 	) ,
    (   H1 =< Lmax	) .
limit_check(TargetName,Limits):-
	glanian(TargetName,_,Features),
	compar(Features,Limits).
matches(DislikedActivities,TargetName):-
	likes(TargetName,LikedActivities,_),
	findall(M,(member(M,DislikedActivities),member(M,LikedActivities)),Matched),
	length(Matched,Len),Len < 3.
target_city_act(Name,TargetName,CityName,Act,TargetDistance,TargetList):-
	%find a Target thats in limits and doesnt have too much conflict, get its distance
	likes(Name,LikedActivities,LikedCities),
	dislikes(Name,DislikedActivities,_,Limits),
	member(TargetName,TargetList),limit_check(TargetName,Limits),matches(DislikedActivities,TargetName),
	weighted_glanian_distance(Name,TargetName,TargetDistance),

	%find possible cities that target and name both like
	merge_possible_cities(Name,TargetName,Cities),
	
	city(CityName,HabList,ActList),member(CityName,Cities),member(Act,ActList),\+member(Act,DislikedActivities),
	(member(Name,HabList);member(CityName,LikedCities);member(Act,LikedActivities)).
%find_my_best_target(josizar,D,A,C,T).
possible_acts(Act,Cities1):-
	member(City,Cities1),
	city(City,_,ActList),
	member(Act,ActList).
find_my_best_target(Name, Distances, Activities, Cities, Targets):-

	%findall(Relation,(old_relation([Name,Relation]);old_relation([Relation,Name])), OldRelations),%find old Relations
	find_weighted_targets_wout_old(Name,TargetList),%create TargetList without old relations

	
	findall([TargetDistance,Act,CityName,TargetName],(target_city_act(Name,TargetName,CityName,Act,TargetDistance,TargetList)),List),
	sort(List,Sorted),
	%print(List),
	findall(D,(nth0(0,E,D),member(E,Sorted)), Distances),
	findall(A,(nth0(1,E,A),member(E,Sorted)), Activities),
	findall(C,(nth0(2,E,C),member(E,Sorted)), Cities),
	findall(T,(nth0(3,E,T),member(E,Sorted)), Targets).

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points
target_gender_exp(TargetName,Name):-
	glanian(Name,Gender,_),
	expects(TargetName,TargetExpectedGenders,_),member(Gender,TargetExpectedGenders).
find_weighted_targets_wout_old_2(Name,TargetList):-
	expects(Name,ExpectedGenders,_),

	%expects(TargetName,TargetExpectedGenders,_),
	%glanian(Name,Gender,_),
	%dislikes(Name,_,_,Limits),
	%findall(TargetName,(glanian(TargetName,TargetGender,_),(member(TargetGender,ExpectedGenders),\+member(TargetGender,OldRelations))), TargetList),
	findall(TargetName,(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),target_gender_exp(TargetName,Name),(\+old_relation([Name,TargetName]),\+old_relation([TargetName,Name]))), TargetList).

target_city_act_2(Name,TargetName,CityName,Act,TargetDistance,TargetList):-
	%find a Target thats in limits and doesnt have too much conflict, get its distance
	likes(Name,LikedActivities,LikedCities),
	likes(TargetName,TargetLikedActivities,TargetLikedCities),
	dislikes(Name,DislikedActivities,DislikedCities,Limits),
	dislikes(TargetName,TargetDislikedActivities,TargetDislikedCities,TargetLimits),
	member(TargetName,TargetList),limit_check(TargetName,Limits),limit_check(Name,TargetLimits),matches(DislikedActivities,TargetName),matches(TargetDislikedActivities,Name),
	weighted_glanian_distance(Name,TargetName,D1),
	weighted_glanian_distance(TargetName,Name,D2),TargetDistance is (D1+D2)/2,

	merge_possible_cities(Name,TargetName,Cities),
	city(CityName,HabList,ActList),member(CityName,Cities),member(Act,ActList),\+member(CityName,DislikedCities),\+member(CityName,TargetDislikedCities),\+member(Act,DislikedActivities),\+member(Act,TargetDislikedActivities),
	(member(Name,HabList);member(CityName,LikedCities);member(Act,LikedActivities)),
	(member(TargetName,HabList);member(CityName,TargetLikedCities);member(Act,TargetLikedActivities)).
	%((member(CityName,Cities1),member(Act,Acts1));(member(CityName,CitiesContainLikedAct),member(Act,LikedActivities))),((member(CityName,TargetCities1),member(Act,TargetActs1));(member(CityName,TargetCitiesContainLikedAct),member(Act,TargetLikedActivities))).


find_my_best_match(Name, Distances, Activities, Cities, Targets):-

	find_weighted_targets_wout_old_2(Name,TargetList),%create TargetList without old relations
	findall([TargetDistance,Act,CityName,TargetName],(target_city_act_2(Name,TargetName,CityName,Act,TargetDistance,TargetList)),List),
	sort(List,Sorted),
	%print(List),
	findall(D,(nth0(0,E,D),member(E,Sorted)), Distances),
	findall(A,(nth0(1,E,A),member(E,Sorted)), Activities),
	findall(C,(nth0(2,E,C),member(E,Sorted)), Cities),
	findall(T,(nth0(3,E,T),member(E,Sorted)), Targets).
