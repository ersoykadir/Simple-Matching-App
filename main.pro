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
    Cities = [City1,LikedCities],
    append(Cities,TempList),%this might not work, need to use a set to avoid repetition
    list_to_set(TempList,CityList).

% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points
merge_possible_cities(Name1, Name2, MergedCities):-
	find_possible_cities(Name1, CityList1),
	find_possible_cities(Name2, CityList2),
	Cities = [CityList1,CityList2],
	append(Cities,TempList),
	list_to_set(TempList,MergedCities).

% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points
find_mutual_activities(Name1, Name2, MutualActivities):-
	likes(Name1,LikedActivities1,_),
    likes(Name2,LikedActivities2,_),
    findall(Mutual,(member(Mutual,LikedActivities1),member(Mutual,LikedActivities2)), MutualActivities).

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

% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points
