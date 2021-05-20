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
find_weighted_targets_wout_old(Name,Dists,TargetList,OldRelations):-
	expects(Name,ExpectedGenders,_),
	%dislikes(Name,_,_,Limits),
	%findall(TargetName,(glanian(TargetName,TargetGender,_),(member(TargetGender,ExpectedGenders),\+member(TargetGender,OldRelations))), TargetList),
	findall([TargetDistance,TargetName],(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),\+member(TargetGender,OldRelations),weighted_glanian_distance(Name,TargetName,TargetDistance)), T),
   	sort(T,SortedT),
    findall(Distance,(nth0(0,E,Distance),member(E,SortedT)), Dists),
    findall(TargetName,(nth0(1,E,TargetName),member(E,SortedT)), TargetList).
appropriate_cities(Name,CityName):-
	likes(Name,LikedActivities,_),
	dislikes(Name,_,DislikedCities,_),
	city(CityName,_,ActList),member(Act,LikedActivities),member(Act,ActList),\+member(CityName,DislikedCities).
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
target_city_act(Name,TargetName,CityName,Act,TargetDistance,Dists,TargetList,Cities1,Acts1,CitiesContainLikedAct):-
	%find a Target thats in limits and doesnt have too much conflict, get its distance
	likes(Name,LikedActivities,_),
	dislikes(Name,DislikedActivities,_,Limits),
	member(TargetName,TargetList),limit_check(TargetName,Limits),matches(DislikedActivities,TargetName),
	nth0(Ind,TargetList,TargetName),nth0(Ind,Dists,TargetDistance),

	%find possible cities that target and name both like
	merge_possible_cities(Name,TargetName,Cities),
	%find the acts that can be done in those cities
		%findall(A,possible_acts(A,Cities),Acts2),
		%(member(Act,Acts1),member(Act,Acts2)),%found acts should be included to the liked acts of Name
	%find a city that is in both common cities of target and name ,and possible cities of name,	that has one of the found activities included, not disliked one
	city(CityName,_,ActList),member(CityName,Cities),member(Act,ActList),\+member(Act,DislikedActivities),
	(member(CityName,Cities1),member(Act,Acts1));(member(CityName,CitiesContainLikedAct),member(Act,LikedActivities)).
%find_my_best_target(josizar,D,A,C,T).
possible_acts(Act,Cities1):-
	member(City,Cities1),
	city(City,_,ActList),
	member(Act,ActList).
find_my_best_target(Name, Distances, Activities, Cities, Targets):-

	findall(Relation,(old_relation([Name,Relation]);old_relation([Relation,Name])), OldRelations),%find old Relations
	find_weighted_targets_wout_old(Name,Dists,TargetList,OldRelations),%create TargetList without old relations

	%find cities that Name lives in, likes or likes an activity in.  
	find_possible_cities(Name,Cities1),
	findall(Act,(city(CN,_,ActList),member(CN,Cities1),member(Act,ActList)),Acts1),

	findall(CityName,appropriate_cities(Name,CityName),CitiesContainLikedAct),
	%findall(ActN,(nth0(0,E,ActN),member(E,Cities2)), Acts2),
	
	%find possible acts Name likes to do.

	findall([TargetDistance,Act,CityName,TargetName],(target_city_act(Name,TargetName,CityName,Act,TargetDistance,Dists,TargetList,Cities1,Acts1,CitiesContainLikedAct)),List),
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
find_weighted_targets_wout_old_2(Name,Dists,TargetList,OldRelations):-
	expects(Name,ExpectedGenders,_),
	%dislikes(Name,_,_,Limits),
	%findall(TargetName,(glanian(TargetName,TargetGender,_),(member(TargetGender,ExpectedGenders),\+member(TargetGender,OldRelations))), TargetList),
	findall([TargetDistance,TargetName],(glanian(TargetName,TargetGender,_),member(TargetGender,ExpectedGenders),target_gender_exp(TargetName,Name),\+member(TargetGender,OldRelations),weighted_glanian_distance(Name,TargetName,TargetDistance)), T),
   	sort(T,SortedT),
   	%	nth0(1,E,TargetName),nth0(0,E,Distance),member(E,SortedT).
    findall(Distance,(nth0(0,E,Distance),member(E,SortedT)), Dists),
    findall(TargetName,(nth0(1,E,TargetName),member(E,SortedT)), TargetList).
target_city_act_2(Name,TargetName,CityName,Act,TargetDistance,Dists,TargetList,Cities1,Acts1,CitiesContainLikedAct):-
	%find a Target thats in limits and doesnt have too much conflict, get its distance
	likes(Name,LikedActivities,_),
	likes(TargetName,TargetLikedActivities,_),
	dislikes(Name,DislikedActivities,DislikedCities,Limits),
	dislikes(TargetName,TargetDislikedActivities,TargetDislikedCities,TargetLimits),
	member(TargetName,TargetList),limit_check(TargetName,Limits),limit_check(Name,TargetLimits),matches(DislikedActivities,TargetName),matches(TargetDislikedActivities,Name),
	nth0(Ind,TargetList,TargetName),nth0(Ind,Dists,D1),
	weighted_glanian_distance(TargetName,Name,D2),TargetDistance is (D1+D2)/2,
	%for target
	

	find_possible_cities(TargetName,TargetCities1),
	findall(Act,(city(CN,_,ActList),member(CN,TargetCities1),member(Act,ActList)),TargetActs1),

	findall(CityName,appropriate_cities(TargetName,CityName),TargetCitiesContainLikedAct),
	%find possible acts Name can do.
	%findall(TargetAct,possible_acts(TargetAct,TargetMergedCities),TargetActs),
	%findall(NameAct,possible_acts(NameAct,MergedCities),Acts),
	%likes(Name,LikedActivities,_),
	%likes(TargetName,TargetLikedActivities,_),

	%find possible cities that target and name both like
	merge_possible_cities(Name,TargetName,Cities),
	%find the acts that can be done in those cities
	%findall(A,possible_acts(A,Cities),Acts2),
	%(member(Act,Acts1),member(Act,Acts2),member(Act,TargetActs1)),%found acts should be included to the liked acts of Name

	%findall(Act,(member(CityName,Cities),member(CityName,MergedCities),member(CityName,TargetMergedCities),city(CityName,_,ActList)),MutualActivities),

	%list_to_set(Cities,S1),list_to_set(MergedCities,S2),list_to_set(TargetMergedCities,S3),
   	%intersection(MergedCities,TargetMergedCities,MutualCities),
    %findall(MutualAct,(member(C,MutualCities),city(C,_,AList),member(MutualAct,AList),member(MutualAct,LikedActivities),member(MutualAct,TargetLikedActivities)),MutualActivities),
    %length(MutualActivities,MLen),MLen>0,
	%find a city that is in both common cities of target and name ,and possible cities of name,	that has one of the found activities included, not disliked one

	city(CityName,_,ActList),member(CityName,Cities),member(Act,ActList),\+member(CityName,DislikedCities),\+member(CityName,TargetDislikedCities),\+member(Act,DislikedActivities),\+member(Act,TargetDislikedActivities),
	((member(CityName,Cities1),member(Act,Acts1));(member(CityName,CitiesContainLikedAct),member(Act,LikedActivities))),((member(CityName,TargetCities1),member(Act,TargetActs1));(member(CityName,TargetCitiesContainLikedAct),member(Act,TargetLikedActivities))).


find_my_best_match(Name, Distances, Activities, Cities, Targets):-

	findall(Relation,(old_relation([Name,Relation]);old_relation([Relation,Name])), OldRelations),%find old Relations
	find_weighted_targets_wout_old_2(Name,Dists,TargetList,OldRelations),%create TargetList without old relations

	%find cities that Name lives in, likes or likes an activity in.  

	find_possible_cities(Name,Cities1),
	findall(Act,(city(CN,_,ActList),member(CN,Cities1),member(Act,ActList)),Acts1),

	findall(CityName,appropriate_cities(Name,CityName),CitiesContainLikedAct),


	%find possible acts Name can do.
	%findall(Act,possible_acts(Act,MergedCities),Acts),
	%print(TargetList),
	findall([TargetDistance,Act,CityName,TargetName],(target_city_act_2(Name,TargetName,CityName,Act,TargetDistance,Dists,TargetList,Cities1,Acts1,CitiesContainLikedAct)),List),
	sort(List,Sorted),
	%print(List),
	findall(D,(nth0(0,E,D),member(E,Sorted)), Distances),
	findall(A,(nth0(1,E,A),member(E,Sorted)), Activities),
	findall(C,(nth0(2,E,C),member(E,Sorted)), Cities),
	findall(T,(nth0(3,E,T),member(E,Sorted)), Targets).