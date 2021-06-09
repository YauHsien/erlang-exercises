-module('puzzle-8-queens').
-export(
   [ queens/0,
     queens/2,
     next/1
   ]).

-spec queens() -> [[integer()]].

queens() ->
    lists:map(
      fun(L) ->
              L1 = lists:foldr( fun({pos,N}, Acc) -> [N|Acc];
                                   ({direct,N}, Acc) -> [N|Acc];
                                   (_, Acc) -> Acc
                                end,
                                [],
                                L ),
              L1
      end,
      queens(8, [])
     ).

% ----- 內在處理 ----------------------
% 每一行有四種棋格的佔據情況：
% - {up, N} 表示皇后向上封鎖了第 N 棋格：
% - {direct, N} 表示皇后水平封鎖第 N 棋格：
% - {down, N} 表示皇后向下封鎖了第 N 棋格：
% - {pos, N} 表示皇后佔據了第 N 棋格。

-spec queens(
        N :: 0|1|2|3|4|5|6|7|8,
        State :: [{ Symbol::up|direct|down|pos, P::integer() }]
       ) ->
          [[integer()]].

queens(0, State) ->
    State;

queens(N, OldState) when 1 =< N andalso N =< 8 ->
    State = next(OldState),
    Cases =
        proplists:get_all_values(up, State) ++
        proplists:get_all_values(direct, State) ++
        proplists:get_all_values(down, State),
    States =
        lists:filter(
          fun([]) -> false; (_) -> true end,
          lists:map(
            fun(M) ->
                    case lists:any(fun(M1) when M1 =:= M -> true; (_) -> false end, Cases) of
                        true ->
                            [];
                        false ->
                            [{pos,M}|State]
                    end
            end,
            [1,2,3,4,5,6,7,8]
           )),
    Results =
        lists:map(
          fun(L) -> queens(N-1,L) end,
          States
         ),
    case N of
        1 ->
            Results;
        _ ->
            lists:append(Results)
    end.

next(State) ->
    lists:foldr(
      fun({up,P}, Acc) ->
              [{up,P-1}|Acc];
         ({direct,P}, Acc) ->
              [{direct,P}|Acc];
         ({down,P}, Acc) ->
              [{down,P+1}|Acc];
         ({pos,P}, Acc) ->
              [{up,P-1},{direct,P},{down,P+1}|Acc]
      end,
      [],
      State
     ).
