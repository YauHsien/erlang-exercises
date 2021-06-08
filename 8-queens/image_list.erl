-module(image_list).
-export(
  [ generate/4
  ]).

-spec generate(F :: function(), Num :: integer(), GapSize :: integer(), Data :: list()) -> ok.

generate(F, Num, GapSize, Data) ->
    % 將資料 Data 分行列
    {_, Data1} =
        lists:foldl( fun(A, {N,Acc}) when N rem Num =:= 0 ->
                             {N+1, [[A]|Acc]};
                        (A, {N,[L|Acc]}) ->
                             {N+1, [[A|L]|Acc]}
                     end,
                     % 分列

                     {0, []},

                     Data
                   ),

    Data2 = lists:reverse(Data1),

    logger:alert("~p~n", [Data2]),

    {_, Data3} =
        lists:foldr( fun(A,{N,Acc}) -> {N-1,[{N,A}|Acc]} end,
                     % 加上 index

                     {erlang:length(Data2)-1, []},

                     lists:map( fun(L) ->
                                       {_, [_|Data4]} =
                                            lists:foldl( fun(A, {N,Acc}) ->
                                                                 {N+1, [{gap,GapSize},{N+1,A}|Acc]}
                                                         end,
                                                         % 每二欄之間加上 gap 並且將每一欄標號

                                                         {0, []},

                                                         L),

                                        lists:map(fun({gap, N}) -> {gap, N};
                                                     ({N1, L1}) -> {erlang:length(L)-N1, L1}
                                                  end,
                                                  Data4)
                                end,
                                % 每二欄之間加上 gap

                                Data2
                              )
                   ),

    logger:alert("~p~n", [Data3]),
    
    ok.
