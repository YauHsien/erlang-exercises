-module(image_list).
-export(
  [ generate/4,
    generate/1
  ]).
-define(W, "w.png").
-define(B, "b,png").
-define(QiB, "qib.png").
-define(QiW, "qiw.png").

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

-spec generate(Queens :: [ 1|2|3|4|5|6|7|8 ]) -> [[string()]].

generate(Queens) when erlang:length(Queens) =:= 8 ->
    {_, List} =
        lists:foldl( fun(C, {R,Acc}) ->
                             {R+1, [lists:reverse(generate(R,C,8))|Acc]}
                     end,
                     {1, []},
                     Queens
                   ),
    lists:reverse(List).

-spec generate(R :: integer(), C :: integer(), S :: integer()) -> [string()].

generate(R, C, S) when S =:= 8 ->
    Rrem2 = R rem 2,
    {_, List} =
        lists:foldl( fun(_, {M,Acc}) when M =:= C ->
                             Tile =
                                 case {Rrem2, M rem 2} of
                                     {1, 1} -> ?QiW;
                                     {0, 1} -> ?QiB;
                                     {1, 0} -> ?QiB;
                                     {0, 0} -> ?QiW
                                 end,
                             {M+1, [Tile|Acc]};
                        (_, {M,Acc}) ->
                             Tile =
                                 case {Rrem2, M rem 2} of
                                     {1, 1} -> ?W;
                                     {0, 1} -> ?B;
                                     {1, 0} -> ?B;
                                     {0, 0} -> ?W
                                 end,
                             {M+1, [Tile|Acc]}
                     end,
                     {1, []},
                     string:chars($a, S)
                   ),
    List.
