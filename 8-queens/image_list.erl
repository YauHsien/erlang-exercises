-module(image_list).
-export(
  [ generate/4,
    generate_image/4
  ]).
-include_lib("wx/include/wx.hrl").
-define(W, "w.png").
-define(B, "b.png").
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

                                        lists:map( fun({gap,N}) -> {gap,N};
                                                      ({N1,L1}) -> {erlang:length(L)-N1, L1}
                                                   end,
                                                   Data4)
                                end,
                                % 每二欄之間加上 gap

                                Data2
                              )
                   ),
    lists:map(F, Data3),

    ok.

-spec generate_image( QueensSet :: [[ 1|2|3|4|5|6|7|8 ]],
                      CountInRow :: integer(),
                      GapSize :: float(),
                      FileName :: string()
                    ) -> [[string()]].

%% QueensSet:  執行 'puzzle-8-queens':queens() 所得的答案；
%% CountInRow: 指明一列要有多少欄；
%% GapSize:    依棋格寬度，指明一個比例值，表示二欄之間的間距。

% 使用這段程式之前，要先執行 wx:new() 啟動 Wx 環境
generate_image([Queens|_]=QueensSet, CountInRow, GapSize, FileName) when is_list(Queens) andalso erlang:length(Queens) =:= 8 ->

    wxBitmap:loadFile( B =wxBitmap:new(), ?B, [{type,?wxBITMAP_TYPE_PNG}]),
    wxBitmap:loadFile( W = wxBitmap:new(), ?W, [{type,?wxBITMAP_TYPE_PNG}]),
    wxBitmap:loadFile( QiB = wxBitmap:new(), ?QiB, [{type,?wxBITMAP_TYPE_PNG}]),
    wxBitmap:loadFile( QiW = wxBitmap:new(), ?QiW, [{type,?wxBITMAP_TYPE_PNG}]),

    TileWidth = wxBitmap:getWidth(B),
    GapSize1 = list_to_integer(float_to_list( TileWidth * GapSize, [{decimals,0}])),

    BoardWidth = BoardHeight = TileWidth * 8,
    ImageWidth = CountInRow * BoardWidth + (CountInRow-1) * GapSize1,

    Count =
        erlang:length(QueensSet) div CountInRow +
        case (erlang:length(QueensSet) rem CountInRow) of 0 -> 0; _ -> 1 end,
    ImageHeight = Count * BoardWidth + (Count-1) * GapSize1,

    OutBitmap = wxBitmap:new( OutImage = wxImage:new({ImageWidth,ImageHeight})),
    wxMemoryDC:selectObject( DC = wxMemoryDC:new(), OutBitmap),

    generate( fun({R, L}) ->
                      lists:map( fun({gap, GapSize2}) ->
                                         ok;
                                    ({C, Board}) ->
                                         lists:map( fun(Row) ->
                                                            lists:map( fun({R1,C1,FN}) ->
                                                                               wxDC:drawBitmap(
                                                                                 DC,
                                                                                 case FN of
                                                                                     ?B -> B;
                                                                                     ?W -> W;
                                                                                     ?QiB -> QiB;
                                                                                     ?QiW -> QiW
                                                                                 end,
                                                                                 {(BoardHeight+GapSize1) * C + C1 * TileWidth,
                                                                                  (BoardWidth+GapSize1) * R + R1 * TileWidth
                                                                                 }
                                                                                )
                                                                       end,
                                                                       Row
                                                                     )
                                                    end,
                                                    Board
                                                  )
                                 end,
                                 % 由資料轉換為貼圖

                                 L
                               )
              end,
              % 由資料轉換為貼圖

              CountInRow,
              GapSize1,

              lists:map(fun generate/1, QueensSet)
            ),

    wxBitmap:saveFile(OutBitmap, FileName, ?wxBITMAP_TYPE_PNG),
    wxBitmap:destroy(OutBitmap),
    wxBitmap:destroy(B),
    wxBitmap:destroy(W),
    wxBitmap:destroy(QiB),
    wxBitmap:destroy(QiW),
    wxImage:destroy(OutImage),

    QueensSet.

-spec generate( Queens :: [ 1|2|3|4|5|6|7|8 ]
              ) -> [[string()]].

generate(Queens) when is_list(Queens) andalso erlang:length(Queens) =:= 8 ->
    {_, List} =
        lists:foldl( fun(C, {R,Acc}) ->
                             {R+1, [lists:reverse(generate(R,C,8))|Acc]}
                     end,
                     {1, []},
                     Queens
                   ),
    lists:reverse(List).

-spec generate(R :: integer(), C :: integer(), S :: integer())
              ->
          [{Row :: integer(), Column :: integer(), FileName :: string()}].

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
                             {M+1, [{R,C,Tile}|Acc]};
                        (_, {M,Acc}) ->
                             Tile =
                                 case {Rrem2, M rem 2} of
                                     {1, 1} -> ?W;
                                     {0, 1} -> ?B;
                                     {1, 0} -> ?B;
                                     {0, 0} -> ?W
                                 end,
                             {M+1, [{R,C,Tile}|Acc]}
                     end,
                     {1, []},
                     string:chars($a, S)
                   ),
    List.
