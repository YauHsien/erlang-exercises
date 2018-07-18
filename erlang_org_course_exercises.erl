-module(erlang_org_course_exercises).
-export([double/1]).
-export([convert/1, perimeter/1]).
-export([min/1, max/1, min_max/1]).
-export([swedish_date/0]).
-export([fore_back/1, msg_sender/2]).

-compile({no_auto_import, [min/2, max/2]}).

%% Problem set: http://erlang.org/course/exercises.html
%%
%%

%%
%% ### Entering a program
%%
%% Type the demo:double example into a file called demo.erl. Use your favourite text editor.
%%
%% Start Erlang.
%% Give the command c:c(demo). to compile the file.
%% Try running the query:
%%     demo:double(12).
%%
%% This is just to test if you can get the system started and can use the editor together with the Erlang system.
%%
double(X) ->
    X * 2.

f2c(F) ->
    5 * (F-32) / 9.0.

c2f(C) ->
    9 * C / 5.0 + 32.

convert({c,C}) ->
    {f, c2f(C)};
convert({f,F}) ->
    {c, f2c(F)}.

perimeter({square,Side}) ->
    4 * Side;
perimeter({circle,Radius}) ->
    Radius * Radius * math:pi();
perimeter({triangle,A,B,C}) ->
    A + B + C.

%% Write a function lists1:min(L) which returns the mini- mum element of the list L.
min(List) ->
    min(List, nil).

min([H|L], nil) ->
    min(L, H);
min([H|L], Acc) when Acc /= nil andalso H < Acc ->
    min(L, H);
min([_|L], Acc) when Acc /= nil ->
    min(L, Acc);
min([], Acc) ->
    Acc.

%% Write a function list1:max(L) which returns the maximum element of the list L.
max(List) ->
    max(List, nil).

max([], Acc) ->
    Acc;
max([H|L], nil) ->
    max(L, H);
max([H|L], Acc) when Acc /= nil andalso H > Acc ->
    max(L, H);
max([_|L], Acc) when Acc /= nil ->
    max(L, Acc).

%% Write a function list1:min_max(L) when returns a tuple containing the min and max of the list L.
min_max(List) ->
    min_max(List, {nil, nil}).

min_max([], Acc) ->
    Acc;
min_max([H|L], {nil, nil}) ->
    min_max(L, {H, H});
min_max([H|L], {Acc1, Acc2}) when H > Acc2 ->
    min_max(L, {Acc1, H});
min_max([H|L], {Acc1, Acc2}) when H < Acc1 ->
    min_max(L, {H, Acc2});
min_max([_|L], Acc) ->
    min_max(L, Acc).

%% Write the function time:swedish_date() which returns a string containing the date in swedish YYMMDD format:
%%  > time:swedish_date()
%%  "080901"
swedish_date() ->
    {Y,M,D} = erlang:date(),
    erlang:integer_to_list((Y - 2000) * 10000 + M * 100 + D).

%% Write a function which starts 2 processes, and sends a message M times forewards and backwards between them. After the messages have been sent the processes should terminate gracefully.
fore_back(M) ->
    Pid0 = self(),
    io:fwrite("me, ~p~n", [Pid0]),
    {Pid1, Pid2} = {spawn(?MODULE, msg_sender, [M, Pid0]),
		    spawn(?MODULE, msg_sender, [M, Pid0])},
    Pid0 = self(),
    Pid1 ! {?MODULE, Pid0, Pid2},
    Pid2 ! {?MODULE, Pid0, Pid1},
    Pid1 ! {?MODULE, Pid0, go},
    wait_fore_back(Pid0, nil).

wait_fore_back(Controller, Pid) ->
    receive
	{?MODULE, Pid1, Msg} when Msg == complete orelse Msg == bad_end ->
	    io:fwrite("Message from ~p: ~p~n", [Pid1, Msg]),
	    Pid1 ! {?MODULE, Controller, stop},
	    case {Pid, Pid1} of
		{nil, _} ->
		    wait_fore_back(Controller, Pid1);
		_ ->
		    ok
	    end;
	{?MODULE, PidSending, PidReceiving, Value} ->
	    io:fwrite("~p sends to ~p with message ~p ~n", [PidSending, PidReceiving, Value]),
	    wait_fore_back(Controller, Pid);
	Msg ->
	    io:fwrite("message ~p passes by~n", [Msg])
    end.

msg_sender(M, Controller) ->
    receive
	{?MODULE, Controller, Recipient} ->
	    receive
		{?MODULE, Controller, go} ->
		    msg_sender(M, Controller, Recipient);
		{?MODULE, Recipient, N} ->
		    Controller ! {?MODULE, Recipient, self(), N},
		    msg_sender(M, Controller, Recipient)
	    after
		2000 ->
		    {?MODULE, Controller, bad_end}
	    end
    after
	5000 ->
	    {?MODULE, Controller, bad_end}
    end.

msg_sender(M, Controller, Recipient) ->
    Recipient ! {?MODULE, self(), M},
    receive
	{?MODULE, Controller, stop} ->
	    io:fwrite("~p stops~n", [self()]);
	{?MODULE, Recipient, 0} ->
	    Recipient ! {?MODULE, self(), 0},
	    Controller ! {?MODULE, self(), complete};
	{?MODULE, Recipient, N} ->
	    Controller ! {?MODULE, Recipient, self(), N},
	    msg_sender(M-1, Controller, Recipient)
    after
	2000 ->
	    {?MODULE, Controller, bad_end}
    end.
