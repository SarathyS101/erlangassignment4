% Team Members: [Sarathy Selvam PID 730770538, Lakshin Ganesha PID 730757493, Sushant Potu PID 730768373]
% Assignment 4 Erlang

-module(server_chain).
-export([start/0]).

start() ->
   % Spawn the servers in reverse order
   Serv3 = spawn(fun() -> serv3(0) end),
   Serv2 = spawn(fun() -> serv2(Serv3) end),
   Serv1 = spawn(fun() -> serv1(Serv2) end),
  
   io:format("Servers started, enter Erlang terms ending with a period.~n"),
   io:format("Example: {add, 5, 3}. or all_done.~n"),
   input_loop(Serv1).


% Read user input and pass it to Server 1
input_loop(Serv1Pid) ->
   Input = io:read(">>> "),
   case Input of
       {ok, all_done} ->
           Serv1Pid ! halt,
           io:format("Main loop ending.~n");
       {ok, Message} ->
           Serv1Pid ! Message,
           input_loop(Serv1Pid);
       _Other ->
           io:format("Error: Make sure to type a valid term and end with a period!~n"),
           input_loop(Serv1Pid)
   end.


%serv 1: Arithmetic Operations
serv1(NextPid) ->
   receive
       halt ->
           NextPid ! halt,
           io:format("(serv1) Halting.~n");
          
       {add, N1, N2} when is_number(N1), is_number(N2) ->
           io:format("(serv1) Add: ~p + ~p = ~p~n", [N1, N2, N1 + N2]),
           serv1(NextPid);
          
       {sub, N1, N2} when is_number(N1), is_number(N2) ->
           io:format("(serv1) Subtract: ~p - ~p = ~p~n", [N1, N2, N1 - N2]),
           serv1(NextPid);
          
       {mult, N1, N2} when is_number(N1), is_number(N2) ->
           io:format("(serv1) Multiply: ~p * ~p = ~p~n", [N1, N2, N1 * N2]),
           serv1(NextPid);
          
       {'div', _, 0} ->
           io:format("(serv1) Error: Division by zero~n"),
           serv1(NextPid);
          
       {'div', N1, N2} when is_number(N1), is_number(N2) ->
           io:format("(serv1) Divide: ~p / ~p = ~p~n", [N1, N2, N1 / N2]),
           serv1(NextPid);
          
       {neg, N} when is_number(N) ->
           io:format("(serv1) Negate: ~p = ~p~n", [N, -N]),
           serv1(NextPid);
          
       {sqrt, N} when is_number(N), N >= 0 ->
           io:format("(serv1) Sqrt ~p = ~p~n", [N, math:sqrt(N)]),
           serv1(NextPid);
          
       {sqrt, _N} ->
           io:format("(serv1) Error: Can’t calculate sqrt of a negative number~n"),
           serv1(NextPid);
          
       OtherMessage ->
           NextPid ! OtherMessage,
           serv1(NextPid)
   end.


%serv 2: List processing
serv2(NextPid) ->
   receive
       halt ->
           NextPid ! halt,
           io:format("(serv2) Halting.~n");
          
       [Head | _Tail] = List when is_integer(Head) ->
           Sum = sum_list(List),
           io:format("(serv2) Sum of int list = ~p~n", [Sum]),
           serv2(NextPid);
          
       [Head | _Tail] = List when is_float(Head) ->
           Product = multiply_list(List),
           io:format("(serv2) Product of Float list = ~p~n", [Product]),
           serv2(NextPid);
          
       OtherMessage ->
           NextPid ! OtherMessage,
           serv2(NextPid)
   end.


% helper to add numbers in a list
sum_list([]) -> 0;
sum_list([Head | Tail]) when is_number(Head) -> Head + sum_list(Tail);
sum_list([_ | Tail]) -> sum_list(Tail).


% helper to multiply numbers in a list
multiply_list([]) -> 1;
multiply_list([Head | Tail]) when is_number(Head) -> Head * multiply_list(Tail);
multiply_list([_ | Tail]) -> multiply_list(Tail).


%serv 3: Error handling and counter
serv3(Count) ->
   receive
       halt ->
           io:format("(serv3) Halting. Total unhandled messages: ~p~n", [Count]);
          
       {error, Reason} ->
           io:format("(serv3) Error caught: ~p~n", [Reason]),
           serv3(Count);
          
       OtherMessage ->
           io:format("(serv3) Not handled: ~p~n", [OtherMessage]),
           serv3(Count + 1)
   end.
