-module(chain_server).
-export([start/0, parse_input/1]).

start() ->
    % Starts the servers in reverse order
    Serv3Pid = spawn(fun() -> serv3_loop(none, 0) end),
    Serv2Pid = spawn(fun() -> serv2_loop(Serv3Pid) end),
    Serv1Pid = spawn(fun() -> serv1_loop(Serv2Pid) end),

    % input reading loop
    read_loop(Serv1Pid).

% Read user input and send to serv1
read_loop(Serv1Pid) ->
    Input = io:get_line(">>> "),
    case Input of
        eof ->
            ok;
        Line ->
            case parse_input(Line) of
                {ok, all_done} ->
                    Serv1Pid ! halt,
                    ok;
                {ok, Term} ->
                    Serv1Pid ! Term,
                    read_loop(Serv1Pid);
                {error, ErrorMsg} ->
                    io:format("Error: ~s~n", [ErrorMsg]),
                    read_loop(Serv1Pid)
            end
    end.

% Convert user input into Erlang 
parse_input(Input) ->
    CleanInput = string:trim(Input),
    InputWithDot = case CleanInput of
        "" -> "";
        _ ->
            case lists:last(CleanInput) of
                $. -> CleanInput;
                _ -> CleanInput ++ "."
            end
    end,

    case InputWithDot of
        "" -> {error, "Empty input"};
        _ ->
            % Replace reserved keywords with quoted versions
            QuotedInput = quote_reserved_keywords(InputWithDot),
            case erl_scan:string(QuotedInput) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, Term} -> {ok, Term};
                        {error, _} -> {error, "Parse error"}
                    end;
                {error, _} -> {error, "Scan error"}
            end
    end.

% Quote reserved Erlang keywords so they can be used as atoms
quote_reserved_keywords(Input) ->
    ReservedWords = ['div', 'rem', 'mod', 'band', 'bor', 'bxor', 'bsl', 'bsr', 'and', 'or', 'not'],
    quote_keywords_list(Input, ReservedWords).

% Recursively quotes keywords
quote_keywords_list(Input, []) ->
    Input;
quote_keywords_list(Input, [Keyword|Rest]) ->
    KeywordStr = atom_to_list(Keyword),
    QuotedStr = "'" ++ KeywordStr ++ "'",
    QuotedInput = replace_keyword_simple(Input, KeywordStr, QuotedStr),
    quote_keywords_list(QuotedInput, Rest).

% Replaces keyword when surrounded by non alphanumeric characters
replace_keyword_simple(Input, Keyword, Quoted) ->
    case string:find(Input, Keyword) of
        nomatch ->
            Input;
        _ ->
            do_replace_keyword(Input, Keyword, Quoted, [])
    end.

% Character by character replacement
do_replace_keyword([], _, _, Acc) ->
    lists:reverse(Acc);
do_replace_keyword(Input, Keyword, Quoted, Acc) ->
    KeyLen = length(Keyword),
    case Input of
        [Char|Rest] when Char == ${; Char == $[; Char == $( ->
            case Rest of
                L when length(L) >= KeyLen ->
                    case lists:prefix(Keyword, L) of
                        true ->
                            % Check if keyword is followed by non alphanumeric
                            RestAfter = lists:nthtail(KeyLen, L),
                            case RestAfter of
                                [C|_] when (C >= $a andalso C =< $z) orelse
                                          (C >= $A andalso C =< $Z) orelse
                                          (C >= $0 andalso C =< $9) orelse
                                          C == $_ ->
                                    do_replace_keyword(Rest, Keyword, Quoted, [Char|Acc]);
                                _ ->
                                    % Replace the keyword
                                    NewAcc = lists:reverse(Quoted) ++ [Char|Acc],
                                    do_replace_keyword(RestAfter, Keyword, Quoted, NewAcc)
                            end;
                        false ->
                            do_replace_keyword(Rest, Keyword, Quoted, [Char|Acc])
                    end;
                _ ->
                    do_replace_keyword(Rest, Keyword, Quoted, [Char|Acc])
            end;
        _ ->
            do_replace_keyword(tl(Input), Keyword, Quoted, [hd(Input)|Acc])
    end.

% Server 1: Arithmetic operations
serv1_loop(Serv2Pid) ->
    receive
        % Binary operations
        {add, N1, N2} ->
            Result = N1 + N2,
            io:format("(serv1) add ~w ~w = ~w~n", [N1, N2, Result]),
            serv1_loop(Serv2Pid);
        {sub, N1, N2} ->
            Result = N1 - N2,
            io:format("(serv1) sub ~w ~w = ~w~n", [N1, N2, Result]),
            serv1_loop(Serv2Pid);
        {mult, N1, N2} ->
            Result = N1 * N2,
            io:format("(serv1) mult ~w ~w = ~w~n", [N1, N2, Result]),
            serv1_loop(Serv2Pid);
        {'div', N1, 0} ->
            io:format("(serv1) div ~w 0 = Error: Division by zero~n", [N1]),
            serv1_loop(Serv2Pid);
        {'div', N1, N2} ->
            Result = N1 / N2,
            io:format("(serv1) div ~w ~w = ~w~n", [N1, N2, Result]),
            serv1_loop(Serv2Pid);
        % Unary operations
        {neg, N} ->
            Result = -N,
            io:format("(serv1) neg ~w = ~w~n", [N, Result]),
            serv1_loop(Serv2Pid);
        {sqrt, N} when N < 0 ->
            io:format("(serv1) sqrt ~w = Error: Negative square root~n", [N]),
            serv1_loop(Serv2Pid);
        {sqrt, N} ->
            Result = math:sqrt(N),
            io:format("(serv1) sqrt ~w = ~w~n", [N, Result]),
            serv1_loop(Serv2Pid);
        halt ->
            io:format("(serv1) halting~n"),
            Serv2Pid ! halt;
        Msg ->
            Serv2Pid ! Msg,
            serv1_loop(Serv2Pid)
    end.

% Server 2: List processing
serv2_loop(Serv3Pid) ->
    receive
        [H|_] = List when is_number(H) ->
            case is_integer(H) of
                true ->
                    % Sum for integer head
                    Result = lists:sum(List),
                    io:format("(serv2) sum of ~w = ~w~n", [List, Result]),
                    serv2_loop(Serv3Pid);
                false ->
                    % Product for float head
                    Result = lists:foldl(fun(X, Acc) -> X * Acc end, 1, List),
                    io:format("(serv2) product of ~w = ~w~n", [List, Result]),
                    serv2_loop(Serv3Pid)
            end;
        halt ->
            io:format("(serv2) halting~n"),
            Serv3Pid ! halt;
        Msg ->
            Serv3Pid ! Msg,
            serv2_loop(Serv3Pid)
    end.

% Server 3: Error handling and unhandled message counter
serv3_loop(_, Count) ->
    receive
        {error, Message} ->
            io:format("(serv3) Error: ~w~n", [Message]),
            serv3_loop(none, Count);
        halt ->
            io:format("(serv3) Unhandled message count: ~w~n", [Count]),
            io:format("(serv3) halting~n");
        Msg ->
            io:format("(serv3) Not handled: ~w~n", [Msg]),
            serv3_loop(none, Count + 1)
    end.
