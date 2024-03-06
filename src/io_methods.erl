-module(io_methods).

-export([start_input/2, start_output/0]).

% Обработка точки
parse_point(Line) ->
    [case string:to_float(Num) of
         {error, no_float} ->
             list_to_integer(Num);
         {Float, _} ->
             Float
     end
     || Num
            <- string:tokens(
                   string:trim(Line), " ")].

% Вывод чисел
print_number(Nums) ->
    Message =
        [lists:join("\t",
                    lists:map(fun(N) -> erlang:float_to_list(float(N), [{decimals, 2}]) end,
                              Nums))],
    %io:format("~s~n", Message),
    Message.

% Цикл ввода
loop_input(Workers, Dev) ->
    case io:get_line(Dev, "") of
        eof ->
            [Pid ! {stop, nil, self()} || Pid <- Workers];
        Line ->
            [Pid ! {point, parse_point(Line), self()} || Pid <- Workers]
    end,
    loop_input(Workers, Dev).

% Цикл вывода
loop_output() ->
    receive
        {result, {Name, [X, Y]}, _} ->
            %io:format(Name ++ "~n"),
            Name,
            print_number(X),
            print_number(Y);
        Msg ->
            Msg
    end,
    loop_output().

start_input(Workers, IOType) ->
    case IOType of
        "stdio" ->
            loop_input(Workers, standard_io);
        Filename ->
            {ok, Dev} = file:open(Filename, [read]),
            loop_input(Workers, Dev)
    end.

start_output() ->
    loop_output().
