-module(math_methods).

-export([loop_linear/3, loop_lagrange/4, loop_newton/4]).
-export([linear_func/2, lag_func/2, new_func/3]).

% Функции для линейной интерполяции

float_range(Step, First, Last) when First - Step < Last ->
    [First | float_range(Step, First + Step, Last)];
float_range(_, _, _) ->
    [].

linear_func(Step, [[X1, Y1], [X2, Y2]]) ->
    A = (Y2 - Y1) / (X2 - X1),
    B = Y1 - A * X1,
    XRange = float_range(Step, X1, X2),
    [XRange, lists:map(fun(X) -> A * X + B end, XRange)].

liner_handler(Points, Step, OutputPid) ->
    Mass = linear_func(Step, queue:to_list(Points)),
    OutputPid ! {result, {"Linear interpolation", Mass}, self()},
    Points.

loop_linear(Step, Points, OutputPid) ->
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    2 ->
                        liner_handler(queue:drop(
                                          queue:in(Point, Points)),
                                      Step,
                                      OutputPid);
                    1 ->
                        liner_handler(queue:in(Point, Points), Step, OutputPid);
                    0 ->
                        loop_linear(Step, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            _ ->
                loop_linear(Step, Points, OutputPid)
        end,
    loop_linear(Step, NewPoints, OutputPid).

% ********************************************************

% Получение координаты точки
coord(x, [X, _]) ->
    X;
coord(y, [_, Y]) ->
    Y.

% Функции для подсчёта лагранжа

lag_prod(X, Xi, Points) ->
    lag_prod(X, Xi, Points, 1).

lag_prod(_, _, [], Acc) ->
    Acc;
lag_prod(X, Xi, [[Xi, _] | Tail], Acc) ->
    lag_prod(X, Xi, Tail, Acc);
lag_prod(X, Xi, [[Xj, _] | Tail], Acc) ->
    Acc * ((X - Xj) / (Xi - Xj)) * lag_prod(X, Xi, Tail, Acc).

lag_poly(X, Points) ->
    lag_poly(X, Points, Points, 0).

lag_poly(_, _, [], Acc) ->
    Acc;
lag_poly(X, Points, [[Xi, Yi] | Tail], Acc) ->
    Acc + Yi * lag_prod(X, Xi, Points) + lag_poly(X, Points, Tail, Acc).

lag_func(Step, [[X1, Y1] | Tail]) ->
    XRange = float_range(Step, X1, coord(x, lists:last(Tail))),
    [XRange, lists:map(fun(X) -> lag_poly(X, [[X1, Y1] | Tail]) end, XRange)].

lag_handler(Points, Step, OutputPid) ->
    Mass = lag_func(Step, queue:to_list(Points)),
    OutputPid ! {result, {"Lagrange", Mass}, self()},
    Points.

loop_lagrange(Step, Window, Points, OutputPid) ->
    WindowPrev = Window - 1,
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    Window ->
                        lag_handler(queue:drop(
                                        queue:in(Point, Points)),
                                    Step,
                                    OutputPid);
                    WindowPrev ->
                        lag_handler(queue:in(Point, Points), Step, OutputPid);
                    _ ->
                        loop_lagrange(Step, Window, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            _ ->
                loop_lagrange(Step, Window, Points, OutputPid)
        end,
    loop_lagrange(Step, Window, NewPoints, OutputPid).

% ********************************************************

% Функции для метода Ньютона

diff(_, [], _) ->
    0;
diff(Index, Points, 0) ->
    coord(y, lists:nth(Index, Points));
diff(Index, Points, Order) ->
    (diff(Index + 1, Points, Order - 1) - diff(Index, Points, Order - 1))
    / (coord(x, lists:nth(Index + Order, Points)) - coord(x, lists:nth(Index, Points))).

new_prod(X, K, Points) ->
    new_prod(X, K, 1, Points, 1).

new_prod(X, K, K, Points, Acc) ->
    Acc * (X - coord(x, lists:nth(K, Points)));
new_prod(X, K, Curr, Points, Acc) ->
    new_prod(X, K, Curr + 1, Points, Acc * (X - coord(x, lists:nth(Curr, Points)))).

new_poly(X, Len, Points) ->
    new_poly(X, Len, Points, 1, 0).

new_poly(_, 0, Points, _, Acc) ->
    Acc + diff(1, Points, 0);
new_poly(X, Len, Points, K, Acc) ->
    new_poly(X, Len - 1, Points, K + 1, Acc + diff(1, Points, K) * new_prod(X, K, Points)).

new_func(Window, Step, [[X1, Y1] | Tail]) ->
    XRange = float_range(Step, X1, coord(x, lists:last(Tail))),
    [XRange, lists:map(fun(X) -> new_poly(X, Window - 1, [[X1, Y1] | Tail]) end, XRange)].

new_handler(Points, Window, Step, OutputPid) ->
    Mass = new_func(Window, Step, queue:to_list(Points)),
    OutputPid ! {result, {"Newton", Mass}, self()},
    Points.

loop_newton(Step, Window, Points, OutputPid) ->
    WindowPrev = Window - 1,
    NewPoints =
        receive
            {point, Point, _} ->
                case queue:len(Points) of
                    Window ->
                        new_handler(queue:drop(
                                        queue:in(Point, Points)),
                                    Window,
                                    Step,
                                    OutputPid);
                    WindowPrev ->
                        new_handler(queue:in(Point, Points), Window, Step, OutputPid);
                    _ ->
                        loop_newton(Step, Window, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            _ ->
                loop_newton(Step, Window, Points, OutputPid)
        end,
    loop_newton(Step, Window, NewPoints, OutputPid).
