-module(main).

-export([main/1]).

main(Args) ->
    [IOType, Window, Step | Methods] = Args,
    WindowVal = list_to_integer(Window),
    StepVal = list_to_float(Step),
    OutputPid = spawn(io_methods, start_output, []),
    LagrangeMass = [StepVal, WindowVal, queue:from_list([]), OutputPid],
    Workers =
        lists:map(fun(Method) ->
                     AtomMethod = list_to_atom(Method),
                     case AtomMethod of
                         linear ->
                             spawn(math_methods,
                                   loop_linear,
                                   [StepVal, queue:from_list([]), OutputPid]);
                         lagrange ->
                             spawn(math_methods, loop_lagrange, LagrangeMass);
                         newton ->
                             spawn(math_methods, loop_newton, LagrangeMass);
                         _ ->
                             AtomMethod
                     end
                  end,
                  Methods),
    wait(Workers, spawn(io_methods, start_input, [Workers, IOType]), OutputPid).

wait(Workers, IPid, OPid) ->
    case {erlang:process_info(IPid), erlang:process_info(OPid)} of
        {undefined, undefined} ->
            exit;
        _ ->
            wait(Workers, IPid, OPid)
    end.
