-module(main).

-export([main/1]).

main(Args) ->
    IOType = lists:nth(1, Args),
    WindowVal = list_to_integer(lists:nth(2, Args)),
    StepVal = list_to_float(lists:nth(3, Args)),
    [_, _, _ | Methods] = Args,
    OutputPid = spawn(io_methods, start_output, []),
    Workers = lists:map(
        fun(Method) ->
            AtomMethod = list_to_atom(Method),
            case AtomMethod of
                linear ->
                    PID = spawn(math_methods, loop_linear, [StepVal, queue:from_list([]), OutputPid]),
                    link(PID),
                    PID;
                lagrange ->
                    PID = spawn(math_methods, loop_lagrange, [StepVal, WindowVal, queue:from_list([]), OutputPid]),
                    link(PID),
                    PID;
                newton ->
                    PID = spawn(math_methods, loop_newton, [StepVal, WindowVal, queue:from_list([]), OutputPid]),
                    link(PID),
                    PID;
                _ ->
                    AtomMethod
            end
        end,
        Methods),
    wait(Workers, spawn(io_methods, start_input, [Workers, IOType]), OutputPid).

wait(Workers, IPid, OPid) ->
    case {erlang:process_info(IPid), erlang:process_info(OPid)} of
        {undefined, undefined} -> exit;
        _ -> wait(Workers, IPid, OPid)
    end.