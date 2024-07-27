-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((ENX) -> ENY), fun((ENY) -> ENZ)) -> fun((ENX) -> ENZ).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EOA, EOB) -> EOC)) -> fun((EOA) -> fun((EOB) -> EOC)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EOE, EOF, EOG) -> EOH)) -> fun((EOE) -> fun((EOF) -> fun((EOG) -> EOH))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EOJ, EOK, EOL, EOM) -> EON)) -> fun((EOJ) -> fun((EOK) -> fun((EOL) -> fun((EOM) -> EON)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EOP, EOQ, EOR, EOS, EOT) -> EOU)) -> fun((EOP) -> fun((EOQ) -> fun((EOR) -> fun((EOS) -> fun((EOT) -> EOU))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EOW, EOX, EOY, EOZ, EPA, EPB) -> EPC)) -> fun((EOW) -> fun((EOX) -> fun((EOY) -> fun((EOZ) -> fun((EPA) -> fun((EPB) -> EPC)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EPE, EPF) -> EPG)) -> fun((EPF, EPE) -> EPG).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EPH) -> EPH.
identity(X) ->
    X.

-spec constant(EPI) -> fun((any()) -> EPI).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EPK, fun((EPK) -> any())) -> EPK.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EPM) -> EPN), EPM) -> EPN.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EPO, EPP) -> EPQ), EPO, EPP) -> EPQ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EPR, EPS, EPT) -> EPU), EPR, EPS, EPT) -> EPU.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
