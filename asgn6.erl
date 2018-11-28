-module(asgn6).
-export([test/0]).

-include_lib("eunit/include/eunit.hrl").

% Note: in Erlang, records must have a lowercase name.
% Variables have capital letter names.

%TODO: values, actually getting interp working with ifc appc

% ExprCs
-record(numC, {val}). 
-record(idC, {name}).
-record(strC, {val}).
-record(ifC, {test, then, else}).
-record(lamC, {args, body}).
-record(appC, {func, args}).
-type exprC() :: #numC{} | #idC{} | #strC{} | #ifC{} | #lamC{} | #appC{}.

% Interprets an expression with a given environment
interp(Expr, Env) ->
   case Expr of
      #numC{val=N} -> N;
      #strC{val=S} -> S;
      #idC{name=Id} -> lookupEnv(Id, Env);
      % A lamC is really no different from a cloV
      #lamC{args=Args, body=Body} -> Expr;
      #ifC{test=Test, then=Then, else=Else} ->
         case interp(Test, Env) of
            true -> interp(Then, Env);
            false -> interp(Else, Env)
         end;
      #appC{func=Func, args=Args} -> "todo"
   end.


% TODO: Parse

% Creates a new top env
newTopEnv()->
   #{'+' => fun(X, Y) -> X + Y end, '-' => fun(X, Y) -> X - Y end, 
      '*' => fun(X, Y) -> X * Y end, '/' => fun(X, Y) -> X / Y end,
      '<=' => fun(X, Y) -> if X =< Y -> true; X > Y -> false end end,
      'equal?' => fun(X, Y) -> 
         if 
            is_boolean(X) and is_boolean(Y) ->
               X == Y;
            is_number(X) and is_number(Y) ->
               X == Y;
            %generalizing strings to lists, which should be ok?
            is_list(X) and is_list(Y) ->
               X == Y;
            true ->
               false
         end end}.

% Gets the value of a binding from the environment
lookupEnv(Needle, Haystack) ->
   IsInEnv = maps:is_key(Needle, Haystack),

   if
      IsInEnv ->
         maps:get(Needle, Haystack);

      true -> %works just like an else branch
         throw('ZIBR: Id not in environment')
   end.

% Helper function for extending the environment with a list of bindings and a list
% of values
multiExtendEnv(IdList, ValueList, Env) ->
   IdListLen = length(IdList),
   ValueListLen = length(ValueList),

   if
      IdListLen /= ValueListLen ->
         throw('ZIBR: arg length mismatch');

      IdListLen == 0 ->
         Env;

      true ->
         [IdFirst | IdRest] = IdList,
         [ValueFirst | ValueRest] = ValueList,
         multiExtendEnv(IdRest, ValueRest, 
            extendEnv(IdFirst, ValueFirst, Env))
   end.

% Returns a new environment with a new binding added to it
extendEnv(Id, Value, Env) ->
   maps:put(Id, Value, Env).

% Test cases
test()->
   io:fwrite("Test suite\n"),
   testBasic(),
   testEnv(),
   testInterp(),
   io:fwrite("Test suite complete\n").

% Basic ExprCs
testBasic() ->
   12 = #numC{val=12}#numC.val,
   $d = #idC{name=$d}#idC.name,
   "meow" = #strC{val="meow"}#strC.val,
   true = #ifC{test=true, then=true, else=false}#ifC.test,
   {#idC{name=$a}, #idC{name=$b}} = #lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}#lamC.args,
   {#numC{val=12}, #numC{val=3}} = #appC{func=#lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}, args={#numC{val=12}, #numC{val=3}}}#appC.args,
   io:fwrite("\tSuccess Basic ExprC Test\n").

% Basic Environment Tests
testEnv() ->
   12 = apply(lookupEnv('+', newTopEnv()), [4, 8]),
   0 = apply(lookupEnv('-', newTopEnv()), [4, 4]),
   16 = apply(lookupEnv('*', newTopEnv()), [4, 4]),
   4.0 = apply(lookupEnv('/', newTopEnv()), [16, 4]),
   false = apply(lookupEnv('equal?', newTopEnv()), [5, true]),
   false = apply(lookupEnv('equal?', newTopEnv()), [5, 4]),
   false = apply(lookupEnv('equal?', newTopEnv()), [true, false]),
   false = apply(lookupEnv('equal?', newTopEnv()), ["let's get", "this bread"]),
   true = apply(lookupEnv('equal?', newTopEnv()), ["bread", "bread"]),
   true = apply(lookupEnv('equal?', newTopEnv()), [false, false]),
   true = apply(lookupEnv('equal?', newTopEnv()), [5, 5]),
   8 = apply(lookupEnv('double', 
      extendEnv('double', fun(X) -> X * 2 end, newTopEnv())), [4]),
   12 = apply(lookupEnv('triple', 
      multiExtendEnv(['double', 'triple'], [fun(X) -> X * 2 end, fun(X) -> X * 3 end], newTopEnv())), [4]),
   ?assertException(throw, 'ZIBR: Id not in environment', 
      lookupEnv('\\', newTopEnv())),
   ?assertException(throw, 'ZIBR: arg length mismatch', 
      multiExtendEnv(['double', 'triple'], [fun(X) -> X * 2 end], newTopEnv())),
   io:fwrite("\tSuccess Env Test\n").

% Interp
testInterp() ->
   12 = interp(#numC{val=12}, newTopEnv()),
   "meow" = interp(#strC{val="meow"}, newTopEnv()),
   #lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}} = interp(#lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}, newTopEnv()),
   % TODO: Tests for appc, ifc
   io:fwrite("\tSuccess Interp Test\n").
