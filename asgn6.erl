-module(asgn6).
-export([test/0]).

-include_lib("eunit/include/eunit.hrl").

% Note: in Erlang, records must have a lowercase name.
% Variables have capital letter names.

% ExprCs
-record(numC, {val}). 
-record(idC, {name}).
-record(strC, {val}).
-record(ifC, {test, then, else}).
-record(lamC, {args, body}).
-record(appC, {func, args}).
-record(closV, {args, body, env}).
-record(primopV, {op}).
%TODO: boolV's (needed to get interp working with appC's of ifC's)

% appears to not be needed:
%-type exprC() :: #numC{} | #idC{} | #strC{} | #ifC{} | #lamC{} | #appC{}.

% Interprets an expression with a given environment
interp(Expr, Env) ->
   case Expr of
      #numC{val=N} -> N;
      #strC{val=S} -> S;
      #idC{name=Id} -> lookupEnv(Id, Env);
      #lamC{args=Args, body=Body} -> #closV{args=Args, body=Body, env=Env};
      #ifC{test=Test, then=Then, else=Else} ->
         case interp(Test, Env) of
            true -> interp(Then, Env);
            false -> interp(Else, Env)
         end;
      #appC{func=Func, args=Args} ->
         case interp(Func, Env) of
            #primopV{op=Op} ->
               Op(interp(lists:nth(1, Args), Env), interp(lists:nth(2, Args), Env));
            #closV{args=Cargs, body=Cbody, env=Cenv} ->
               Argvals=lists:map(fun(Arg) -> interp(Arg, Env) end, Args),
               Env2=multiExtendEnv(Cargs, Argvals, Cenv),
               interp(Cbody, Env2)
            end
   end.

% TODO: Parse

% Creates a new top env
newTopEnv()->
   #{'+' => #primopV{op=fun(X, Y) -> X + Y end},
      '-' => #primopV{op=fun(X, Y) -> X - Y end}, 
      '*' => #primopV{op=fun(X, Y) -> X * Y end},
      '/' => #primopV{op=fun(X, Y) -> X / Y end},
      '<=' => #primopV{op=fun(X, Y) -> if X =< Y -> true; X > Y -> false end end},
      'equal?' => #primopV{op=fun(X, Y) -> 
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
         end end}
      %TODO: add 'true' and 'false' to environment
      }.

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
   io:fwrite("\trunning basic tests\n"),
   12 = #numC{val=12}#numC.val,
   $d = #idC{name=$d}#idC.name,
   "meow" = #strC{val="meow"}#strC.val,
   true = #ifC{test=true, then=true, else=false}#ifC.test,
   {#idC{name=$a}, #idC{name=$b}} = #lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}#lamC.args,
   {#numC{val=12}, #numC{val=3}} = #appC{func=#lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}, args={#numC{val=12}, #numC{val=3}}}#appC.args,
   io:fwrite("\tSuccess Basic ExprC Test\n").

% Basic Environment Tests
testEnv() ->
   io:fwrite("\trunning Env tests\n"),
   Topenv=newTopEnv(),
   Plus=lookupEnv('+', Topenv),
   Minus=lookupEnv('-', Topenv),
   Multiply=lookupEnv('*', Topenv),
   Divide=lookupEnv('/', Topenv),
   Eq=lookupEnv('equal?', Topenv),
   12 = apply(Plus#primopV.op, [4, 8]),
   0 = apply(Minus#primopV.op, [4, 4]),
   16 = apply(Multiply#primopV.op, [4, 4]),
   4.0 = apply(Divide#primopV.op, [16, 4]),
   false = apply(Eq#primopV.op, [5, true]),
   false = apply(Eq#primopV.op, [5, 4]),
   false = apply(Eq#primopV.op, [true, false]),
   false = apply(Eq#primopV.op, ["let's get", "this bread"]),
   true = apply(Eq#primopV.op, ["bread", "bread"]),
   true = apply(Eq#primopV.op, [false, false]),
   true = apply(Eq#primopV.op, [5, 5]),
   8 = apply(lookupEnv('double', 
      extendEnv('double', fun(X) -> X * 2 end, Topenv)), [4]),
   12 = apply(lookupEnv('triple', 
      multiExtendEnv(['double', 'triple'], [fun(X) -> X * 2 end, fun(X) -> X * 3 end], Topenv)), [4]),
   ?assertException(throw, 'ZIBR: Id not in environment', 
      lookupEnv('\\', Topenv)),
   ?assertException(throw, 'ZIBR: arg length mismatch', 
      multiExtendEnv(['double', 'triple'], [fun(X) -> X * 2 end], Topenv)),
   io:fwrite("\tSuccess Env Test\n").

% Interp
testInterp() ->
   io:fwrite("\trunning Interp tests\n"),
   Topenv=newTopEnv(),
   12 = interp(#numC{val=12}, Topenv),
   "meow" = interp(#strC{val="meow"}, Topenv),
   #closV{args=['Symbol1', 'Symbol2'], body=#numC{val=8}, env=Topenv} = interp(#lamC{args=['Symbol1', 'Symbol2'], body=#numC{val=8}}, Topenv),
   3 = interp(#appC{func=#idC{name='+'}, args=[#numC{val=1}, #numC{val=2}]}, Topenv),
   8 = interp(#appC{func=#lamC{args=[], body=#numC{val=8}}, args=[]}, Topenv),
   7 = interp(#appC{func=#lamC{args=['Z'], body=#idC{name='Z'}}, args=[#numC{val=7}]}, Topenv),
   12 = interp(#appC{
                  func=#lamC{
                           args=['ValToBeTripled'],
                           body=#appC{
                              func=#idC{name='*'},
                              args=[#idC{name='ValToBeTripled'}, #numC{val=3}]
                           }
                        },
                  args=[#numC{val=4}]
               }, 
               Topenv),
   % TODO: Tests for ifc
   io:fwrite("\tSuccess Interp Test\n").
