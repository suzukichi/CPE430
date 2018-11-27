-module(asgn6).
-export([test/0]).

% Note: in Erlang, records must have a lowercase name.
% Variables have capital letter names.

%TODO: environment, values, primops.

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
%s TODO: fill out
12.

% Gets the value of a bar from the environment
lookupEnv(Needle, Haystack) ->
   % TODO: actually look up.
   Needle.

% Test cases
test()->
   io:fwrite("Test suite\n"),
   testBasic(),
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
   io:fwrite("\tSuccess basic exprc\n").

% Interp
testInterp() ->
   12 = interp(#numC{val=12}, newTopEnv()),
   "meow" = interp(#strC{val="meow"}, newTopEnv()),
   #lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}} = interp(#lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}, newTopEnv()),
   % TODO: Tests for appc, ifc
   io:fwrite("\tSuccess Interp\n").
