-module(asgn6).
-export([test/0]).

% ExprCs
% Note in Erlang, records must have a lowercase name (unknown why).
% TODO: ExprC (U NumC IdC StrC IfC LamC AppC))
-record(numC, {val}). 
-record(idC, {name}).
-record(strC, {val}).
-record(ifC, {test, then, else}).
-record(lamC, {args, body}).
-record(appC, {func, args}).

% TODO: Interp


% TODO: Parse


% Test cases
test() ->
   % Basic ExprCs
   12 = #numC{val=12}#numC.val,
   $d = #idC{name=$d}#idC.name,
   "meow" = #strC{val="meow"}#strC.val,
   true = #ifC{test=true, then=true, else=false}#ifC.test,
   {#idC{name=$a}, #idC{name=$b}} = #lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}#lamC.args,
   {#numC{val=12}, #numC{val=3}} = #appC{func=#lamC{args={#idC{name=$a}, #idC{name=$b}}, body=#numC{val=5}}, args={#numC{val=12}, #numC{val=3}}}#appC.args,
   passed.
