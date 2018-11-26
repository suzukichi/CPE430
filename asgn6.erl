-module(asgn6).
-export([hello_world/0]).

% ExprCs
% TODO: ExprC (U NumC IdC StrC IfC LamC AppC))
-record(NumC, {val}). 
-record(IdC, {name}).
-record(StrC, {val}).
-record(IfC, {test, then, else}).
-record(LamC, {args, body}).
-record(AppC, {func, args}).

% TODO: Interp


% TODO: Parse


hello_world() -> io:fwrite("hello, world\n").
   
