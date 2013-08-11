-module(mkpasswd).
-export([create/1, create/2]).
-import(random).
-import(lists).

% This version prints a string.
%% get_one({source, Source}) ->
%%     lists:nth(random:uniform(length(Source)), Source).

%% create({length, 1}, {source, Source}) ->
%%     [get_one({source, Source})];

%% create({length, Len}, {source, Source}) ->
%%     [get_one({source, Source}) | create({length, Len-1}, {source, Source})].

% This version prints an array of numbers.
% Returning [lists...] from this version prints an array of strings.
% I have no idea what the magic is!
% Made it work with concat and chars, but ugly.  Want to build as
% array of numbers and convert to chars at the end, but I guess not required.
create({length, 1}, {source, Source}) ->
    string:chars(lists:nth(random:uniform(length(Source)), Source), 1);

create({length, Len}, {source, Source}) ->
    string:concat(create({length, 1}, {source, Source}),
		  create({length, Len-1}, {source, Source})).


create({length, Len}) ->
%    random:seed(erlang:now()),
% 48-57 are 0-9.
% in between are symbols (I think).
% 65-122 are A-Za-z.
    create({length, Len}, {source, lists:seq(48,122)}).
