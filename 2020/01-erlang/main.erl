-module(main).
-export([main/1]).
-define(TARGET, 2020).

load_data(From) ->
  {ok, Data} = file:read_file(From),
  %% global splits on all occurrences, not just first
  Lines = binary:split(string:trim(Data), <<"\n">>, [global]),
  %% https://stackoverflow.com/a/12508106/9348376
  Nums = [begin {Int,_}=string:to_integer(Ln), Int end || Ln <- Lines],
  Nums.

%% in part 1, Tar is always 2020
%% this acts as the 'inner loop' for part_two, so its set as an
%% argument here
part_one(N, S) -> part_one(N, S, ?TARGET).
part_one([], _, _) -> error;
part_one([C|N], S, Tar) ->
  Target = Tar - C,
  case sets:is_element(Target, S) of
    true -> Target * C;
    false -> part_one(N, S, Tar)
  end.

%% C acts as the outer loop value
part_two(N, S) -> part_two(N, N, S).
part_two([], _, _) -> error;
part_two([C|OuterNums], OrigNums, S) ->
  Midtarget = ?TARGET - C,
  case part_one(OrigNums, S, Midtarget) of
    error -> part_two(OuterNums, OrigNums, S);  %% part_one couldn't find a solution, try next number
    Prod -> Prod * C
  end.

main(Args) ->
  [Filename] = Args,
  Nums = load_data(Filename),
  Snums = sets:from_list(Nums),
  io:format("Part 1: ~p~nPart 2: ~p~n", [part_one(Nums, Snums), part_two(Nums, Snums)]).
