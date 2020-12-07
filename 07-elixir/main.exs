defmodule AdventOfCode do
  defp parse_line(input_line) when is_bitstring(input_line) do
    [current_bag | [others | []]] =
      Regex.run(
        ~r"^((?:\w+\s)+)+bags? contain (.*)",
        input_line,
        capture: :all_but_first
      )

    {current_bag |> String.trim(),
     others |> String.split(",") |> Enum.map(&parse_contained(&1)) |> Enum.reject(&is_nil(&1))}
  end

  defp parse_contained(other) do
    case Regex.run(
           ~r"(\d+) ((?:\w+\s)+)bags?",
           other,
           capture: :all_but_first
         ) do
      [count, color] ->
        {String.to_integer(count), color |> String.trim()}

      _ ->
        nil
    end
  end

  # handles first recursive call
  def part1(parsed_map, target), do: part1(parsed_map, target, Map.keys(parsed_map), 0)
  # base case, no more to check
  def part1(_parsed_map, _target, [], count), do: count

  def part1(parsed_map, target, [cur_bag | other_bags], count) do
    if bag_contains(parsed_map, target, parsed_map[cur_bag]) do
      # contains, increment count
      part1(parsed_map, target, other_bags, count + 1)
    else
      part1(parsed_map, target, other_bags, count)
    end
  end

  def bag_contains(_parsed_map, _target, []), do: false
  # this matched the target with the current head of list, found it
  def bag_contains(_parsed_map, target, [{_bag_count, target} | _other_bags]), do: true

  # otherwise
  # do a recursive call to check the bags this contains
  # else, try with the rest of the list
  def bag_contains(parsed_map, target, [{_bag_count, bag_color} | other_bags]) do
    if bag_contains(parsed_map, target, parsed_map[bag_color]) do
      true
    else
      bag_contains(parsed_map, target, other_bags)
    end
  end

  # handles first recursive call
  def part2(parsed_map, from) when is_bitstring(from),
    do: part2(parsed_map, 0, parsed_map[from]) - 1

  # reached the end of some list, count *this* bag
  def part2(_parsed_map, bag_count, []), do: bag_count + 1

  def part2(parsed_map, bag_count, [{bcount, color} | other_bags]) do
    # combine the score for the current bag were looking at with our current
    # recursive call to continue checking bags
    part2(parsed_map, bag_count + bcount * part2(parsed_map, 0, parsed_map[color]), other_bags)
  end

  def part(result, num), do: "Problem #{num}: #{result}"

  def main() do
    [input_file] = System.argv()
    {:ok, raw_text} = File.read(input_file)

    parsed_map =
      raw_text
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&parse_line(&1))
      |> Enum.into(Map.new())

    parsed_map |> part1("shiny gold") |> part(1) |> IO.puts()
    parsed_map |> part2("shiny gold") |> part(2) |> IO.puts()
  end
end

AdventOfCode.main()
