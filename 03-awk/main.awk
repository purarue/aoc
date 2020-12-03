#!/usr/bin/awk -f

BEGIN {
  # first offset is for part 1
  off["1x"] = 3; off["1y"] = 1
  off["2x"] = 1; off["2y"] = 1
  off["3x"] = 5; off["3y"] = 1
  off["4x"] = 7; off["4y"] = 1
  off["5x"] = 1; off["5y"] = 2
}

{
  field[NR - 1] = $0
}

function count_trees(strategy) {
  cols = length(field[0])
  x = y = trees = 0
  # lookup offsets
  xoff = off[sprintf("%dx", strategy)]
  yoff = off[sprintf("%dy", strategy)]
  # count trees, modulo x location
  while (y < NR) {
    x = (x + xoff) % cols
    y += yoff
    if (substr(field[y], x + 1, 1) == "#") {
      trees += 1
    }
  }
  return trees
}

END {
  # part one
  print("Part 1:", count_trees(1))
  # part two
  for (tmult = s = 1; s <= 5; s++) {
    tmult *= count_trees(s)
  }
  print("Part 2:", tmult)
}
