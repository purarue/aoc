require 'set'
$upper = Set["B", "R"]

def binary_space_partition(encoded)
  x = 2 ** encoded.length - 1
  y = 0
  encoded.chars.each do |bit|
    if $upper.include?(bit)
      y = x - ((x - y) / 2)
    else
      x = y + ((x - y) / 2)
    end
  end
  x
end

def seat_number(line)
  binary_space_partition(line[0..6]) * 8 + binary_space_partition(line[7..-1])
end

def main(datafile)
  input = (IO.read datafile).split "\n"
  ids = input.map { |ln| seat_number(ln) }
  # part one
  puts ids.max
  # part two
  sids = ids.to_set
  puts ((sids.min..sids.max).to_set - sids).to_a.first
end

main ARGV.first
