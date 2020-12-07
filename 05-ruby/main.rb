# frozen_string_literal: true

require "set"

def binary_space_partition(encoded)
  x = 2 ** encoded.length - 1
  y = 0
  encoded.chars.each do |bit|
    if Set["B", "R"].include?(bit)
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
  puts "Part 1: #{ids.max}"
  # part two
  sids = ids.to_set
  puts "Part 2: #{((sids.min..sids.max).to_set - sids).to_a.first}"
end

main ARGV.first
