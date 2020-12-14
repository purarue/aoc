@enum InstrType mask mem

struct Instruction
  type::InstrType
  value::Union{String,Tuple{Int64,Int64}}
end

is_mask(i::Instruction) = i.type == mask

function parse_file(input_file::String)
  parsed = []
  open(input_file) do file
    for ln in eachline(file)
      if startswith(ln, "mask")
        push!(parsed, Instruction(mask, String(split(ln)[3])))
      else
        push!(parsed, Instruction(mem, Tuple(map(x->parse(Int64,x), SubString.(ln, findall(r"\d+", ln))))))
      end
    end
  end
  return parsed
end

function apply_mask(cmask::String, num::Int64)
  for (index, val) in enumerate(reverse(cmask))
    if val != 'X'
      ival = parse(Int64,val)
      if ival == 0
        # clear bit
        num &= ~(1 << (index - 1));
      else
        # set bit
        num |= (1 << (index - 1));
      end
    end
  end
  return num
end

function part_one(parsed_instructions)
  cmask = nothing
  memory = Dict()
  for instr in parsed_instructions
    if is_mask(instr)
      cmask = instr.value
    else
      addr, memval = instr.value
      memory[addr] = apply_mask(cmask, memval)
    end
  end
  return sum(values(memory))
end

function main(input_file)
  parsed_instructions = parse_file(input_file)
  println("Part 1: $(part_one(parsed_instructions))")
end

main(ARGS[1])
