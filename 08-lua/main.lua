-- https://stackoverflow.com/a/26367080/9348376
function copy(obj, seen)
    if type(obj) ~= 'table' then return obj end
    if seen and seen[obj] then return seen[obj] end
    local s = seen or {}
    local res = setmetatable({}, getmetatable(obj))
    s[obj] = res
    for k, v in pairs(obj) do res[copy(k, s)] = copy(v, s) end
    return res
end

function parse_file(filename)
    local data = {}
    for line in io.lines(filename) do
        local i = 1
        local parsed = {}
        for w in string.gmatch(line, "[^%s]+") do table.insert(parsed, w) end
        parsed[2] = tonumber(parsed[2])
        data[#data + 1] = parsed
    end
    return data
end

function part_one(data)
    local line = 1
    local seen = {}
    local acc = 0
    local returnval = {}
    while true do
        -- reached end of program
        if line > #data then
            returnval[1] = "eof"
            returnval[2] = acc
            return returnval
        end
        -- if we hit a line we've seen before, return
        -- else, add this to the seen table
        if seen[line] == true then
            returnval[1] = "duplicate"
            returnval[2] = acc
            return returnval
        else
            seen[line] = true
        end
        -- perform operation
        if data[line][1] == "nop" then
            line = line + 1
        elseif data[line][1] == "jmp" then
            line = line + data[line][2]
        else -- acc
            acc = acc + data[line][2]
            line = line + 1
        end
    end
end

function part_two(data)
    local line = 1
    while line <= #data do
        local cdata = copy(data)
        if data[line][1] == "nop" then
            cdata[line][1] = "jmp"
        elseif data[line][1] == "jmp" then
            cdata[line][1] = "nop"
        else
        end
        line = line + 1
        local result = part_one(cdata)
        if result[1] == "eof" then return result[2] end
    end
end

input_data = parse_file(arg[1])
print("Part 1: " .. part_one(input_data)[2])
print("Part 2: " .. part_two(input_data))
