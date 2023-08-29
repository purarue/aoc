import os
import sugar
import strutils
import sequtils

type
  Rule = ref object of RootObj
    beginning: int
    ending: int


proc readRules(fileObj: File): seq[bool] =
  var
    max: int
    rules: seq[Rule]
  rules = newSeq[Rule]()
  max = 0
  # read lines till we hit an empty one, parsing rules
  while true:
    let line = fileObj.readLine().strip()
    if line == "":
      break
    for rule in toSeq(line.split(":")[1].split("or"))
      .map(x => toSeq(x.strip().split("-"))
      .map(y => parseInt(y))):
      rules.add(Rule(beginning: rule[0], ending: rule[1]))
      for rl in rule:
        if rl > max:
          max = rl
  var
    isValid: seq[bool]
  # create true/false map for rules
  isValid = repeat(false, max + 1)
  for rl in rules:
    for num in rl.beginning..rl.ending:
      isValid[num] = true
  return isValid

let
  file = open("./input.txt")
  rules = readRules(file)

# discard rules till we hit nearby
while true:
  if file.readLine().strip().startsWith("nearby tickets"):
    break

var
  scanningErrorRate: int
  line: string

scanningErrorRate = 0

# read nearby seats
while file.readLine(line):
  for num in line.strip().split(",").map(x => parseInt(x)):
    if num > rules.len:
      scanningErrorRate += num
    else:
      if not rules[num]:
        scanningErrorRate += num

echo "Part 1: ", scanningErrorRate
