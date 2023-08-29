<?php

class Location
{
    public $x;
    public $y;
    public function __construct($x, $y)
    {
        $this->x = $x;
        $this->y = $y;
    }

    public function manhattanDistance()
    {
        return abs($this->x) + abs($this->y);
    }
}

class Action
{
    const North = 0;
    const East = 1;
    const South = 2;
    const West = 3;
    const Left = 4;
    const Right = 5;
    const Forward = 6;

    public function fromCode($char)
    {
        switch ($char) {
            case "N":
                return Action::North;
            case "S":
                return Action::South;
            case "E":
                return Action::East;
            case "W":
                return Action::West;
            case "L":
                return Action::Left;
            case "R":
                return Action::Right;
            case "F":
                return Action::Forward;
        }
    }
}

class Facing
{
    const North = 0;
    const East = 1;
    const South = 2;
    const West = 3;

    public $facing;
    public function __construct($initial = Facing::East)
    {
        $this->facing = $initial;
    }

    // rotates clockwise
    public function rotate($by)
    {
        while ($by > 0) {
            $by -= 90;
            $this->facing = ($this->facing + 1) % 4;
        }
    }
}

class Instruction
{
    public $action;
    public $value;
    public function __construct($action, $value)
    {
        $this->action = $action;
        $this->value = $value;
    }
    public function fromLine($line)
    {
        $parsedAction = Action::fromCode(substr($line, 0, 1));
        $parsedValue = intval(substr($line, 1));
        return new Instruction($parsedAction, $parsedValue);
    }
}

class Ship
{
    private $loc;
    private $dir;
    public function __construct()
    {
        $this->loc = new Location(0, 0);
        $this->dir = new Facing();
    }
    public function move($instr)
    {
        switch ($instr->action) {
            case Action::North:
                $this->loc->y += $instr->value;
                break;
            case Action::South:
                $this->loc->y -= $instr->value;
                break;
            case Action::East:
                $this->loc->x += $instr->value;
                break;
            case Action::West:
                $this->loc->x -= $instr->value;
                break;
            case Action::Left:
                $this->dir->rotate((360 - $instr->value) % 360);
                break;
            case Action::Right:
                $this->dir->rotate($instr->value % 360);
                break;
            case Action::Forward:
                $this->move(new Instruction($this->dir->facing, $instr->value));
                break;
        }
    }

    public function getLoc()
    {
        return $this->loc;
    }
}

function parseFile($inputFile)
{
    $parsed = [];
    foreach (
        preg_split("/((\r?\n)|(\r\n?))/", trim(file_get_contents($inputFile)))
        as $line
    ) {
        array_push($parsed, Instruction::fromLine($line));
    }
    return $parsed;
}

function main($args)
{
    $inputFile = $args[1];
    $instructions = parseFile($inputFile);
    $ship = new Ship();
    foreach ($instructions as $instr) {
        $ship->move($instr);
    }
    echo $ship->getLoc()->manhattanDistance() . "\n";
}

main($argv);
