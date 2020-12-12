import 'dart:io';

enum TileType { floor, empty, occupied }

class Tile {
  TileType tile;
  Tile(this.tile);

  Tile.fromRepr(String tileChar) {
    switch (tileChar) {
      case '.':
        this.tile = TileType.floor;
        break;
      case 'L':
        this.tile = TileType.empty;
        break;
      case '#':
        this.tile = TileType.occupied;
        break;
    }
  }

  Tile copy() => new Tile(this.tile);
}

class Position {
  int x;
  int y;
  Position(this.x, this.y);
}

class TileChange {
  TileType from;
  TileType to;
  Position pos;
  TileChange(this.pos, this.from, this.to);
}

const offsets = [-1, 0, 1];

class Grid {
  List<List<Tile>> grid;

  // default constructor
  Grid(this.grid);

  // alternate constructor
  Grid.fromFile(String filename) {
    String contents = new File(filename).readAsStringSync();
    List<List<Tile>> nGrid = [];
    // for each line
    this.grid = contents
        .split('\n')
        .map((String line) =>
            line.split('').map((String tChar) => Tile.fromRepr(tChar)).toList())
        .toList();
  }

  // return any changes to the grid as a list
  List<TileChange> checkRulesPartOne() {
    List<TileChange> changes = [];
    for (int i = 0; i < this.grid.length; i++) {
      for (int j = 0; j < this.grid[i].length; j++) {
        int adjacentOccupied = 0;
        for (var a in offsets) {
          for (var b in offsets) {
            if (a == 0 && b == 0) {
              continue;
            }
            int nx = i + a;
            int ny = j + b;
            if (nx >= 0 &&
                nx < this.grid.length &&
                ny >= 0 &&
                ny < this.grid[nx].length) {
              // valid cell
              if (this.grid[nx][ny].tile == TileType.occupied) {
                adjacentOccupied++;
              }
            }
          }
        }
        if (this.grid[i][j].tile == TileType.occupied &&
            adjacentOccupied >= 4) {
          changes.add(new TileChange(
              new Position(i, j), TileType.occupied, TileType.empty));
        } else if (this.grid[i][j].tile == TileType.empty &&
            adjacentOccupied == 0) {
          changes.add(new TileChange(
              new Position(i, j), TileType.empty, TileType.occupied));
        }
      }
    }
    return changes;
  }

  List<TileChange> checkRulesPartTwo() {
    List<TileChange> changes = [];
    for (int i = 0; i < this.grid.length; i++) {
      for (int j = 0; j < this.grid[i].length; j++) {
        int adjacentOccupied = 0;
        for (var a in offsets) {
          for (var b in offsets) {
            if (a == 0 && b == 0) {
              continue;
            }
            int view = 1;
            while (true) {
              // look in this direction, multiply by 'view' distance
              int nx = i + (a * view);
              int ny = j + (b * view);
              if (nx >= 0 &&
                  nx < this.grid.length &&
                  ny >= 0 &&
                  ny < this.grid[nx].length) {
                // valid cell
                if (this.grid[nx][ny].tile == TileType.occupied) {
                  adjacentOccupied++;
                  break;
                } else if (this.grid[nx][ny].tile == TileType.empty) {
                  // empty seats stop a person from seeing adjacent seats that are further away
                  break;
                }
              } else {
                // if we've hit the end of the grid, stop checking this direction
                break;
              }
              view++;
            }
          }
        }
        if (this.grid[i][j].tile == TileType.occupied &&
            adjacentOccupied >= 5) {
          changes.add(new TileChange(
              new Position(i, j), TileType.occupied, TileType.empty));
        } else if (this.grid[i][j].tile == TileType.empty &&
            adjacentOccupied == 0) {
          changes.add(new TileChange(
              new Position(i, j), TileType.empty, TileType.occupied));
        }
      }
    }
    return changes;
  }

  // return the number of applications made
  int applyRules(List<TileChange> changes) {
    changes.forEach((change) =>
        this.grid[change.pos.x][change.pos.y] = new Tile(change.to));
    return changes.length;
  }

  int countType(TileType ttype) {
    int result = 0;
    for (List<Tile> line in this.grid) {
      for (Tile tile in line) {
        if (tile.tile == ttype) {
          result++;
        }
      }
    }
    return result;
  }

  Grid copy() {
    return Grid(this
        .grid
        .map((List<Tile> line) => line.map((Tile tile) => tile.copy()).toList())
        .toList());
  }
}

int part(Grid seats, func) {
  while (true) {
    if (func(seats) == 0) {
      return seats.countType(TileType.occupied);
    }
  }
}

int part1(Grid seats) =>
    part(seats, (Grid gr) => gr.applyRules(gr.checkRulesPartOne()));
int part2(Grid seats) =>
    part(seats, (Grid gr) => gr.applyRules(gr.checkRulesPartTwo()));

void main(List<String> args) {
  String filename = args[0];
  Grid seats = Grid.fromFile(filename);
  print("Part 1: " + part1(seats.copy()).toString());
  print("Part 2: " + part2(seats.copy()).toString());
}
