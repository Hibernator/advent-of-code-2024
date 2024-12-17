package ch.hibernator.adventofcode.util

enum Direction4:
  def opposite: Direction4 =
    this match
      case Up    => Down
      case Down  => Up
      case Left  => Right
      case Right => Left

  case Up, Down, Left, Right
