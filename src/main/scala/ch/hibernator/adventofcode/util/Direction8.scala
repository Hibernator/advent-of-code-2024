package ch.hibernator.adventofcode.util

enum Direction8:
  def opposite: Direction8 =
    this match
      case Up        => Down
      case Down      => Up
      case Left      => Right
      case Right     => Left
      case UpRight   => DownLeft
      case UpLeft    => DownRight
      case DownLeft  => UpRight
      case DownRight => UpLeft

  case Up, Down, Left, Right, UpRight, UpLeft, DownRight, DownLeft
