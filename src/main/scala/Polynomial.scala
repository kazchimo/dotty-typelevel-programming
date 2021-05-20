import scala.compiletime.ops.int
import scala.compiletime.ops.any.*
import Polynomial.*

enum Polynomial:
  case X[Coef <: Int, N <: Int]()
  case Plus[XX <: X[?, ?], YY <: Polynomial]()

type N[XX <: X[?, ?]] = XX match
  case ? X n => n

type C[XX <: X[?, ?]] = XX match
  case c X ? => c

type +[A <: Polynomial, B <: Polynomial] <: Polynomial = A match
  case c X n =>
    B match
      case cc X nn =>
        n == nn match
          case true => int.+[c, cc] X n
          case false =>
            int.>[n, nn] match
              case true  => A Plus B
              case false => B Plus A
      case xx Plus yy =>
        int.>[n, N[xx]] match
          case true => A Plus B
          case false =>
            n == N[xx] match
              case true  => (int.+[c, C[xx]] X n) Plus yy
              case false => xx Plus (A + yy)
  case xx Plus yy =>
    B match
      case cc X nn =>
        N[xx] == nn match
          case true => (int.+[C[xx], cc] X nn) Plus yy
          case false =>
            int.>[N[xx], nn] match
              case true  => xx Plus (B + yy)
              case false => B Plus A
      case xxx Plus yyy =>
        N[xx] == N[xxx] match
          case true => (int.+[C[xx], C[xxx]] X N[xx]) Plus (yy + yyy)
          case false =>
            int.>[N[xx], N[xxx]] match
              case true  => xx Plus (yy + B)
              case false => xxx Plus (yyy + A)

object Polynomial:
  summon[N[1 X 2] =:= 2]
  summon[C[1 X 2] =:= 1]

  summon[(3 X 1) + (4 X 1) =:= (7 X 1)]
  summon[(3 X 1) + (2 X 0) =:= ((3 X 1) Plus (2 X 0))]
  summon[(3 X 1) + (4 X 2) =:= (4 X 2) + (3 X 1)]
  summon[(3 X 2) + (4 X 1) + (2 X 0) =:= ((3 X 2) Plus ((4 X 1) Plus (2 X 0)))]
  summon[(3 X 2) + ((4 X 1) + (2 X 0)) =:= (3 X 2) + (4 X 1) + (2 X 0)]
  summon[(3 X 1) + ((4 X 1) + (2 X 0)) =:= (7 X 1) + (2 X 0)]
  summon[(3 X 0) + ((4 X 1) + (2 X 0)) =:= (4 X 1) + (5 X 0)]
  summon[((3 X 1) + (2 X 0)) + (4 X 1) =:= (7 X 1) + (2 X 0)]
  summon[((3 X 1) + (2 X 0)) + (4 X 2) =:= (4 X 2) + (3 X 1) + (2 X 0)]
  summon[(3 X 1) + ((4 X 2) + (2 X 0)) =:= ((4 X 2) + (3 X 1) + (2 X 0))]
  summon[
    ((5 X 3) + (3 X 1)) + ((4 X 2) + (2 X 0)) =:= (5 X 3) + (4 X 2) + (3 X 1) + (2 X 0)
  ]
