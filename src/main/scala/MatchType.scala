import compiletime.ops.int.*

type ToString[X] = X match
  case String => "String"
  case Int    => "Int"

type Factorial[X <: Int] <: Int = X match
  case 0    => 1
  case S[i] => X * Factorial[i]

//type ListLike[X] = X match
//  case String => List[String]
//  case Int    => List[List[Int]]

type ListLike[X] <: List[?] = X match
  case String => List[String]
  case Int    => List[List[Int]]

type Content[X] = X match
  case List[a]   => a
  case Map[?, a] => a

def head[X](xs: ListLike[X]) = xs.head

object M extends App:
  summon[ToString[String] =:= "String"]
  summon[ToString[Int] =:= "Int"]
  summon[ToString[Long] <:< Any]

  println((List(List(1)): ListLike[Int]).map(_.head))

  summon[Content[List[Int]] =:= Int]
  summon[Content[Map[String, String]] =:= String]

  summon[Factorial[0] =:= 1]
  summon[Factorial[3] =:= 6]
