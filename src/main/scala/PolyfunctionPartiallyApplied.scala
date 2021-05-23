import scala.compiletime.ops.int.*

final case class SizedList[L <: Int, V](data: List[V]):
  def head(using L > 0 =:= true) = data.head

object SizedList:
//  class SizedListPartiallyApplied[L <: Int]:
//    def apply[V](data: List[V]): SizedList[L, V] = SizedList[L, V](data)
//
//  def apply[L <: Int]: SizedListPartiallyApplied[L] =
//    SizedListPartiallyApplied[L]

  class SizedListPartiallyApplied[L <: Int]:
    def apply[V](data: V*): SizedList[L, V] = SizedList[L, V](data.toList)

  def apply[L <: Int]: SizedListPartiallyApplied[L] =
    SizedListPartiallyApplied[L]

//  def apply[L <: Int]: [V] => List[V] => SizedList[L, V] =
//    [V] => (data: V*) => ???

//  def apply[L <: Int]: [V] => List[V] => SizedList[L, V] =
//    [V] => (data: List[V]) => SizedList[L, V](data)

object PolyfunctionPartiallyApplied:
//  val sizeOne = SizedList[1, Int](List(1))
//  val sizeZero = SizedList[0, Int](List(0))
//
//  sizeOne.head
//  sizeZero.head

//  val sizeOne = SizedList[1](List(1))
//  val sizeZero = SizedList[0](List(0))
//
//  sizeOne.head
//  sizeZero.head

  def toList[V](v: V) = List(v)
//  val toList = (v: Int) => List(v)
  val toList = [V] => (v: V) => List(v)

  SizedList[1](1)
  SizedList[2](1, 2)
