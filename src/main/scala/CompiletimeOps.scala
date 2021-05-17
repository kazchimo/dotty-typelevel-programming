object IntOps:
  import scala.compiletime.ops.int.*
  val two: 1 + 1 = 2
  summon[2 =:= 1 + 1]
//  summon[3 =:= 1 + 1]
  summon[2 =:= Max[1, 2]]
  summon[1 =:= Min[1, 2]]
  summon["1" =:= ToString[1]]
  summon[2 =:= S[1]]
//  summon[0 =:= S[-1]]

object StringOps:
  import scala.compiletime.ops.string.*
  summon["hello taro" =:= "hello " + "taro"]

object BooleanOps:
  import scala.compiletime.ops.boolean.*
  summon[true =:= ![false]]
  summon[false =:= ![true]]

  summon[false =:= (true ^ true)]
  summon[true =:= (false ^ true)]

  summon[true =:= (true && true)]
  summon[false =:= (true && false)]

  summon[true =:= (true || false)]
  summon[false =:= (false || false)]

object AnyOps:
  import scala.compiletime.ops.int.*
  import scala.compiletime.ops.any.*
  summon[true =:= (2 == S[1])]
  summon[false =:= (2 == S[2])]
