package uk.camsw.bloom

import java.nio.ByteBuffer

import com.roundeights.hasher.Algo

object HashFunction {

  val hash: Algo => String => Array[Byte] =
    algo => algo(_)

  val toInt: Array[Byte] => Int = ByteBuffer.wrap(_).getInt

  val bound: Range => Int => Int =
    range =>
      n => {
        range.min + math.abs(n % range.size)
      }

  def boundedHash(algo: Algo, range: Range): String => Int =
    hash(algo) andThen toInt andThen bound(range)

}