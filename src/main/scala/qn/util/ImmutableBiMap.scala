package qn.util

import com.google.common.{collect => guava}

import scala.collection.JavaConversions._

class ImmutableBiMap[K, V] private(private val g: guava.ImmutableBiMap[K, V]) {
  def inverse: ImmutableBiMap[V, K] = new ImmutableBiMap[V, K](g.inverse())
}

object ImmutableBiMap {
  def apply[K, V] (m:Map[K,V]) = new ImmutableBiMap[K, V]({
    val mapBuilder = guava.ImmutableBiMap.builder[K,V]()
    m.foreach(pair=> mapBuilder.put(pair._1, pair._2))
    mapBuilder.build()
  })
  def empty[K, V] = new ImmutableBiMap[K, V](guava.ImmutableBiMap.of())

  implicit def toMap[K, V](x: ImmutableBiMap[K, V]): Map[K, V] = x.g.toMap
}
