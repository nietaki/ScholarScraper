/**
 * Created by nietaki on 20.05.14.
 */
import scala.math._

object Levenshtein {

  //var minSimilarity2 = 0.81
  var minSimilarity2 = 0.7
  var freeDiff = 4

  def minimum(i1: Int, i2: Int, i3: Int)=min(min(i1, i2), i3)

  def distance(s1i:String, s2i:String)={
    val s1: String = s1i.toLowerCase()
    val s2: String = s2i.toLowerCase()
    val dist=Array.tabulate(s2.length+1, s1.length+1){(j,i)=>if(j==0) i else if (i==0) j else 0}

    for(j<-1 to s2.length; i<-1 to s1.length)
      dist(j)(i)=if(s2(j-1)==s1(i-1)) dist(j-1)(i-1)
      else minimum(dist(j-1)(i)+1, dist(j)(i-1)+1, dist(j-1)(i-1)+1)

    dist(s2.length)(s1.length)
  }

  def lengthDiff(s1: String, s2: String): Int = abs(s1.length - s2.length)

  def minLength(s1: String, s2: String): Int = min(s1.length, s2.length)

  def maxLength(s1: String, s2: String): Int = max(s1.length, s2.length)

  def heuristicSimilarity1(s1: String, s2: String): Double = {
    1.0 - (distance(s1, s2) - lengthDiff(s1, s2)) * 1.0 / minLength(s1, s2)
  }

  /// we'll be using this one
  def heuristicSimilarity2(s1: String, s2: String): Double = {
    1.0 - distance(s1, s2) * 1.0 / maxLength(s1, s2)
  }

  def printDistance(s1:String, s2:String)=println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
}
