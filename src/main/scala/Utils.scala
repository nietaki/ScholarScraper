/**
 * Created by nietaki on 20.05.14.
 */
import scala.util.matching.Regex

object Utils {
  val universityOfRegex = new Regex("(?ui)University of ") //mind the space
  val uniStar = new Regex("(?ui)Uni[wv]er\\p{Alpha}+ ") //again, mind the space


  def stripUniversities(in: String): String = {
    val in2 = universityOfRegex.replaceAllIn(in, "Uo ")
    val in3 = uniStar.replaceAllIn(in2, "U ")
    in3
  }
}
