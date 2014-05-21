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

  val theRegex = new Regex("(?i)The ")
  val uk = new Regex("(?i)United Kingdom")
  val usa = new Regex("(?i)United States( of America)?")
  def stripCountries(in: String): String = {
    val in2 = theRegex.replaceAllIn(in, "")
    val in3 = uk.replaceAllIn(in2, "UK")
    val in4 = usa.replaceAllIn(in3, "USA")
    in4
  }

  def sameCaseInsensitive(s1: String, s2: String): Boolean = {
    s1.compareToIgnoreCase(s2) == 0
  }
}
