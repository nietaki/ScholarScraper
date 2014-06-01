/**
 * Created by nietaki on 20.05.14.
 */
import scala.util.matching.Regex

object Utils {
  val universityOfRegex = new Regex("(?ui)University of ") //mind the space
  val uniStar = new Regex("(?ui)Uni[wv]er[^ ]+([ _]?)") //again, mind the space
  val iotRegex = new Regex("(?ui)Institute of Technology")
  val parentsAndCommas = new Regex("[)(,]")

  def stripUniversities(in: String): String = {
    //val in2 = universityOfRegex.replaceAllIn(in, "Uo ")
    val in2 = universityOfRegex.replaceAllIn(in, "U ")
    //val in3 = uniStar.replaceAllIn(in2, "U ")
    val in3 = uniStar.replaceAllIn(in2, "U$1")
    val in4 = iotRegex.replaceAllIn(in3, "InsOT")
    in4.trim()
  }

  def stripParents(in: String): String = {
    parentsAndCommas.replaceAllIn(in, "")
  }

  def uppercaseWords(s: String) = {
    //c.isUpper || c.isLower isn't true for all chars
    stripParents(s).split(' ').filter(_.forall(! _.isLower)).filter(_.length() >= 3)
  }

  def haveMatchingAcronym(s1: String, s2: String): Boolean = {
    val s2Uppers = uppercaseWords(s2)
    uppercaseWords(s1).exists {s =>
      s2Uppers.exists(_ == s)
    }
  }

  val theRegex = new Regex("(?i)The ")
  val uk = new Regex("(?i)United Kingdom")
  val usa = new Regex("(?i)United States( of America)?")
  def stripCountries(in: String): String = {
    val in2 = theRegex.replaceAllIn(in, "")
    val in3 = uk.replaceAllIn(in2, "UK")
    val in4 = usa.replaceAllIn(in3, "USA")
    in4.trim()
  }

  def sameCaseInsensitive(s1: String, s2: String): Boolean = {
    s1.compareToIgnoreCase(s2) == 0
  }

  def tryParseInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch{
      case _: java.lang.NumberFormatException => None
    }
  }
}
