/**
 * Created by nietaki on 01.06.14.
 */
case class Scientist(paperId: Int, name: String, uni: String, country: Option[String] = None) {
  def toList() = List(paperId.toString(), name, uni, country.getOrElse(""))
}
