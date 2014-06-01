/**
 * Created by nietaki on 01.06.14.
 */
case class University(rankString: String, name: String, country: String) {
  lazy val rank: Int = {
    Utils.tryParseInt(rankString) match {
      case Some(r) => r
      case None => {
        val ranks = rankString.split('-')
        (Integer.parseInt(ranks.head) + Integer.parseInt(ranks.last) ) / 2
      }
    }
  }

  def toList = {
    List(rankString, name, country)
  }

  def toListWithRank = {
    List[String](rank.toString(), name, country)
  }
}
