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
}
