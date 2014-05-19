/**
 * Created by nietaki on 19.05.14.
 */
import org.jsoup._
import scala.collection.JavaConverters._
object Runner {
  def main (args: Array[String]) {
    val ranking1: nodes.Document = Jsoup.connect("http://www.timeshighereducation.co.uk/world-university-rankings/2012-13/world-ranking").get()
    val tbody = ranking1.getElementById("rankings-table").getElementsByTag("tbody").get(0)
    val trs: Iterable[nodes.Element] = tbody.children().asScala
    //println(ranking1)
    //println(tbody)
    val tuples = for(tr <- trs) yield {
      /// rank
      val rank = tr.getElementsByClass("rank").get(0)
      println(rank)
      val rankVal: Int = rank.text().toInt
      println(rankVal)

      /// university name
      val universityName = tr.getElementsByClass("uni").text()
      println(universityName)

      /// region
      val region = tr.getElementsByClass("region-title").text()
      println(region)
      (universityName, region, rankVal)
    }
  }
}
