/**
 * Created by nietaki on 19.05.14.
 */

import com.github.tototoshi.csv.CSVWriter
import java.io.File
import org.jsoup._
import scala.collection.JavaConverters._
object Runner {
  def main (args: Array[String]) = {
    //saveUniversities()
  }

  def saveUniversities() = {
    val baseAddress = "http://www.timeshighereducation.co.uk/world-university-rankings/2013-14/world-ranking/range/"
    val ranges = List("001-200", "201-225", "226-250", "251-275", "276-300", "301-350", "351-400")

    val tuples = for(
      rng <- ranges;
      address = baseAddress + rng;
      curRanking: nodes.Document = Jsoup.connect(address).get();
      tbody = curRanking.getElementById("rankings-table").getElementsByTag("tbody").get(0);
      trs = tbody.children().asScala;
      tr <- trs
    ) yield {

      /// rank
      val rank = tr.getElementsByClass("rank").get(0).text()
      //println(rank)
      //val rankVal: Int = rank.text().toInt

      /// university name
      val universityName = tr.getElementsByClass("uni").text()
      //println(universityName)

      /// region
      val region = tr.getElementsByClass("region-title").text()
      //println(region)


      /// score
      val score = tr.getElementsByClass("score-container").text()

      val ret = (rank, universityName, region, score)
      println(ret)
      ret
    }

    def save() = {
      val out = new File("universities.csv")
      val writer = CSVWriter.open(out)
      writer.writeAll(tuples.map{case (t1, t2, t3, t4) => List(t1, t2, t3, t4)})
      writer.close()
    }
    //save()

  }
}
