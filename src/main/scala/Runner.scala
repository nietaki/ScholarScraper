/**
 * Created by nietaki on 19.05.14.
 */

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import java.io.File
import org.jsoup._
import org.jsoup.select.Elements
import scala.collection.JavaConverters._
import scala.util.matching.Regex
object Runner {
  def main (args: Array[String]): Unit = {
    //saveUniversities()
    //saveAAMAS()
    //printUniversitySimilarities()
    //join()
    //saveStoc()
    //joinAAMAS()

    joinAndSave(getStoc(), "stoc_join.csv")
  }

  def getUniversities(): Seq[University] = {
    val hu :: universityRows = CSVReader.open(new File("csv/universities_12-13.csv")).all()
    universityRows.map(ls => University(ls(0), ls(1), ls(2)))
  }
  def csvHeaders(): List[String] = List("paper_id","researcher_name","university_name", "country", "univ_similarity", "university_rank", "university_name", "country" )

  def joinAAMAS() = {
    val hr :: researcherRows = CSVReader.open(new File("csv/AAMAS_2013.csv")).all()
    val researchers = researcherRows.map(ls => Scientist(Integer.parseInt(ls(0)), ls(1), ls(2), Some(ls(3))))

    joinAndSave(researchers, "AAMAS_test.csv")
  }

  def joinAndSave(scientists: Seq[Scientist], filename: String) = {
    val universities = getUniversities()
    val resList = joinResultToStringList(join(scientists, universities))

    val writer = CSVWriter.open(new File("csv/" ++ filename))
    writer.writeRow(csvHeaders())
    writer.writeAll(resList)
    writer.close()
  }


  def join(scientists: Seq[Scientist], universities: Seq[University]): Seq[(Scientist, Option[(Double, University)])] = {

    val res = for(scientist <- scientists) yield {
      val researcherUniversity = Utils.stripUniversities(scientist.uni)
      val researcherCountry = scientist.country.map(Utils.stripCountries(_))

      var best: Option[(Double, University)] = None
      for (university <- universities) {
        val universityName = Utils.stripUniversities(university.name)
        val universityCountry = Utils.stripCountries(university.country)
        val curScore = Levenshtein.heuristicWithAcronymMatcher(researcherUniversity, universityName)
        val distance = Levenshtein.distance(researcherUniversity, universityName)
        val potentialTuple = (curScore, university)
        if ((curScore > Levenshtein.minSimilarity2 || distance <= Levenshtein.freeDiff) &&
            researcherCountry.fold(true)(Utils.sameCaseInsensitive(_, universityCountry))) {
          best = Some(best.fold(potentialTuple)(t => if(t._1 < curScore) potentialTuple else t))
        }
      }

      (scientist, best)
    }

    res
  }

  def joinResultToStringList(res: Seq[(Scientist, Option[(Double, University)])]): Seq[List[String]] = {
    res.map {case (scientist, scoreUniOption) =>

      val secondPart = scoreUniOption match {
        case None => List()
        case Some((score, uni)) => score.toString() :: uni.toListWithRank
      }
      scientist.toList ::: secondPart
    }
  }

  def printUniversitySimilarities() = {
    val reader = CSVReader.open(new File("universities_12-13.csv"))
    val writer = CSVWriter.open(new File("similarities.csv"))
    val all: List[String] = reader.all().map{row => row(1)}
    val used = all.take(200)
    writer.writeRow("x" :: used.map{u => Utils.stripUniversities(u)})
    var counter = 0
    for (y <- used) yield {
      val y2 = Utils.stripUniversities(y)
      val row = for(x <- used) yield {
        val x2 = Utils.stripUniversities(x)
        if (Levenshtein.heuristicSimilarity2(x2, y2) > 0.81) {
          counter += 1
          if(x != y)
            println(s"$y ~== $x")
          "TRUE"
        } else {
          "0"
        }
      }
      writer.writeRow(y2 :: row)
    }

    writer.close()
    println(counter)
  }

  def saveAAMAS() = {
    val in = new File("AAMAS_2013.html")
    val doc = Jsoup.parse(in, "windows-1252")
    val papers: Elements = doc.getElementsByAttributeValue("style", "text-indent: 0; margin-left: 12; margin-right: 9; margin-top: 6; margin-bottom: 0")

    def getResearcherElements(paper: nodes.Element): Elements = {
      paper.getElementsByAttributeValue("color", "#222222")
    }

    //val regex = new Regex("^([^\\(\\)]*)\\((.*)\\)$", "researcher", "uni")
    val regex = new Regex("^([^\\(\\)]*)\\((.*), ([^,]*)\\)$", "researcher", "uni", "country")
    // .findFirstMatchIn("Stan Lee (University of Warsaw, Poland)")

    val researcherLists = for(
      paper <- papers.asScala.zipWithIndex;
      researcherUniCombo <- getResearcherElements(paper._1).asScala;
      resUniText = researcherUniCombo.text().trim();
      mo = regex.findFirstMatchIn(resUniText);
      if(!mo.isEmpty)
    ) yield {
      val paperIndex = paper._2
      val m = mo.get
      val researcher = m.group("researcher").trim()
      val uni = m.group("uni").trim()
      val country = m.group("country").trim()

      println(researcher)
      println(uni)
      println(country)
      //println((paperIndex, researcherUniCombo))
      val researchUni = researcherUniCombo.text

      //println()
      List[String](paperIndex.toString(), researcher, uni, country)
    }

    saveCsv("AAMAS.csv", researcherLists)

    Unit
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
    save()
  }

  def saveCsv(fileName: String, items: Seq[Seq[Any]]) = {
    val f = new File(fileName)
    val writer = CSVWriter.open(f)
    writer.writeAll(items)
    writer.close()
  }

  def getStoc(): Seq[Scientist] = {
    val address = "http://theory.stanford.edu/stoc2013/accepted.html"
    val stocDoc: nodes.Document = Jsoup.connect(address).get();

    val commasInParentsRegex = new Regex("\\(([^(),]+)(,.*)\\)")
    //val authorUniversityMatcher = new Regex("([^)(]+) \\(([^)(]+)\\)", "name", "uni")
    val authorUniversityMatcher = new Regex(" ?(and )?([^)(]+) \\(([^)(]+)\\)", "and", "name", "uni")

    val papers = stocDoc.getElementsByTag("pre").last().text()
    println(papers)
    val papersSeq = papers.replaceAll("(?m)^$", "|").split('|')
    println(papersSeq.toList)
    val entries = for (
      (paper, index) <- papersSeq.zipWithIndex;
      lines = paper.split("\n").filter(_.length > 0).map(_.trim());
      if(lines.length == 2);
      (title, authors) = (lines.head, lines.last);
      authorsSanitized = commasInParentsRegex.replaceAllIn(authors, "($1)");
      authorsList = authorsSanitized.split(',').map(_.trim().replaceFirst("^and", "").replaceAll("\\s+", " ").trim()).toList;
      authorsUniversities <- authorsList;
      m <- authorUniversityMatcher.findAllMatchIn(authorsUniversities)
    ) yield {
      val name = m.group("name");
      val uni= m.group("uni")
      /*
      println(index)
      println(title)
      println(name)
      println(uni)*/
      Scientist(index, name, uni)
    }
    entries
  }
}
