
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

case class Plane(
  model: String,
  serial: String,
  reg: String,
  engine: String,
  flying: Boolean,
  totalHours: Option[String] = None,
  yearBuilt: Option[String] = None,
  contact: Option[String] = None) {

  override def toString = s""""$model", "$serial", "$reg", "$engine", "${totalHours.getOrElse("")}", "${yearBuilt.getOrElse("")}", "${contact.getOrElse("")}""""
}

object Main extends App {

  val Models = List("RV4", "RV8")
  val GInfoUrl = "http://publicapps.caa.co.uk/modalapplication.aspx?catid=1&pagetype=65&appid=1&mode=detailnosummary&fullregmark="
  val browser = JsoupBrowser()
  val listingPage = browser.get("http://www.rvuk.co.uk/viewpage.php?page_id=13")
  val Letter = """Dear RV owner,

I'm writing to you as the registered owner of <reg> on behalf of our RV Group 'North West RV Flyers'. We are a group of 7 flying G-DVMI, an RV-7 built in 2013, out of City Airport, Manchester (Barton in old money). We are currently considering expanding our member numbers and purchasing another aircraft – specifically an RV.

I do hope you are not offended but we are canvassing RV owners across the UK to see if they are thinking about or might consider selling their aircraft to our group. Obviously if you are not in this position then please disregard this letter.

If on the other hand you might consider an offer I wonder if you could contact us so we could discuss the proposition further? Be assured we are not any kind of commercial operation looking to sell advertising, and we have the funds ready to purchase any available aircraft. We're a genuine group of flyers – check us out on G-INFO.

You can contact us here:

Email: xxxx@xxx.com
Telephone: xxxxxxxxxxx

Once again, I do hope you don’t find this approach intrusive.

Yours,
Steven and the North West RV Flyers


"""

  val listing: List[Element] = listingPage >> elementList("#container > div > div > div > div > font > table > tbody > tr")

  val planes = listing.tail.map {
    tr =>
      val tds: List[Element] = tr >> elementList("td")
      Plane(
        tds(0) >> text("td"),
        tds(3) >> text("td > a"),
        tds(2) >> text("td"),
        tds(4) >> text("td"),
        (tds(1) >?> text("td > font")).exists(_ == "Flying"))
  }
  .filter(_.flying)
  .filter(item => Models.contains(item.model))
  .map {
    plane =>
      val ginfoPage = browser.get(GInfoUrl + plane.reg.substring(2))
      plane.copy(
        totalHours = ginfoPage >?> text("#currentModule_currentModule_TotalHours"),
        yearBuilt = ginfoPage >?> text("#currentModule_currentModule_YearBuilt"),
        contact = ginfoPage >?> text("#currentModule_currentModule_RegisteredOwners"))
  }
  .filter(_.yearBuilt.get.trim.toInt > 2000)
  .filter(p => util.Try(p.totalHours.get.trim.takeWhile(_ != ' ').toInt).getOrElse(0) < 1000)

  planes foreach {
    p =>
      println(Letter.replace("<reg>", p.reg))
  }
}
