name := "scholarscraper"

version := "1.0"

libraryDependencies ~= { seq =>
  val vers = "0.11.1"
  seq ++ Seq(
    "net.databinder.dispatch" %% "dispatch-core" % vers,
    //"net.databinder.dispatch" %% "dispatch-oauth" % vers,
    //"net.databinder.dispatch" %% "dispatch-nio" % vers,
    /* Twine doesn't need the below dependencies, but it simplifies
     * the Dispatch tutorials to keep it here for now. */
    //"net.databinder.dispatch" %% "dispatch-http" % vers,
    //"net.databinder.dispatch" %% "dispatch-tagsoup" % vers,
    "net.databinder.dispatch" %% "dispatch-jsoup" % vers
  )
}

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.0.0"
