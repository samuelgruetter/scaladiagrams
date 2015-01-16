package net.invalidkeyword.scaladiagrams

import java.io.File
import org.rogach.scallop._
import java.io.OutputStream
import java.io.OutputStreamWriter

object DiagramCreator {

  def main(args: Array[String]) : Unit = {
    object Config extends ScallopConf(args) {
      val extension = opt[String]("extension", default=Some(".scala"))
      val source = opt[String]("source", default=Some("."), descr = "location of source files")
      val root = opt[String]("root", required=true, descr = "only output descendants of this particular class")
    }
    
    val files = new InputFinder().files(Config.source(),Config.extension())
    val allNodes = getNodesFromFiles(files)
    
    val map: Map[String, TYPE] = allNodes.map(n => (n.name, n)).toMap
    def name2Type(name: String): List[TYPE] = map.get(name) match {
      case Some(t) =>
       List(t)
      case None =>
        Console.err.println("Warning: '" + name + "' not found")
        List()
    }
    // TYPE.children should be called TYPE.parents!
    def parentsOf(t: TYPE): Iterable[TYPE] = t.children.flatMap(rel => name2Type(rel.name))
    
    import scala.collection.mutable
    val chosen = mutable.HashSet[TYPE](map(Config.root()))
    
    var sizeBefore = -1
    while (chosen.size > sizeBefore) {
      sizeBefore = chosen.size
      for (n <- allNodes if parentsOf(n).exists(p => chosen contains p)) chosen += n
    }
    
    println(textBefore)
    for (n <- chosen; p <- parentsOf(n) if chosen contains p) {
      println("  \"" + n.name + "\" -> \"" + p.name + "\"")
    }
    println(textAfter)
  }
  
  val textBefore = """digraph diagram {
  rankdir = "LR"
  ranksep = "1.5"
  node [fontname = "Helvetica", shape = "box"]"""
  
  val textAfter = "}"
  
  def fileToString(file : File) = {scala.io.Source.fromFile(file).mkString}
  
  def parseFile(file : File) = {
    val result = ScalaSourceParser.run(fileToString(file))
    ScalaSourceParser.filter(result.get)
  }
  
  def getNodesFromFiles(files: Array[File]) = {
    files.map(parseFile(_)).flatten.toList
  }
  
  def parentNodes(ns : NodeSelector, name : String) : Iterable[TYPE] = {
    val root = ns.findNode(name)
    if(root.isDefined) ns.selectChildNodes(root.get)
    else List[TYPE]()
  }
}