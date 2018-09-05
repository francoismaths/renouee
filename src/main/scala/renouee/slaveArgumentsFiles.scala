package renouee

import java.io.FileReader
import scala.io.Source
import java.io.BufferedReader
import java.io.FileReader
import better.files._


object slaveArgumentsFiles {


  /*
  To use with RunCalibration
   */

  // to write final plants (position)
  def slaveWriteFinalPop(f : better.files.File, l : Seq[Plant], i:Int) = {
    f.appendLine().append(List(l(i).x,l(i).y,l(i).biomass).mkString(","))
  }

  def writeFinalPop(f : better.files.File, l : Seq[Plant] ) = {
    f.overwrite("xPos,yPos,biom")
    for ( i <- 0 to  l.length -1) slaveWriteFinalPop(f, l ,i)
  }



  // Here file is a directory (dossier) in which we put file (fichier) named : name_file_i
  def createFilesPositionsABC( res : Seq[Seq[Plant]] ,file : java.io.File, name_file : String) ={
    for ( i <- 0 to res.length -1 ) {
      val temp = res(i)
      //val f =  new java.io.File(file) /   s"$name_file$i.txt"
       val f = file.toScala /   s"$name_file$i.txt"    //name_file2
      writeFinalPop(f, temp)
    }
  }



/*
Unuseful, we can find the pop size thanks to the positions (file) of a pop
  def createFileFinalPopSizeABC( res : Seq[Int] ,file : java.io.File) = {
    val f = file.toScala / "finalPopSize.txt"
    f.overwrite(res(0).toString() + ",")
    for (i <- 1 to res.length - 2) {
      val temp = res(i)
      f.append(temp.toString() + ",")
    }
    val temp = res(res.length-1)
    f.append(temp.toString() )
  }
  */



  def fileToSeq(name_file: String) : List[Double] = {

    val r = new BufferedReader(new FileReader(name_file+".txt"))
    val ligne2 : String = r.readLine
    val temp = ligne2.split(" ").toList.map(_.toDouble)
    temp
  }


}