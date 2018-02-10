package renouee

import better.files._

package object createfileforR {

  ///////////////////////////////////////////////////////////
  //////////    For a single simulation
  //////////    slaves final pop + History
  //////////////////////////////////////////////////////////


      // to write final plants (position and biomass)
      def slaveWriteFinalPop(f : better.files.File, plantEvolution: PlantEvolution, i:Int) = {
        f.appendLine().append(List(plantEvolution.plants(i).x,plantEvolution.plants(i).y,plantEvolution.plants(i).biomass).mkString(","))
      }

      def writeFinalPop(f : better.files.File, plantEvolution: PlantEvolution) = {
        f.overwrite("xPos,yPos,biomass")
        for ( i <- 0 to plantEvolution.plants.length -1) slaveWriteFinalPop(f, plantEvolution ,i)
      }




      // to write history of the simu
      // function that erase "false" event (when nothing happens)
      def clearEvents (g :PlantEvolution) : Seq[InfosEvolution] = {
        g.infosEvolution.filter( _.event != "Nobirth" ).filter( _.event != "Nodeath" )
      }

      def slaveWriteHistory(h : better.files.File , infosEvolution : Seq[InfosEvolution] , i:Int) = {
        h.appendLine().append( List( infosEvolution(i).time , infosEvolution(i).popSize , infosEvolution(i).area , infosEvolution(i).event  ).mkString(","))
      }

      def writeHistory(h : better.files.File, infosEvolution : Seq[InfosEvolution] ) = {
        h.overwrite("time,popSize,area,event")
        for ( i <- 0 to infosEvolution.length -1) slaveWriteHistory(h, infosEvolution ,i)
      }




      // write parameters in a text document (to use in R for visualisation)
      def writeParameters (g : better.files.File)= {
        g.overwrite("parameter,value")
        g.appendLine().append("T," + parameter.T)
        g.appendLine().append("tau," + Plant.tau)
        g.appendLine().append("p," + Plant.proportionMowing)
        g.appendLine().append("deathParameterScaling," + Plant.deathParameterScaling)
        g.appendLine().append("InitialPopSize," + Plant.initialPopulationSize)
        g.appendLine().append("L," + Plant.L)
        g.appendLine().append("K," + Plant.K)
        g.appendLine().append("b1," + Plant.b1)
        g.appendLine().append("d1," + Plant.d1)
        g.appendLine().append("shape," + Plant.shape)
        g.appendLine().append("scale," + Plant.scale)
        g.appendLine().append("a_0," + Plant.a_0(Plant.K))
        g.appendLine().append("bbar," + Plant.bbar)
        g.appendLine().append("mowing_parameter," + Plant.mowingParameter)

      }




  //////////////////////////////////////////////////////////
  ////////////    function that writes the results in  files
  /////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////
  ////////////    Single Experiment
  /////////////////////////////////////////////////////////
  def writeResultSingleExperiment(initialPlants : PlantEvolution, resultEvolution : PlantEvolution, file : java.io.File  )  ={
    // write initial plants

    val e = file.toScala /  "renouee_initial_pop.txt"
    createfileforR.writeFinalPop(e,initialPlants)

    // write final plants
    val f =file.toScala / "renouee_final_pop.txt"
    createfileforR.writeFinalPop(f,resultEvolution)

    //  write parameters
    val g = file.toScala /  "parameters_renouee.txt"
    createfileforR.writeParameters(g)

    // write history
    val h = file.toScala / "result_renouee_history.txt"
    createfileforR.writeHistory(h,clearEvents(resultEvolution))

  }


/*

///////////////////////////////////////////////////////////
  //////////    Repeat several times the same experience
  //////////////////////////////////////////////////////////


  def slaveMultipleWrite(string: String, resM : ResultMultipleExperience, i:Int) : String = {
    string+i+","+resM.size(i)+","+resM.totalBiomass(i)
  }


  def writeMultiple(string : String = "pop", resM : ResultMultipleExperience, initialPopulation : PlantEvolution,  pathFile: String = "/Users/François/Desktop/scala/R/_multiple exp") = {
    //= "/Users/François/Desktop/scala/R"
    val m = File(pathFile +"/result_renouee_multiple.txt")
    m.overwrite("numero exp,popSize,totalBiomass ")
    for ( i <- 0 to parameter.n -1) m.appendLine().append(slaveMultipleWrite(string,resM,i))

    // write initial plants
    val e = File(pathFile + "/renouee_initial_pop.txt")
    createfileforR.writeFinalPop(e,initialPopulation)

    //  write parameters
    val g = File(pathFile + "/parameters_renouee.txt")
    createfileforR.writeParameters(g)
    g.appendLine().append("numberOfSimu," + parameter.n)

  }

  ///////////////////////////////////////////////////////////
  //////////    For multiple simulation (repetition of different experiences (tau varie)
  //////////////////////////////////////////////////////////

  def slaveMultipleWriteTau(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience] , i:Int) : String = {
    "Tau"+i+"="+tau(i)+","+resMTau(i).size.mkString(",")
    //tau(i)+","+resMTau(i).size.mkString(",")
  }

    def slaveMultipleWriteTauBiomass(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience] , i:Int) : String = {
    "Tau"+i+"="+tau(i)+","+resMTau(i).totalBiomass.mkString(",")
    //tau(i)+","+resMTau(i).size.mkString(",")
  }


  def slaveMultipleWriteFinalTime(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience] , i:Int) : String = {
    "Tau"+i+"="+tau(i)+","+resMTau(i).time.mkString(",")
    //tau(i)+","+resMTau(i).size.mkString(",")
  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////  variation of tau
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////


  def tauVariePopSize(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience], numberOfSimu : Int = parameter.n , pathFile: String = "/Users/François/Desktop/scala/R/_tau varie") : better.files.File = {
    //= "/Users/François/Desktop/scala/R"
    val t = File(pathFile +"/result_renouee_tau_varie_PopSize.txt")
    t.overwrite("tau,"+(List.fill(numberOfSimu)("exp") zip  List.tabulate(numberOfSimu)(n => n+1)).map( p => p._1 + p._2 ).mkString(","))
    for ( i <- 0 to tau.length-1) t.appendLine().append(slaveMultipleWriteTau( tau , resMTau  , i ))
    t
  }

  def tauVarieBiomass(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience], numberOfSimu : Int = parameter.n , pathFile: String = "/Users/François/Desktop/scala/R/_tau varie") : better.files.File = {
    //= "/Users/François/Desktop/scala/R"
    val t = File(pathFile +"/result_renouee_tau_varie_Biomass.txt")
    t.overwrite("tau,"+(List.fill(numberOfSimu)("exp") zip  List.tabulate(numberOfSimu)(n => n+1)).map( p => p._1 + p._2 ).mkString(","))
    for ( i <- 0 to tau.length-1) t.appendLine().append(slaveMultipleWriteTauBiomass( tau , resMTau  , i ))
    t
  }

  def tauVarieFinalTime(tau : Seq[Double], resMTau : Seq[ResultMultipleExperience], numberOfSimu : Int = parameter.n , pathFile: String = "/Users/François/Desktop/scala/R/_tau varie") : better.files.File = {
    //= "/Users/François/Desktop/scala/R"
    val t = File(pathFile +"/result_renouee_tau_varie_FinalTime.txt")
    t.overwrite("tau,"+(List.fill(numberOfSimu)("exp") zip  List.tabulate(numberOfSimu)(n => n+1)).map( p => p._1 + p._2 ).mkString(","))
    for ( i <- 0 to tau.length-1) t.appendLine().append(slaveMultipleWriteFinalTime( tau , resMTau  , i ))
    t
  }



    def writeParamTau(tau : Seq[Double], pathFile : String = "/Users/François/Desktop/scala/R/_tau varie") : better.files.File ={
      val t = File(pathFile +"/param_tau.txt")
      t.overwrite("tau")
      for ( i <- 0 to tau.length-1) t.appendLine().append(tau(i).toString)
      t
    }

  //  write parameters
  def writeOtherParametersThanTau (numberOfSimu : Int, pathFile : String = "/Users/François/Desktop/scala/R/_tau varie") : better.files.File = {
    val g = File(pathFile + "/parameters_renouee_tau_varie.txt")
    g.overwrite("parameter,value")
    g.appendLine().append("T," + parameter.T)
    g.appendLine().append("p," + Plant.proportionMowing)
    g.appendLine().append("deathParameterScaling," + Plant.deathParameterScaling)
    g.appendLine().append("InitialPopSize," + Plant.initialPopulationSize)
    g.appendLine().append("numberOfSimu," + numberOfSimu)
  }

  def writeTauVarie  (tau : Seq[Double], resMTau : Seq[ResultMultipleExperience], numberOfSimu : Int = parameter.n , pathFile: String = "/Users/François/Desktop/scala/R/_tau varie")  = {
    tauVariePopSize(tau, resMTau, numberOfSimu,pathFile)
    tauVarieBiomass(tau,resMTau, numberOfSimu, pathFile)
    tauVarieFinalTime(tau,resMTau, numberOfSimu, pathFile)
    writeParamTau(tau, pathFile)
    writeOtherParametersThanTau(numberOfSimu, pathFile)
  }







  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////  variation of initial poop size, not with repeteExperience cause we wante to change the initial pop (but some have the same size)
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def WriteInitialPopSizeVariation( res : Seq[Seq[Double]], initialPopSizemax: Double , numberOfSimu : Int , InitialPopSizeMin : Double, step :Double , pathFile: String = "/Users/François/Desktop/scala/R/_initial pop size varie") : better.files.File = {
    //= "/Users/François/Desktop/scala/R"
    val t = File(pathFile +"/result_renouee_InitialPopSizeVariation.txt")
    t.overwrite("InitialPopSize,"+(List.fill(numberOfSimu)("exp") zip  List.tabulate(numberOfSimu-1)(n => n+1)).map( p => p._1 + p._2 ).mkString(","))
    for ( i <- 0 to  math.floor( ((initialPopSizemax - InitialPopSizeMin))/step ).toInt ) t.appendLine().append( InitialPopSizeMin+step*i +"," + res(i).mkString(","))
    t
  }

  def WriteParameterPopSizeVariation(T : Double ,initialPopSizemax: Double ,numberOfSimu : Int , InitialPopSizeMin : Double, step :Double , pathFile: String = "/Users/François/Desktop/scala/R/_initial pop size varie") : better.files.File = {
    val t = File(pathFile +"/Parameter_renouee_InitialPopSizeVariation.txt")
    t.overwrite("parameter,value")
    t.appendLine().append("InitialPopSizeMin,"+ InitialPopSizeMin)
    t.appendLine().append( "InitialPopSizeMax,"+initialPopSizemax)
    t.appendLine().append( "numberOfSimu,"+numberOfSimu )
    t.appendLine().append( "T,"+T )
    t
  }

  def writePopSizeVariation ( res : Seq[Seq[Double]], initialPopSizemax: Double , numberOfSimu : Int , InitialPopSizeMin : Double, step :Double , T:Double = parameter.T,  pathFile: String = "/Users/François/Desktop/scala/R/_initial pop size varie") = {
    WriteInitialPopSizeVariation( res, initialPopSizemax, numberOfSimu , InitialPopSizeMin , step , pathFile)
    WriteParameterPopSizeVariation(T : Double ,initialPopSizemax,numberOfSimu , InitialPopSizeMin , step , pathFile)
  }


 */


}
