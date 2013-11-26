import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet._
import javax.servlet.http._
import scala.io.Source
import scala.xml._
import scala.collection.mutable.Map
import org.apache.commons.io.FileUtils
import java.io.File

object RespondTo extends App {
  // default config parameters
  var configFile = "../etc/respondto-config.xml"
  var logDir = "../log/"
  var port = 16200
  var delay = 0    // in milliseconds
  var code = 200
  
  nextOption(args.toList)
  println("Config file location: " + configFile)
  println("Log directory: " + logDir)
  println("Listening on port " + port)
  println("Responding with a delay of " + delay + " ms")
  
  val server = new Server(port)
  val context = new Context(server, "/", Context.SESSIONS)
  context.addServlet(classOf[RespondServlet], "/*")
  server.start()
  server.join() 
  
  def nextOption(list: List[String]) : List[String] = {
      list match {
        case Nil =>
          list
        case "-h" :: tail =>
          println("dummyads [-p port] [-d delay] [-l log-dir] [-c config-file]")
          exit(1)
        case "-p" :: value :: tail =>
          port = value.toInt
          nextOption(tail)
        case "-d" :: value :: tail =>
          delay = value.toInt
          nextOption(tail)
        case "-c" :: value :: tail =>
          configFile = value
          nextOption(tail)
        case "-l" :: value :: tail =>
          logDir = value
          nextOption(tail)
        case option :: tail => 
          println("Unknown option " + option) 
          exit(1) 
      }
  }
} 


//=============================================================================================
// RespondTo servlet
//=============================================================================================  
case class RespondServlet extends HttpServlet {
  val rs = new DecisionMaker
  
  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) {
    val body = Source.fromInputStream(req.getInputStream()).getLines().mkString("\n")
    
    // delay built into the service
    Thread.sleep(RespondTo.delay)
    
    logRequest(req, body)
    
    // create appropriate response
    val response = getResponse(body)
    response match {
      case HttpError(c) =>
        resp.setStatus(c)
      case StaticReply(b) =>
        logResponse(b)
        resp.getWriter().write(b)
      case Substitutions(subs) =>
        val vmap = ads.createVmapResponse(subs)
        logResponse(vmap)
        resp.getWriter().write(vmap)
      case _ =>
        resp.setStatus(500)  // we couldn't possibly get here, so definitely server error!
    }
  }
  
  /**
   * Print the headers.
   */
  def printHeaders(req: HttpServletRequest) {
    val headerNames = req.getHeaderNames()
    while (headerNames.hasMoreElements) {
      val headerName = headerNames.nextElement.asInstanceOf[String]
      println("[" + headerName + ":" + req.getHeader(headerName) + "]")
    }
      
  }
  
  /**
   * Get the configured response for the requested asset id, or the default if it is not
   * a known asset id.
   */
  def getResponse(reqBody: String): Response = {
     // reload config for every response, so we could be 'dynamic'
     val config = new Configuration(RespondTo.configFile)
     
     val placementRequest = XML.loadString(reqBody)
     val reqAssetId = (placementRequest \\ "AssetRef" \ "@assetID").text
     if (config.knownAssets.contains(reqAssetId)) {
         config.knownAssets(reqAssetId)
     }
     else {
       val reqPlacementOpps = (placementRequest \ "PlacementOpportunity").map(n => (n \ "@id").text)
       val substitutions = reqPlacementOpps.map(slotId => {
          Substitution(slotId, config.contentHandle, config.reportingUrl, config.duration, "")
       })
       Substitutions(substitutions)
     }
  }
  
  /**
   * Log servlet requests.
   */
  def logRequest(req: HttpServletRequest, body: String) {
    println("Received request:")
    printHeaders(req)
    println(body)
    FileUtils.writeStringToFile(new File(RespondTo.logDir + "/request.xml"), body, "UTF-8")
  }
  
  def logResponse(body: String) {
    println("Responding with:")
    println(body)
    FileUtils.writeStringToFile(new File(RespondTo.logDir + "/response.xml"), body, "UTF-8")
  }
}

/**
 * Read the configuration file. It assumes the config file conforms to the schema respondto-config.xsd.
 */
case class Configuration(configFile: String) {
  val configXML = XML.loadFile(configFile)
  val reportingUrl = (configXML \ "defaults" \ "reportingUrl" head).text
  val contentHandle = (configXML \ "defaults" \ "content" head).text
  val duration = (configXML \ "defaults" \ "duration" head).text
 
  val knownAssets = Map[String, Response]()
  (configXML \\ "asset").foreach(asset => {
      val assetId = (asset \ "@id").text
      val response: Response = (asset \ "_" head) match {
        case c @ <httpCode>{_*}</httpCode> =>
          HttpError(c.text.toInt)
        case b @ <body>{_*}</body> =>
          StaticReply(b.text)
        case _ =>
          val substitutions: Seq[Substitution] = (asset \\ "substitution").map(sub => {
            val slotId = (sub \ "@slotId").text
            val reportingUrl = (sub \ "reportingUrl" head).text
            val content = (sub \ "content" head).text
            val duration = (sub \ "duration" head).text
            val expiryDate = (sub \ "expiryDate").map(_.text).mkString
            Substitution(slotId, content, reportingUrl, duration, expiryDate)
          })
          Substitutions(substitutions)
      }
      knownAssets += (assetId -> response)
  })
}

//=============================================================================================
// VMAP response
//=============================================================================================
abstract class Response
case class HttpError(code: Int) extends Response
case class StaticReply(body: String) extends Response
case class Substitutions(substitutions: Seq[Substitution]) extends Response
case class Substitution(val slotId: String, val contentHandle: String, val reportingUrl: String, 
                        val duration: String, val expiryDate: String)   

/**
 * The ADS.
 */
case class DecisionMaker {  
    def createVmapResponse(substitutions: Seq[Substitution]):String = {
        val buffer = new StringBuilder("""<?xml version="1.0"?><VMAP xmlns="http://www.iab.net/videosuite/vmap" version="1.0" 
      xmlns:VAST="http://www.iab.net/videosuite/vast" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">""")
        
        substitutions.foreach(sub => {
          val expiryDateElement = if (sub.expiryDate.isEmpty()) "" 
            else s"""<VAST:Extensions><VAST:Extension></VAST:Extension></VAST:Extensions>"""
          
          buffer.append(s"""<AdBreak timeOffset="#${sub.slotId}" breakType="linear">""")
          
          if (!sub.contentHandle.isEmpty()) {
              buffer.append(s"""<AdSource allowMultipleAds="true" followRedirects="false" id="1"><VASTData>
                <VAST:VAST version="3.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <VAST:Ad sequence="1">
                        <VAST:InLine>
                            <VAST:AdSystem version=""></VAST:AdSystem>
                            <VAST:AdTitle></VAST:AdTitle>
                            <VAST:Impression>${sub.reportingUrl}</VAST:Impression>
                            <VAST:Creatives>
                                <VAST:Creative>
                                    <VAST:Linear>
                                        <VAST:Duration>${sub.duration}</VAST:Duration>
                                        <VAST:MediaFiles>
                                            <VAST:MediaFile delivery="progressive" type="x-application/contentHandle" width="0" height="0">${sub.contentHandle}</VAST:MediaFile>
                                        </VAST:MediaFiles>
                                    </VAST:Linear>
                                </VAST:Creative>
                            </VAST:Creatives>
                            $expiryDateElement
                        </VAST:InLine>
                    </VAST:Ad>
                </VAST:VAST>
            </VASTData>
            </AdSource>""")
          }
                            
          buffer.append("</AdBreak>")
        })
        
        buffer.append("</VMAP>")
        buffer.toString
    }
}


