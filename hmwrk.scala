import com.sun.net.httpserver._
import java.awt._
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.BufferedReader
import java.io.File
import java.io.FileOutputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.net.InetSocketAddress
import java.net.URL
import java.nio.charset.Charset
import java.util.Base64
import javax.imageio.ImageIO
import scala.collection.mutable._
import sun.misc.BASE64Encoder
import sun.misc.BASE64Decoder


object hmwrk{
  def main(args: Array[String]){
    
    var server = HttpServer.create(new InetSocketAddress(8000), 0)
    server.createContext("/ejercicio1", new getDirections())
    server.createContext("/ejercicio2", new getRestaurants())
    server.createContext("/ejercicio3", new grayscaleImg())
    server.start()
    
    println("Listening to http://localhost:8000...")
    
  }
  
  class getDirections() extends HttpHandler{
    override def handle(t: HttpExchange){
      if(t.getRequestMethod == "POST"){
        
        val os: OutputStream = t.getResponseBody
        
        try{
          var output = new ByteArrayOutputStream()
          var input = t.getRequestBody()
          var counter = 0
          var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
          val test = new String(response, Charset.forName("UTF-8"))
          val splt = test.split("\"")
          
          var origin = splt(3).replace(' ','+')
          var destination = splt(7).replace(' ','+')
          
          val googlereq = "https://maps.googleapis.com/maps/api/directions/json?origin=" + origin + "&destination=" + destination + "&key=AIzaSyDun5QvAmgTrK8_mJ6P542DDuTT2v4-N3g"
          val newurl = new URL(googlereq)
          
          val buffer = new BufferedReader(new InputStreamReader(newurl.openStream()))
          
          var gmaps: String = ""
          var tmp: String = ""
          
           while(buffer.ready()){
	                tmp = buffer.readLine()
	                gmaps = gmaps + tmp
	         }
          
           var splitt = gmaps.split("\"steps\" \\: \\[|\\],               \"traffic_speed_entry\"")
	         splitt = splitt(1).split("\"start_location\" \\: |\"end_location\" \\: |,                     \"html_instructions\"|,                     \"travel_mode\"")
	         val bffer = ListBuffer.empty[String]
           
           while(counter < splitt.size){
             if(counter % 2 == 1){
               bffer += splitt(counter)
             }
             counter = counter + 1
           }
	         
           var steps = bffer.toList
           var json_res = ""
           counter = 3
           
           if(steps.length == 1){
	           json_res += "{\"ruta\":["  + steps(1) + "]}"
           }else if(steps.length == 2){
	           json_res = "{\"ruta\":["  + steps(1) + ", " + steps(0) + "]}"
           }else if(steps.length == 3){
	           json_res = "{\"ruta\":["  + steps(1) + ", " + steps(0) + ", " + steps(2) + "]}"
           }else{
	           json_res = "{\"ruta\":[" + steps(1) + ", " + steps(0) + ", " + steps(2) + ", "
	           while(counter < steps.size ){
	               if(counter % 2 == 0){
	                 json_res = json_res + steps(counter) + ", "
	               }
	                 counter = counter + 1
	               }
	         }
           
           json_res = json_res.dropRight(2)
	         json_res = json_res + "]}"
	         response = json_res.getBytes(Charset.forName("UTF-8"))
	         t.getResponseHeaders().add("content-type", "json")
	         t.sendResponseHeaders(200, response.size.toLong)
	         os.write(response)
	         os.close()
          
        }catch{
          
              case e: Exception =>
              e.printStackTrace();
              t.getResponseHeaders().add("content-type", "json");
              var mssg = "{\n\"Error\":\"No se especifico el origen y/o destino apropiadamente. Intente de nuevo.\"\n}";
              t.sendResponseHeaders(400, mssg.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
              os.write(mssg.getBytes("UTF-8"));
              os.close();
              
        }
        
      }
    }
  }
  
  class getRestaurants() extends HttpHandler{
    override def handle(t: HttpExchange){
      if(t.getRequestMethod == "POST"){
        
        val os: OutputStream = t.getResponseBody
        
        try{
          
          var output = new ByteArrayOutputStream()
          var input = t.getRequestBody()
          var counter = 0
          var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
          val test = new String(response, Charset.forName("UTF-8"))
          val splt = test.split("\"")
          
          var origin = splt(3).replace(' ','+')
          
          var googlereq = "https://maps.googleapis.com/maps/api/geocode/json?address=" + origin + "&key=AIzaSyDun5QvAmgTrK8_mJ6P542DDuTT2v4-N3g"
          var newurl = new URL(googlereq)
          
          val buffer = new BufferedReader(new InputStreamReader(newurl.openStream()))
          
          var gmaps: String = ""
          var tmp: String = ""
          
           while(buffer.ready()){
	                tmp = buffer.readLine()
	                gmaps = gmaps + tmp
	         }
          
           var splitt = gmaps.split("\"location\" \\: \\{|\\},            \"location_type\"")
	         splitt = splitt(1).split("\"lat\" \\: |\"lng\" \\: |,| ")
	         
	         var latitud = splitt(16)
	         var longitud = splitt(33)
	         googlereq = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=" + latitud + "," + longitud + "&radius=45000&type=restaurant&keyword=restaurant&key=AIzaSyDun5QvAmgTrK8_mJ6P542DDuTT2v4-N3g"
	         newurl = new URL(googlereq)
	         val bffer = new BufferedReader(new InputStreamReader(newurl.openStream()))
           
           var maps1 = ""
	            var temp1 = ""

	            while(bffer.ready()){
	                temp1 = bffer.readLine()
	                maps1 = maps1 + temp1
	            }

	            splitt = maps1.split("\"location\" \\: \\{|}\\,            \"viewport\"|\"name\" \\:")
	            val buff = scala.collection.mutable.ListBuffer.empty[String]

	            while(counter < splitt.length){
	                if(counter % 3 == 1){
	                    buff += splitt(counter)
	                }
	                if(counter % 3 == 0 && counter != 0){
	                    var tempo = splitt(counter).split("         \"") 
	                    buff += tempo(0)
	                }
	                counter = counter + 1
	            }
	            
	            var steps = buff.toList
	            var json_resp = ""
	            counter = 0
	            
	            json_resp = json_resp + "{\"restaurantes\":["
	            while(counter < steps.length/2){
	                json_resp = json_resp + "{\"nombre\":" + steps(counter * 2 + 1) + steps(counter * 2) + "}, "
	                counter = counter + 1
	            }
	            
	            json_resp = json_resp.dropRight(2)
	            json_resp = json_resp + "]}"
	            response = json_resp.getBytes(Charset.forName("UTF-8"))
	            t.getResponseHeaders().add("content-type", "json")
	            t.sendResponseHeaders(200, response.size.toLong)
	            os.write(response)
	            os.close()
           
        }catch{
          
              case e: Exception =>
              e.printStackTrace();
              t.getResponseHeaders().add("content-type", "json");
              var mssg = "{\n\"Error\":\"No se especifico el punto de origen apropiadamente. Intente de nuevo.\"\n}";
              t.sendResponseHeaders(400, mssg.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
              os.write(mssg.getBytes("UTF-8"));
              os.close();
              
        }
      }
    }
  }
  
  
class grayscaleImg() extends HttpHandler{
    override def handle(t: HttpExchange){
      if(t.getRequestMethod == "POST"){
        
        val os: OutputStream = t.getResponseBody
        
        try{
          
              var output = new ByteArrayOutputStream()
	            var input = t.getRequestBody()
	            var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
	            val test = new String(response, Charset.forName("UTF-8"))
	            var splt = test.split("\"")
	            val name = splt(3)
	            var data = splt(7)

	            val DataFile = new BASE64Decoder().decodeBuffer(data)

	            //val outputFile = new File(name + ".bmp")
	            val outputFileStream = new FileOutputStream(new File(name + ".bmp"))

	            outputFileStream.write(DataFile)

	            val imageReceived = ImageIO.read(new File(name + ".bmp"))
	            val imgWidth = imageReceived.getWidth()
	            val imgHeigth = imageReceived.getHeight()

	            var i = 0
	            var j = 0

	            for(i <- 0 to imgWidth -1){
	            	for(j <- 0 to imgHeigth -1){
	            	  
	            		var color = new Color(imageReceived.getRGB(i,j))
	            		
	            		var red = (color.getRed*0.2126).toInt
	            		var green = (color.getGreen*0.7152).toInt
	            		var blue = (color.getBlue*0.0722).toInt
	            		
	            		var gray = red + green + blue
	            		var newColor = new Color(gray, gray, gray)
	            		
	            		imageReceived.setRGB(i, j, newColor.getRGB)
	            		
	            	}
	            }

	            val imgResponse = new File(name + "_grayscaled.bmp")
	            ImageIO.write(imageReceived, "bmp", imgResponse)

	            var b64EncodedImg:String = null
	            var BOS = new ByteArrayOutputStream()
	            ImageIO.write(imageReceived, "bmp", BOS)

	            val respImgBytes = BOS.toByteArray()
	            val b64Encoder = new BASE64Encoder()

	           	b64EncodedImg = b64Encoder.encode(respImgBytes)
	           	BOS.close()

	            var json_resp = ""
	            json_resp = "{\"nombre\":\"" + name + "_grayscaled.bmp" + "\", \"data\": \"" + b64EncodedImg + "\"}"
	            response = json_resp.getBytes(Charset.forName("UTF-8"))
	            t.getResponseHeaders().add("content-type", "json")
	            t.sendResponseHeaders(200, response.size.toLong)
	            os.write(response)
	            os.close()
          
        }catch{
          
              case e: Exception =>
              e.printStackTrace();
              t.getResponseHeaders().add("content-type", "json");
              var mssg = "{\n\"Error\":\"Nombre y/o data de imagen faltante! Por favor ingrese el campo faltante, e intente de nuevo.\"\n}";
              t.sendResponseHeaders(400, mssg.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
              os.write(mssg.getBytes("UTF-8"));
              os.close();
              
        }
        
      }
    }
  }
  
}