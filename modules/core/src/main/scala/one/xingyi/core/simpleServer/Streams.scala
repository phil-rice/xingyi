package one.xingyi.core.simpleServer

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}

import scala.annotation.tailrec

object Streams {
  def readAll(stream: InputStream): String = {
    val in = new BufferedReader(new InputStreamReader(stream))
    try {
      val result = new StringBuilder
      @tailrec
      def recurse(): String = {
        val inputLine = in.readLine()
        if (inputLine == null)
          result.toString
        else {
          result.append(inputLine)
          recurse()
        }
      }
      recurse()
    } finally in.close()
  }


  def sendAll(stream: OutputStream, bytes: Array[Byte]): Unit = {
    try
      stream.write(bytes)
    finally stream.close()
  }
}
