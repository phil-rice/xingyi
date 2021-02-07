package one.xingyi.core.map

import one.xingyi.core.UtilsSpec
import org.mockito.Mockito.{times, verify}

import java.io.{File, PrintWriter}

class PrintWriterMapTest extends UtilsSpec {

  behavior of "PrinterWriterMap"

  it should "map from a key to a printer writer and use that to write values to" in {
    val pw1 = mock[PrintWriter]
    val pw2 = mock[PrintWriter]
    val root = new File("root")
    val fileToPwMap = Map(
      new File(root, "1") -> pw1,
      new File(root, "2") -> pw2
    )
    implicit val fileToPrintWriter: FileToPrintWriter = file => fileToPwMap(file)
    val map = PrintWriterMap[String](s => new File(root, s))

    map.writeln("1", "value1a")
    map.writeln("1", "value1b")
    map.writeln("2", "value2")

    verify(pw1, times(1)).println("value1a")
    verify(pw1, times(1)).println("value1a")
    verify(pw2, times(1)).println("value2")
  }

}
