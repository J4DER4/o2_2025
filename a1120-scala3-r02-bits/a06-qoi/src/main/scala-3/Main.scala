package qoi

object Main:
  def main(args: Array[String]): Unit =
    val encodedBytes: IndexedSeq[Byte] = IndexedSeq(
      0xFE.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, // QUI_OP_RGB with black color
      0xFE.toByte, 0xFF.toByte, 0x00.toByte, 0x00.toByte, // QUI_OP_RGB with red color
      0xFE.toByte, 0x00.toByte, 0xFF.toByte, 0x00.toByte, // QUI_OP_RGB with green color
      0xFE.toByte, 0x00.toByte, 0x00.toByte, 0xFF.toByte, // QUI_OP_RGB with blue color
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x01.toByte // End padding
    )
    val decodedPixels = qoiDecode(encodedBytes)
    println(decodedPixels)

