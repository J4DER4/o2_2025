/*
* 
* This file is part of the CS-A1120 Programming 2 course materials at
* Aalto University in Spring 2025, and is for your personal use on that
* course only.
* Distribution of any parts of this file in any form, including posting or
* sharing on public or shared forums or repositories is *prohibited* and
* constitutes a violation of the code of conduct of the course.
* The programming exercises of CS-A1120 are individual and confidential
* assignments---this means that as a student taking the course you are
* allowed to individually and confidentially work with the material,
* to discuss and review the material with course staff, and submit the
* material for grading on course infrastructure.
* All other use - including, having other persons or programs
* (e.g. AI/LLM tools) working on or solving the exercises for you is
* forbidden, and constitutes a violation of the code of conduct of this
* course.
* 
*/


package qoi


/** 
  * If you want to convert a Byte to an Int, be carefull, since Bytes
  * in scala are SIGNED and in range [-128, 127], but we want to interpret
  * them as unsigned, so having the range [0, 255]. This function is a helper
  * to convert a Byte to an Int safely, such that we get a value in the
  * desired range.
  * 
  * The conversion to the other direction is safe directly with .toByte
  *  
  * @param b A regular Byte which we want to read as UNSIGNED
  * @return An Int with value in [0, 255]
  */
def uByteToInt(b: Byte): Int = b.toInt & 0xFF

/**
  * The hash function used in the qoi specification, needed whenever
  * we must find the index in the Array of previously seen pixels.
  * 
  * For the purposes of this exercise you don't need to know very much
  * about hash functions. In short, they are functions that take inputs
  * from a very large input space (in this case all the possible RGBA
  * pixels) and map them into outputs in some smaller output space (in
  * this case integers in [0, 63])
  * 
  * @param r  Red color component, will zero all other bits except 8 lowest
  * @param g  Green color component, will zero all other bits except 8 lowest
  * @param b  Blue color component, will zero all other bits except 8 lowest
  * @param a  Alpha component, will zero all other bits except 8 lowest
  * @return Int in range [0, 63]
  */
def qoiIndexHash(r: Int, g: Int, b: Int, a: Int): Int =
  ((r & 0xFF) * 3 + (g & 0xFF) * 5 + (b & 0xFF) * 7 + (a & 0xFF) * 11) % 64
    

/**
  * Decodes the qoi-encoded input bytes and returns the image
  * pixels packed to Ints in RGBA format. Notice that we
  * don't have to care about whether the original image had 3
  * or 4 channels (so whether it had a dedicated alpha channel).
  * We can just set the alpha to 255 unless the encoding specifically
  * states otherwise. Also notice that this function does not have to
  * worry about the file header mentioned in the specification, `bytes`
  * is just the encoded pixel data ++ end padding.
  * 
  * If the input consist of only the end padding, the length of which
  * is 8 Bytes, the output should be an empty IndexedSeq.
  * 
  * @param bytes an IndexedSeq of qoi encoded bytes
  * @return An IndexedSeq of pixel color values packed to Ints in RGBA format
  */
def qoiDecode(bytes: IndexedSeq[Byte]): IndexedSeq[Int] =

  // Here are some data structures needed for the decoding

  // The QOI specification refers to an array of previously seen pixels, use this for it
  val prevSeenPix = Array.fill(64)(Pixel(0, 0, 0, 255))
  // Put every decoded pixel into this Buffer, we will return it at the end of the function
  val result = scala.collection.mutable.ArrayBuffer[Int]()
  //8 bit tags
  val QUI_OP_RGB = 0xFE //11111110
  val QUI_OP_RGBA = 0xFF //11111111
  val END_PAD = 0x00 //00000000
  val END_TRUE = 0x01 //00000001
  // 2bit tags
  val QUI_OP_INDEX = 0 //00
  val QUI_OP_DIFF = 1 //01
  val QUI_OP_LUMA = 2 //10
  val QUI_OP_RUN = 3 //11

  val LOW6 = 0x3F //00111111

  def wrapHandeler(in: Int): Int = //wraps around values crossing 255 or negative
    //returns a value between [0, 255]
    if in < 0 then
      in + 256
    else if in > 255 then
      in - 256
    else
      in
  end wrapHandeler

  def process_QUI_OP_RGB(in : IndexedSeq[Byte]): Int =
    //process RGB input is 3 bytes
    val r = uByteToInt(in(0))
    val g = uByteToInt(in(1))
    val b = uByteToInt(in(2))
    val a = 255
    val pix = Pixel(r, g, b, a)
    result += pix.v
    val index = qoiIndexHash(r, g, b, a)
    prevSeenPix(index) = pix
    index

  def process_QUI_OP_RGBA(in : IndexedSeq[Byte]): Int =
    //process RGBA input is 4 bytes
    val r = uByteToInt(in(0))
    val g = uByteToInt(in(1))
    val b = uByteToInt(in(2))
    val a = uByteToInt(in(3))

    val pix = Pixel(r, g, b, a)
    val npixIndex = qoiIndexHash(r, g, b, a)
    prevSeenPix(npixIndex) = pix
    result += pix.v
    npixIndex

  def process_QUI_OP_INDEX(index : Int): Int =
    //process INDEX input is 6 bytes
    val pix = prevSeenPix(index)
    result += pix.v
    index

  def process_QUI_OP_DIFF(in : Int, prevPixIndex: Int): Int =
    //process DIFF input is 6 bytes, diff can vary from -2 to 1
    def mapDiff(in: Int): Int = 
      in match
      case 0 => -2 // b00
      case 1 => -1 // b01
      case 2 => 0  // b10
      case 3 => 1  // b11
      case _ => throw new IllegalArgumentException("Invalid diff value")
    end mapDiff

    val BIT2 = 0x03 //00000011
    val dr = mapDiff((in >> 4) & BIT2)
    val dg = mapDiff((in >> 2) & BIT2)
    val db = mapDiff((in >> 0) & BIT2)

    val pix = prevSeenPix(prevPixIndex) //previous pixel
    val (r, g, b, a) = pix.rgba
    
    val r2 = wrapHandeler(r + dr)
    val g2 = wrapHandeler(g + dg)
    val b2 = wrapHandeler(b + db)

    val npix = Pixel(r2, g2, b2, a)
    val npixIndex = qoiIndexHash(r2, g2, b2, a)
    prevSeenPix(npixIndex) = npix
    result += npix.v
    npixIndex

  def process_QUI_OP_LUMA(inG : Int, inRB: Int, prevPixIndex: Int): Int =
    //inG is 6 bits, inRB is 8 bits
    //inRB is split into 2 parts, 4 bits for dr and 4 bits for db
    //dg [-32, 31] so 0 = -32
    //dr&db [-8, 7] so 0 = -8
    val pix = prevSeenPix(prevPixIndex)
    val dg = (inG & 0x3F) - 32
    val dr = ((inRB >> 4) & 0x0f) - 8
    val db = ((inRB >> 0) & 0x0f) - 8
    val (r, g, b, a) = pix.rgba

    val r2 = wrapHandeler(r + dr+dg)
    val g2 = wrapHandeler(g + dg)
    val b2 = wrapHandeler(b + db+dg)

    val npix = Pixel(r2, g2, b2, a)
    val npixIndex = qoiIndexHash(r2, g2, b2, a)

    prevSeenPix(npixIndex) = npix //update the previous pixel
    result += npix.v
    npixIndex

  def process_QUI_OP_RUN(in: Int, prevPixIndex: Int): Int =
    //process RUN input is 6 bytes
    val runLength = in + 1
    val pix = prevSeenPix(prevPixIndex)
    for _ <- 0 until runLength do
      result += pix.v
    prevPixIndex
  
  //! MAIN LOOP ----------

  var i = 0
  var prevPixIndex = 1 //initialize to 1, since 0 is reserved for the first pixel
  var END = false
  
  prevSeenPix(0) = Pixel(0, 0, 0, 0) //initialize the first pixel for index 0

  while i < bytes.length && !END do //run loop
   //check for 8bit tag first
    val tag8 = uByteToInt(bytes(i))

    
    if tag8 == END_TRUE && i == bytes.length -1 then
      END = true // break and check not to mix with OP_INDEX 0x01
    else if tag8 == END_PAD && (bytes(i+1) == END_PAD || bytes(i+1) == END_TRUE) then
      //end padding, just skip and make sure not to mix with OP_INDEX 0x00
      i += 1
    else if tag8 == QUI_OP_RGB then
      //payload is 3 bytes after the tag
      val payload = bytes.slice(i + 1, i + 4)
      prevPixIndex = process_QUI_OP_RGB(payload)
      i += 4
    else if tag8 == QUI_OP_RGBA then
      //payload is 4 bytes after the tag
      val payload = bytes.slice(i + 1, i + 5)
      prevPixIndex = process_QUI_OP_RGBA(payload)
      i += 5
    else//check for 2bit tag

      val tag2 = uByteToInt(bytes(i)) >> 6 //2 highest bits

      if tag2 == QUI_OP_INDEX then
        //payload is is in the 6 bits immediately after the tag
        val payload = tag8 & LOW6 // 6 lowest bits
        prevPixIndex = process_QUI_OP_INDEX(payload)
        i += 1
      else if tag2 == QUI_OP_DIFF then
        //payload is 6 bits immediately after the tag
        val payload = tag8 & LOW6// 6 lowest bits
        prevPixIndex = process_QUI_OP_DIFF(payload, prevPixIndex)
        i += 1
      else if tag2 == QUI_OP_LUMA then
        //payload is 6 bits immediately after the tag + 1 byte after
        val payloadG = tag8 & LOW6 // 6 lowest bits
        val payloadRB = uByteToInt(bytes(i + 1)) // 8 bits
        prevPixIndex = process_QUI_OP_LUMA(payloadG, payloadRB, prevPixIndex)
        i += 2
      else if tag2 == QUI_OP_RUN then
        //payload is 6 bits immediately after the tag
        val payload = tag8 & LOW6 // 6 lowest bits
        prevPixIndex = process_QUI_OP_RUN(payload, prevPixIndex)
        i += 1
      else
        throw new IllegalArgumentException(s"Invalid tag: $tag8")

  end while
  //return the result
  result.toIndexedSeq

end qoiDecode

    